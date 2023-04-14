(ns deps.tools
  (:require
   [clojure.spec.alpha :as s]
   [clojure.set :as set]
   [deps.tools.data :as data]
   [deps.tools.git :as git]
   [deps.tools.io :as io]))

(def slurp-config io/slurp-config)

(defn info
  "Computes information about `lib`, including the status of its
  repository, ignoring `deps.tools` localization."
  ([] (info (slurp-config)))
  ([config] (into {} (map (fn [[k v]] [k (merge v (info config k))])) config))
  ([config lib]
   (prn 'info (symbol lib))
   (let [path            (io/deps-map-path config lib)
         deps-map        (io/slurp-deps-map path)
         repo            (git/load-repo path)
         stash           (git/stash! repo)
         clean-deps-map  (io/slurp-deps-map path)
         _               (when stash (git/stash-pop! repo))
         conf-deps-map   (data/configured-deps-map config clean-deps-map)
         merged-deps-map (data/merge-deps-maps deps-map conf-deps-map)
         _               (io/spit-edn! path merged-deps-map)
         status          (git/status repo)
         _               (io/spit-edn! path deps-map)
         lib-set         (data/configured-deps-lib-set config deps-map)
         branch          (git/branch repo)
         sha             (git/sha repo)]
     {::clean-deps-map clean-deps-map
      ::deps-map       merged-deps-map
      ::lib-set        lib-set
      :git/branch      branch
      :git/status      status
      :sha             sha})))

(def slurp-info (comp info slurp-config))

(def visited? data/visited?)

  ;; TODO: transducers?
(defn localize-visitor-reduce-fn
  [acc x]
  (if (visited? (acc x))
    acc
    (update acc
            x
            merge
            {::deps-map (data/localized-deps-map acc (::deps-map (acc x)))
             ::visited? true})))

(defn localize
  ([] (localize (slurp-info)))
  ([info]
   (->>
    (data/pre-reduce* data/unvisited-recur? localize-visitor-reduce-fn info (set (keys info)))
    (data/remove-visited?-kv)))
  ([info lib]
   (->>
    (data/pre-reduce* data/unvisited-recur? localize-visitor-reduce-fn info #{lib})
    (data/remove-visited?-kv))))

(defn localize!-visitor-reduce-fn
  [acc x]
  (let [acc  (localize-visitor-reduce-fn acc x)
        path (io/deps-map-path acc x)
        edn  (::deps-map (acc x))]
    (io/spit-edn! path edn)
    acc))

(defn localize!
  ([] (localize! (slurp-info)))
  ([info]
   (->>
    (data/pre-reduce* data/unvisited-recur? localize!-visitor-reduce-fn info (set (keys info)))
    (data/remove-visited?-kv)))
  ([info lib]
   (->>
    (data/pre-reduce* data/unvisited-recur? localize!-visitor-reduce-fn info #{lib})
    (data/remove-visited?-kv))))

(defn ^:private clean?-reduce-fn
  [deps-seq acc lib]
  (if (and
       (:git/clean? (acc lib))
       (->>
        (filter lib deps-seq)
        (every? (comp (hash-set (:sha (acc lib))) :sha lib))))
    acc
    (reduced false)))

(defn ^:private clean?-acc
  "Used with `data/post-reduce*` to accumulate
  `:git/clean?` for each dependency in the graph."
  [acc x]
  (and
   (git/clean? (:git/status (acc x)))
   (let [lib-set  (::lib-set (acc x))
         deps-seq (->>
                   (:deps.tools/clean-deps-map (acc x))
                   (data/configured-deps-seq acc))
         rf       (partial clean?-reduce-fn deps-seq)]
     (reduce rf acc lib-set))))

(s/def ::plan
  (s/keys
   :req
   [:git/commit-message
    :git/file-patterns]))

(def plan?
  (partial s/valid? ::plan))

(defn plan-quit
  [acc x]
  (io/prn-plan-quit acc x)
  (reduced acc))

(defn plan-visit
  [acc x plan]
  (io/prn-plan-summary acc x plan)
  (update acc x merge plan {::visited? true}))

(defn plan-visitor-reduce-fn
  [acc x]
  (cond
    (reduced? acc)     (reduced acc)
    (visited? (acc x)) acc
    (clean?-acc acc x) (update acc x merge {::visited? true :git/clean? true})
    (plan? (acc x))    (update acc x merge {::visited? true})
    :else              (let [plan (io/read-plan acc x)]
                         (if (= ::quit plan)
                           (reduced (plan-quit acc x))
                           (plan-visit acc x plan)))))

(defn plan
  ([] (plan (slurp-info)))
  ([info]
   (->>
    (data/post-reduce* data/unvisited-recur? plan-visitor-reduce-fn info (set (keys info)))
    (data/remove-visited?-kv)))
  ([info lib]
   (->>
    (data/post-reduce* data/unvisited-recur? plan-visitor-reduce-fn info #{lib})
    (data/remove-visited?-kv))))

(defn strict-update!
  [plan lib]
  (let [path           (io/deps-map-path plan lib)
        deps-map       (::deps-map (plan lib))
        deps-map       (data/gitified-deps-map plan deps-map)
        repo           (git/load-repo path)
        old-sha        (git/sha repo)
        file-patterns  (:git/file-patterns (plan lib))
        commit-message (:git/commit-message (plan lib))
        dir            (:local/root (plan lib))]
    (prn '=> 'committing lib)
    (io/spit-edn! path deps-map)
    (git/add! plan lib file-patterns)
    (git/commit! dir commit-message)
    (let [new-sha (git/sha repo)]
      (prn (symbol old-sha) '=> (symbol new-sha))
      (update plan lib merge {:sha new-sha}))))

(defn update!-visit
  [acc x]
  (->
   (strict-update! acc x)
   (update x merge {::visited? true})))

(defn update!-visitor-reduce-fn
  [acc x]
  (cond
    (reduced? acc)     (reduced acc)
    (visited? (acc x)) acc
    (clean?-acc acc x) (update acc x merge {::visited? true :git/clean? true})
    (plan? (acc x))    (update!-visit acc x)
    :else              (let [acc (plan-visitor-reduce-fn acc x)]
                         (if (reduced? acc)
                           acc
                           (update!-visit acc x)))))

(defn update!
  ([] (update! (slurp-info)))
  ([info]
   (->>
    (data/post-reduce* data/unvisited-recur? update!-visitor-reduce-fn info (set (keys info)))
    (data/remove-visited?-kv)))
  ([info lib]
   (->>
    (data/post-reduce* data/unvisited-recur? update!-visitor-reduce-fn info #{lib})
    (data/remove-visited?-kv))))
