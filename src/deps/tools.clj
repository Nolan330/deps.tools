(ns deps.tools
  (:require
   [clojure.spec.alpha :as s]
   [clojure.set :as set]
   [deps.tools.data :as deps.tools.data]
   [deps.tools.git :as deps.tools.git]
   [deps.tools.io :as deps.tools.io]))

(def slurp-config deps.tools.io/slurp-config)

(defn info
  "Computes information about `lib`, including the status of its
  repository, ignoring `deps.tools` localization."
  ([] (info (slurp-config)))
  ([config] (into {} (map (fn [[k v]] [k (merge v (info config k))])) config))
  ([config lib]
   (prn 'info (symbol lib))
   (let [path            (deps.tools.io/deps-map-path config lib)
         deps-map        (deps.tools.io/slurp-deps-map path)
         repo            (deps.tools.git/load-repo path)
         stash           (deps.tools.git/stash! repo)
         clean-deps-map  (deps.tools.io/slurp-deps-map path)
         _               (when stash (deps.tools.git/stash-pop! repo))
         conf-deps-map   (deps.tools.data/configured-deps-map config clean-deps-map)
         merged-deps-map (deps.tools.data/merge-deps-maps deps-map conf-deps-map)
         _               (deps.tools.io/spit-edn! path merged-deps-map)
         status          (deps.tools.git/status repo)
         _               (deps.tools.io/spit-edn! path deps-map)
         lib-set         (deps.tools.data/configured-deps-lib-set config deps-map)
         branch          (deps.tools.git/branch repo)
         sha             (deps.tools.git/sha repo)]
     {::clean-deps-map clean-deps-map
      ::deps-map       merged-deps-map
      ::lib-set        lib-set
      :git/branch      branch
      :git/status      status
      :sha             sha})))

(def slurp-info (comp info slurp-config))

(def visited? deps.tools.data/visited?)

  ;; TODO: transducers?
(defn localize-visitor-reduce-fn
  [acc x]
  (if (visited? (acc x))
    acc
    (update acc
            x
            merge
            {::deps-map (deps.tools.data/localized-deps-map acc (::deps-map (acc x)))
             ::visited? true})))

(defn localize
  ([] (localize (slurp-info)))
  ([info]
   (->>
    (deps.tools.data/pre-reduce* deps.tools.data/unvisited-recur? localize-visitor-reduce-fn info (set (keys info)))
    (deps.tools.data/remove-visited?-kv)))
  ([info lib]
   (->>
    (deps.tools.data/pre-reduce* deps.tools.data/unvisited-recur? localize-visitor-reduce-fn info #{lib})
    (deps.tools.data/remove-visited?-kv))))

(defn localize!-visitor-reduce-fn
  [acc x]
  (let [acc  (localize-visitor-reduce-fn acc x)
        path (deps.tools.io/deps-map-path acc x)
        edn  (::deps-map (acc x))]
    (deps.tools.io/spit-edn! path edn)
    acc))

(defn localize!
  ([] (localize! (slurp-info)))
  ([info]
   (->>
    (deps.tools.data/pre-reduce* deps.tools.data/unvisited-recur? localize!-visitor-reduce-fn info (set (keys info)))
    (deps.tools.data/remove-visited?-kv)))
  ([info lib]
   (->>
    (deps.tools.data/pre-reduce* deps.tools.data/unvisited-recur? localize!-visitor-reduce-fn info #{lib})
    (deps.tools.data/remove-visited?-kv))))

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
  "Used with `deps.tools.data/post-reduce*` to accumulate
  `:git/clean?` for each dependency in the graph."
  [acc x]
  (and
   (deps.tools.git/clean? (:git/status (acc x)))
   (let [lib-set  (::lib-set (acc x))
         deps-seq (->>
                   (:deps.tools/clean-deps-map (acc x))
                   (deps.tools.data/configured-deps-seq acc))
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
  (deps.tools.io/prn-plan-quit acc x)
  (reduced acc))

(defn plan-visit
  [acc x plan]
  (deps.tools.io/prn-plan-summary acc x plan)
  (update acc x merge plan {::visited? true}))

(defn plan-visitor-reduce-fn
  [acc x]
  (cond
    (reduced? acc)     (reduced acc)
    (visited? (acc x)) acc
    (clean?-acc acc x) (update acc x merge {::visited? true :git/clean? true})
    (plan? (acc x))    (update acc x merge {::visited? true})
    :else              (let [plan (deps.tools.io/read-plan acc x)]
                         (if (= ::quit plan)
                           (reduced (plan-quit acc x))
                           (plan-visit acc x plan)))))

(defn plan
  ([] (plan (slurp-info)))
  ([info]
   (->>
    (deps.tools.data/post-reduce* deps.tools.data/unvisited-recur? plan-visitor-reduce-fn info (set (keys info)))
    (deps.tools.data/remove-visited?-kv)))
  ([info lib]
   (->>
    (deps.tools.data/post-reduce* deps.tools.data/unvisited-recur? plan-visitor-reduce-fn info #{lib})
    (deps.tools.data/remove-visited?-kv))))

(defn strict-update!
  [plan lib]
  (let [path           (deps.tools.io/deps-map-path plan lib)
        deps-map       (::deps-map (plan lib))
        deps-map       (deps.tools.data/gitified-deps-map plan deps-map)
        repo           (deps.tools.git/load-repo path)
        old-sha        (deps.tools.git/sha repo)
        file-patterns  (:git/file-patterns (plan lib))
        commit-message (:git/commit-message (plan lib))
        dir            (:local/root (plan lib))]
    (prn '=> 'committing lib)
    (deps.tools.io/spit-edn! path deps-map)
    (deps.tools.git/add! plan lib file-patterns)
    (deps.tools.git/commit! dir commit-message)
    (let [new-sha (deps.tools.git/sha repo)]
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
    (deps.tools.data/post-reduce* deps.tools.data/unvisited-recur? update!-visitor-reduce-fn info (set (keys info)))
    (deps.tools.data/remove-visited?-kv)))
  ([info lib]
   (->>
    (deps.tools.data/post-reduce* deps.tools.data/unvisited-recur? update!-visitor-reduce-fn info #{lib})
    (deps.tools.data/remove-visited?-kv))))
