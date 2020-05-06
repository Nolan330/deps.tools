(ns deps.tools.data
  (:require
   [clojure.tools.deps.alpha.specs :as tools.deps.specs]
   [clojure.spec.alpha :as s]
   [clojure.set :as set]))

(def lib?
  (partial s/valid? ::tools.deps.specs/lib))

(def coord?
  (partial s/valid? ::tools.deps.specs/coord))

(def deps?
  (partial s/valid? ::tools.deps.specs/deps))

(s/def ::tools.deps.specs/lib-set
  (s/coll-of ::tools.deps.specs/lib :kind set?))

(def lib-set?
  (partial s/valid? ::tools.deps.specs/lib-set))

(def deps-map?
  (partial s/valid? ::tools.deps.specs/deps-map))

(s/def ::tools.deps.specs/config
  (s/map-of
   ::tools.deps.specs/lib
   (s/and
    :local/coord
    (fn [m] (contains? m :git/url)))))

(def config?
  (partial s/valid? ::tools.deps.specs/config))

(defn extra-deps-lib-set
  [deps-map]
  (into
   #{}
   (mapcat (comp keys :extra-deps second))
   (:aliases deps-map)))

(defn strict-deps-lib-set
  [deps-map]
  (set (keys (:deps deps-map))))

(defn deps-lib-set
  [deps-map]
  (set/union
   (strict-deps-lib-set deps-map)
   (extra-deps-lib-set deps-map)))

(defn configured-deps-lib-set
  [config deps-map]
  (set/intersection
   (deps-lib-set deps-map)
   (set (keys config))))

(defn configured-extra-deps-kvxf
  [config [k v]]
  [k (if (:extra-deps v)
       (update v :extra-deps select-keys (keys config))
       v)])

(defn configured-deps-map
  "Subset of `deps.edn` configured in `deps.config.edn`.
  Same shape as `deps.edn`."
  [config deps-map]
  (->>
   (update deps-map :deps select-keys (keys config))
   ((fn [deps-map]
      (if (:aliases deps-map)
        (let [xf (partial configured-extra-deps-kvxf config)]
          (update deps-map :aliases (partial into {} (map xf))))
        deps-map)))))

(defn configured-deps-seq
  [config deps-map]
  (let [conf-deps-map (configured-deps-map config deps-map)]
    (cons
     (:deps conf-deps-map)
     (map :extra-deps (vals (:aliases conf-deps-map))))))

(defn merge-extra-deps-kvxf
  [deps-map [k v]]
  [k (if (:extra-deps v)
       (update v :extra-deps merge (:extra-deps (k (:aliases deps-map))))
       v)])

  ;; TODO: revisit this
(defn merge-deps-maps
  [deps-map1 deps-map2]
  (->>
   (update deps-map1 :deps merge (:deps deps-map2))
   ((fn [deps-map]
      (if (:aliases deps-map2)
        (let [xf (partial merge-extra-deps-kvxf deps-map2)]
          (update deps-map :aliases (partial into {} (map xf))))
        deps-map)))))

(defn deps-map->deps
  [deps-map]
  (into
   (:deps deps-map)
   (map (fn [[_ v]] (:extra-deps v)))
   (:aliases deps-map)))

(defn local-deps-kvxf
  [[k v]]
  [k (select-keys v [:local/root])])

(defn local-deps
  [config]
  (into {} (map local-deps-kvxf) config))

(defn git-deps-kvxf
  [[k v]]
  [k (select-keys v [:git/url :sha])])

(defn git-deps
  [config]
  (into {} (map git-deps-kvxf) config))

(defn update-extra-deps-kvxf
  [deps [k v]]
  [k (if (:extra-deps v)
       (update v :extra-deps merge (select-keys deps (keys (:extra-deps v))))
       v)])

(defn update-deps-map
  [deps deps-map]
  (->>
   (select-keys deps (keys (:deps deps-map)))
   (update deps-map :deps merge)
   ((fn [deps-map]
      (if (:aliases deps-map)
        (let [xf (partial update-extra-deps-kvxf deps)]
          (update deps-map :aliases (partial into {} (map xf))))
        deps-map)))))

(defn localized-deps-map
  [config deps-map]
  (update-deps-map (local-deps config) deps-map))

(defn gitified-deps-map
  [config deps-map]
  (update-deps-map (git-deps config) deps-map))

(defn pre-reduce*
  ([recur? f init xs]
   (pre-reduce* recur? f f init xs))
  ([recur? cf f init xs]
   (reduce
    (fn [acc x]
      (if-let [recur (recur? acc x)]
        (let [recur (if (true? recur) x recur)]
          (pre-reduce* recur? cf f (cf acc x) recur))
        (f acc x)))
    init
    xs)))

(defn post-reduce*
  ([recur? f init xs]
   (post-reduce* recur? f f init xs))
  ([recur? cf f init xs]
   (reduce
    (fn [acc x]
      (if-let [recur (recur? acc x)]
        (let [recur (if (true? recur) x recur)]
          (cf (post-reduce* recur? cf f acc recur) x))
        (f acc x)))
    init
    xs)))

(s/def :deps.tools/visited? true?)

(s/def :deps.tools/visited
  (s/keys :req [:deps.tools/visited?]))

(def visited?
  (partial s/valid? :deps.tools/visited))

(defn visited-lib-set
  [config]
  (reduce
   (fn [acc [k v]]
     (if (visited? v)
       (conj acc k)
       acc))
   #{}
   config))

(defn unvisited-recur?
  [acc x]
  (not-empty
   (set/difference
    (:deps.tools/lib-set (acc x))
    (visited-lib-set acc))))

(defn remove-visited?-kvxf
  [[k v]]
  [k (dissoc v :deps.tools/visited?)])

(defn remove-visited?-kv
  [visited-kv]
  (into
   {}
   (map remove-visited?-kvxf)
   (unreduced visited-kv)))
