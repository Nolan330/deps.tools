(ns deps.tools
  (:require
   [clojure.tools.deps.alpha.specs :as tools.deps.specs]
   [clojure.spec.alpha :as s]
   [clojure.set :as set]))

(def lib?
  (partial s/valid? ::tools.deps.specs/lib))

(def coord?
  (partial s/valid? ::tools.deps.specs/coord))

(s/def ::tools.deps.specs/config
  (s/map-of lib? coord?))

(def config?
  (partial s/valid? ::tools.deps.specs/config))

(s/def ::tools.deps.specs/lib-set
  (s/coll-of lib? :kind set?))

(def lib-set?
  (partial s/valid? ::tools.deps.specs/lib-set))

(def deps-map?
  (partial s/valid? ::tools.deps.specs/deps-map))

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

(defn parse-configured-extra-deps-kvxf
  [config [k v]]
  [k (if (:extra-deps v)
       (update v :extra-deps select-keys (keys config))
       v)])

(defn parse-configured
  "Subset of `deps.edn` configured in `deps.config.edn`.
  Same shape as `deps.edn`."
  [config deps-map]
  (let [xf (partial parse-configured-extra-deps-kvxf config)]
    (->
     (update deps-map :deps select-keys (keys config))
     (update :aliases (partial into {} (map xf))))))

(defn merge-extra-deps-kvxf
  [deps-map [k v]]
  [k (if (:extra-deps v)
       (update v :extra-deps merge (:extra-deps (k (:aliases deps-map))))
       v)])

;; TODO: improve this
(defn merge-deps-maps
  [deps-map1 deps-map2]
  (let [xf (partial merge-extra-deps-kvxf deps-map2)]
    (->
     (update deps-map1 :deps merge (:deps deps-map2))
     (update :aliases (partial into {} (map xf))))))

(defn local-coord-kvxf
  [[k v]]
  [k (select-keys v [:local/root])])

(defn git-coord-kvxf
  [[k v]]
  [k (select-keys v [:git/url :sha])])

(defn update-extra-deps-kvxf
  [coords [k v]]
  [k (if (:extra-deps v)
       (update v :extra-deps merge (select-keys coords (keys (:extra-deps v))))
       v)])

(defn update-deps-map
  [coords deps-map]
  (let [xf (partial update-extra-deps-kvxf coords)]
    (->
     (update deps-map :deps merge (select-keys coords (keys (:deps deps-map))))
     (update :aliases (partial into {} (map xf))))))
