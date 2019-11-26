(ns deps.tools.io
  (:require
   [clojure.tools.deps.alpha.util.io :as tools.deps.io]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [deps.tools :as deps.tools]))

(defn slurp-config
  ([] (slurp-config (str (System/getProperty "user.home") "/.clojure/deps.config.edn")))
  ([path] (tools.deps.io/slurp-edn path)))

(defn deps-map-path
  [config lib]
  (str (:local/root (config lib)) "/deps.edn"))

(defn slurp-deps-map
  ([config lib] (slurp-deps-map (deps-map-path config lib)))
  ([path] (tools.deps.io/slurp-edn path)))

(defn slurp-configured
  [config lib-or-path]
  (->>
   (if (deps.tools/lib? lib-or-path)
     (deps-map-path config lib-or-path)
     lib-or-path)
   (tools.deps.io/slurp-edn)
   (deps.tools/parse-configured config)))

(defn spit-edn!
  [path data]
  (with-open [w (io/writer path)]
    (binding [*print-namespace-maps* false
              *print-length*         false
              *out*                  w]
      (pprint/pprint data))))
