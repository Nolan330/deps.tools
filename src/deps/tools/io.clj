(ns deps.tools.io
  (:require
   [clojure.tools.deps.alpha.util.io :as tools.deps.io]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as str]
   [deps.tools.data :as deps.tools.data]))

(defn slurp-config
  ([] (slurp-config (str (System/getProperty "user.home") "/.clojure/deps.config.edn")))
  ([path] (tools.deps.io/slurp-edn path)))

(defn deps-map-path
  [config lib]
  (str (:local/root (config lib)) "/deps.edn"))

(defn slurp-deps-map
  ([config lib] (slurp-deps-map (deps-map-path config lib)))
  ([path] (tools.deps.io/slurp-edn path)))

(defn slurp-configured-deps-map
  [config lib-or-path]
  (->>
   (if (deps.tools.data/lib? lib-or-path)
     (deps-map-path config lib-or-path)
     lib-or-path)
   (tools.deps.io/slurp-edn)
   (deps.tools.data/configured-deps-map config)))

(defn spit-edn!
  [path data]
  (with-open [w (io/writer path)]
    (binding [*print-namespace-maps* false
              *print-length*         false
              *out*                  w]
      (pprint/pprint data))))

(def separator
  (symbol "---------------------------------"))

(def spacer
  (symbol "                                 "))

(defn prn-plan-header
  [acc x]
  (prn separator x)
  (prn spacer (symbol (:git/branch (acc x))))
  (prn spacer (symbol (:sha (acc x))))
  (prn)
  (pprint/pprint (:git/status (acc x)))
  (prn)
  (prn '=> 'planning x))

(defn read-file-patterns
  [acc x]
  (or
   (:git/file-patterns (acc x))
   (do
     (prn '=> 'enter 'file (symbol "pattern(s)") '(q! to quit))
     (let [file-patterns (not-empty (read-line))]
       (if file-patterns
         (let [file-patterns (str/split file-patterns #"\s+")]
           (prn file-patterns)
           file-patterns)
         (do
           (prn 'DEFAULT '(deps.edn))
           ["deps.edn"]))))))

(defn read-commit-message
  [acc x]
  (or
   (:git/commit-message (acc x))
   (do
     (prn '=> 'enter 'commit 'message '(q! to quit))
     (let [commit-message (not-empty (read-line))]
       (if commit-message
         (do
           (prn commit-message)
           commit-message)
         (let [default (str "Synchronizing " x " dependencies")]
           (prn 'DEFAULT default)
           default))))))

(defn read-plan
  [acc x]
  (prn-plan-header acc x)
  (let [file-patterns (read-file-patterns acc x)]
    (if (some #{"q!"} file-patterns)
      :deps.tools/quit
      (let [commit-message (read-commit-message acc x)]
        (if (#{"q!"} commit-message)
          :deps.tools/quit
          {:git/commit-message commit-message
           :git/file-patterns  file-patterns})))))

(defn prn-plan-quit
  [acc x]
  (prn '=> 'quitting x))

(defn prn-plan-summary
  [acc x plan]
  (prn)
  (prn 'SUMMARY)
  (pprint/pprint plan)
  (prn))
