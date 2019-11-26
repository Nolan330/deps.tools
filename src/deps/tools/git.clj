(ns deps.tools.git
  (:require
   [clojure.spec.alpha :as s]
   [clojure.java.io :as io]
   [clj-jgit.porcelain :as git]
   [deps.tools.io :as deps.tools.io]
   [deps.tools :as deps.tools])
  (:import
   (org.eclipse.jgit.api Git)))

(s/def ::git/added set?)
(s/def ::git/changed set?)
(s/def ::git/missing set?)
(s/def ::git/modified set?)
(s/def ::git/removed set?)
(s/def ::git/untracked set?)

(s/def ::git/status
  (s/keys
   :req-un
   [::git/added
    ::git/changed
    ::git/missing
    ::git/modified
    ::git/removed
    ::git/untracked]))

(defn load-repo*
  ([f] (load-repo* f f))
  ([origin f]
   (try (git/load-repo f)
        (catch Exception e
          (if-let [p (.getParent (io/file f))]
            (load-repo* origin p)
            (throw (ex-info
                    (str "No Git repository found in lineage of " origin)
                    {:origin origin})))))))

(defn load-repo
  ([config lib]
   (->>
    (deps.tools.io/deps-map-path config lib)
    (load-repo)))
  ([repo]
   (cond
     (string? repo)       (load-repo* repo)
     (instance? Git repo) repo
     :else                (throw
                           (ex-info
                            (str "Unable to coerce as Git repo: " repo)
                            {:repo repo})))))

(defn status
  ([config lib]
   (->>
    (deps.tools.io/deps-map-path config lib)
    (status)))
  ([repo]
   (->>
    (load-repo repo)
    (git/git-status))))

(defn branch
  ([config lib]
   (->>
    (deps.tools.io/deps-map-path config lib)
    (branch)))
  ([repo]
   (->>
    (load-repo repo)
    (git/git-branch-current))))

(defn clean?
  ([config lib]
   (->>
    (deps.tools.io/deps-map-path config lib)
    (clean?)))
  ([repo]
   (every? empty? (vals (status repo)))))

(defn log
  ([config lib]
   (->>
    (deps.tools.io/deps-map-path config lib)
    (log)))
  ([repo]
   (->>
    (load-repo repo)
    (git/git-log))))

(defn sha
  ([config lib]
   (->>
    (deps.tools.io/deps-map-path config lib)
    (sha)))
  ([repo]
   (.getName (:id (first (log repo))))))

(defn stash!
  ([config lib]
   (->>
    (deps.tools.io/deps-map-path config lib)
    (stash!)))
  ([repo]
   (->>
    (load-repo repo)
    (git/git-stash-create))))

(defn stash-pop!
  ([config lib]
   (->>
    (deps.tools.io/deps-map-path config lib)
    (stash-pop!)))
  ([repo]
   (doto (load-repo repo)
     (git/git-stash-apply)
     (git/git-stash-drop :stash-id 0))))

(defn status-ignore-configured
  [config lib]
  (let [path    (deps.tools.io/deps-map-path config lib)
        current (deps.tools.io/slurp-deps-map path)
        repo    (load-repo path)
        _       (stash! repo)
        clean   (deps.tools.io/slurp-configured config path)
        _       (stash-pop! repo)
        merged  (deps.tools/merge-deps-maps current clean)
        _       (deps.tools.io/spit-edn! path merged)
        status  (status repo)
        _       (deps.tools.io/spit-edn! path current)]
    status))
