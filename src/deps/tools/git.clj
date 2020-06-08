(ns deps.tools.git
  (:require
   [clojure.java.shell :as sh]
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [clojure.string :as string]
   [clj-jgit.porcelain :as clj-jgit]
   [deps.tools.data :as deps.tools.data]
   [deps.tools.io :as deps.tools.io])
  (:import
   [java.nio.file Path Paths]
   [org.eclipse.jgit.api Git]))

(defn load-repo*
  ([f] (load-repo* f f))
  ([origin f]
   (try (clj-jgit/load-repo f)
        (catch Exception e
          (if-let [p (.getParent (io/file f))]
            (load-repo* origin p)
            (throw (ex-info
                    (str "No Git repository found in lineage of " origin)
                    {:origin origin})))))))

(defn load-repo
  ([config lib]
   (load-repo (:local/root (config lib))))
  ([repo]
   (cond
     (string? repo)       (load-repo* repo)
     (instance? Git repo) repo
     :else                (throw
                           (ex-info
                            (str "Unable to coerce as Git repo: " repo)
                            {:repo repo})))))

(s/def ::str-set
  (s/coll-of string? :kind set?))

(s/def ::clj-jgit/added ::str-set)
(s/def ::clj-jgit/changed ::str-set)
(s/def ::clj-jgit/missing ::str-set)
(s/def ::clj-jgit/modified ::str-set)
(s/def ::clj-jgit/removed ::str-set)
(s/def ::clj-jgit/untracked ::str-set)

(s/def ::clj-jgit/status
  (s/keys
   :req-un
   [::clj-jgit/added
    ::clj-jgit/changed
    ::clj-jgit/missing
    ::clj-jgit/modified
    ::clj-jgit/removed
    ::clj-jgit/untracked]))

(def status?
  (partial s/valid? ::clj-jgit/status))

(defn status
  ([config lib]
   (status (:local/root (config lib))))
  ([repo]
   (clj-jgit/git-status (load-repo repo))))

(defn clean?
  ([config lib]
   (clean? (:local/root (config lib))))
  ([repo-or-status]
   (->>
    (if (status? repo-or-status)
      repo-or-status
      (status repo-or-status))
    (vals)
    (every? empty?))))

(defn branch
  ([config lib]
   (branch (:local/root (config lib))))
  ([repo]
   (clj-jgit/git-branch-current (load-repo repo))))

(defn log
  ([config lib]
   (log (:local/root (config lib))))
  ([repo]
   (clj-jgit/git-log (load-repo repo))))

(defn sha
  ([config lib]
   (sha (:local/root (config lib))))
  ([repo]
   (.getName (:id (first (log repo))))))

(defn stash!
  ([config lib]
   (stash! (:local/root (config lib))))
  ([repo]
   (clj-jgit/git-stash-create (load-repo repo))))

(defn stash-pop!
  ([config lib]
   (stash-pop! (:local/root (config lib))))
  ([repo]
   (clj-jgit/git-stash-pop (load-repo repo))))

  ;; TODO: 20191130 `clj-jgit.porcelain/git-add`
  ;; doesn't accept sets as `file-patterns`
(s/def :git/file-patterns
  (s/coll-of string? :kind vector?))

(defn local-root->repo-root
  [local-root]
  (let [git-root
        (->>
         (load-repo local-root)
         (.getRepository)
         (.getDirectory)
         (str))]
    (->>
     (string/split git-root #"/")
     (drop-last)
     (string/join "/"))))

(defn -relativize-xf
  [local-root repo-root local-file-pattern]
  (let [absolute-local-path (Paths/get
                             (str local-root "/" local-file-pattern)
                             (into-array String []))
        repo-root-path      (Paths/get
                             repo-root
                             (into-array String []))]
    (str (.relativize repo-root-path absolute-local-path))))

(defn add!
  ([config lib file-patterns]
   (let [local-root    (:local/root (config lib))
         repo-root     (local-root->repo-root local-root)
         file-patterns (mapv
                        (partial -relativize-xf local-root repo-root)
                        file-patterns)]
     (add! repo-root file-patterns)))
  ([repo file-patterns]
   (clj-jgit/git-add (load-repo repo) file-patterns)))

(s/def :git/commit-message
  (s/and string? not-empty))

(defn commit!
  ([config lib message]
   (commit! (:local/root (config lib)) message))
  ([dir message]
     ;; TODO: jgit commit signing
   (sh/sh "git" "commit" "-m" message :dir dir)))
