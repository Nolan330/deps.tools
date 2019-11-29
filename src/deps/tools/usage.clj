(ns deps.tools.usage
  (:require
   [clojure.data :as clojure.data]
   [deps.tools :as deps.tools]))

  ;; modify [[deps.config.example.edn]] to point to local `tools.deps` projects
(def config (deps.tools/slurp-config "deps.config.example.edn"))
(def lib 'deps/tools)
(def info (deps.tools/info config))

(clojure.data/diff
 (::deps.tools/deps-map       (info lib))
 (::deps.tools/clean-deps-map (info lib)))

  ;; replace `lib`'s `:deps.tools/deps-map` in info with a localized version
(def localized (deps.tools/localize info lib))

(clojure.data/diff
 (::deps.tools/deps-map       (localized lib))
 (::deps.tools/clean-deps-map (localized lib)))

  ;; traverse `lib`'s dependency tree as in `deps.tools/update!`
  ;; without side effects, optionally adding commit messages
(deps.tools/plan info lib)

  ;; post-order update each dependency with the latest SHAs
(deps.tools/update! info lib)
