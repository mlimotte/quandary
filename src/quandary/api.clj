(ns quandary.api
  (:require [flatland.ordered.map]
            [quandary.quandary :as q]
            [quandary.util :as qutil]
            [quandary.impl :as impl]))

(defn solve
  "Solve a constraint satisfaction/optimization problem.
  Accepts a map with `:domain` and `:equations` keys (as returned by `qdsl` or `collate`)
  and an optional `options` map passed through to the underlying solver."
  ([d-and-e] (solve d-and-e {}))
  ([{:keys [domain equations]} options]
   (q/solve-equations domain equations options)))

(def collate qutil/collate)

(def $ impl/$)
(def $# impl/$#)

;; ordered-maps are not required, but ordering the domain can be helpful for human review and debugging
(def ordered-map flatland.ordered.map/ordered-map)

(defmacro qdsl
  "Compiles a quandary domain and equation DSL body into a `{:domain … :equations …}` map
  suitable for `solve`. Use `collate` to combine the output of multiple `qdsl` calls.

  Options (first argument, a map):
    - `:tag` (String) — attaches provenance metadata to the result; useful for debugging
      or conditionally disabling sections at runtime.
    - `:tap-entries` (boolean) — when true, taps the intermediate entry structure produced
      by macro expansion for inspection.
    - `:initial-context` (map) — a `{:domain …, :equations …}` map that is collated with
      the body before processing.

  A common pattern is to bind locals to results of `$` and `$#` (or partials of them)
  for concise variable name references in the body."
  [{:keys [tag] :as options} & body]
  (let [env? #(contains? (set (keys &env)) %)
        entries (impl/dqsl-process-body env? body)]
    `(cond-> (impl/qdsl-internal ~options [~@entries])
       ~tag (with-meta {:tag ~tag}))))

(defmacro qdsl-from-resource
  "See `qdsl`.
  - Read file at resource `path` as a sequence of EDN object
  - `args` is a sequence of values that matched the sequence of the :in value in the
     first Map in the file."
  [{:keys [tag] :as options} path args]
  (let [[file-options & body] (impl/read-all-edn-objects-from-file path)
        env? #(contains? (-> file-options :in set) %)
        entries (impl/dqsl-process-body env? body)]
    `(let [~@(interleave (:in file-options) args)]
       (cond-> (impl/qdsl-internal ~options [~@entries])
         ~tag (with-meta {:tag ~tag})))))
