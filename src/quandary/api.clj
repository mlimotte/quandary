(ns quandary.api
  (:require [flatland.ordered.map]
            [quandary.quandary :as q]
            [quandary.util :as qutil]
            [quandary.impl :as impl]))

(defn solve
  ([d-and-e] (solve d-and-e {}))
  ([{:keys [domain equations]} options]
   (q/solve-equations domain equations options)))

(def collate qutil/collate)

(def $ impl/$)
(def $# impl/$#)

;; ordered-maps are not required, but ordering the domain can be helpful for human review and debugging
(def ordered-map flatland.ordered.map/ordered-map)

(defmacro qdsl
  "Top level macro for compiling quandary domain and expression DSL into a computer friendly form
  for `solve`.  Use `s.q.a/collate` to combine the output of multiple `qdsl` calls.
  options:
    - :tag (String) - Maintain some provenance about the source or topic of some batch of
      domain and expressions. This can be helpful for debugging or disabling sections at run time.
    - :tap-entries (boolean) - Set true to tap> the entries created by impl/dqsl-process-body.
      This is the \"initial\" data structure  after Macro processing. It will be further refined through
      multiple functions.
    - initial-context (Map) - Optional Map of {:domain, :equations} that will be collated with your qdsl body.

  A common usage pattern is to wrap the call to this macro in let, and bind locals to results
  of qapi/$ and qapi/$#, or partials of the same for more concise reference in the body."
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
