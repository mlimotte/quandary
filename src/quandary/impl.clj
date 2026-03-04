(ns quandary.impl
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.tools.reader.edn :as edn]
            [clojure.walk :as walk]
            [quandary.quandary :as q]
            [quandary.util :as qutil]
            [quandary.error :as error]
            [clojure.walk :as walk])
  (:import (java.io PushbackReader)))

;; Polynomial Expansion
;; Vector-Based Implementation. In this model:
;; * A Term is a vector: [coefficient variable1 variable2 ...]
;; * A Polynomial is a vector of Terms: [[2 a] [4 b c]]
;; * Leading element in each term is always a constant (often `1`)

(defn- multiply-terms [t1 t2]
  (let [[c1 & vars1] t1
        [c2 & vars2] t2]
    ;; Multiply coefficients, sort and concat variables
    (into [(* c1 c2)] (sort (concat vars1 vars2)))))

(defn- multiply-polynomials [poly1 poly2]
  ;; Cartesian product of all terms in poly1 and poly2
  (for [t1 poly1
        t2 poly2]
    (multiply-terms t1 t2)))

(defn- add-polynomials [& polys]
  ;; In a vector model, addition is just concatenation
  (vec (apply concat polys)))

(defn- simplify-polynomial [poly]
  (->> poly
       (group-by rest)                                      ;; Group by the variables (everything after the coefficient)
       (map (fn [[vars terms]]
              (let [total-coeff (reduce + (map first terms))]
                (into [total-coeff] vars))))
       (remove #(and (> (count %) 1) (= 0 (first %))))      ;; Remove terms with 0 coefficient
       vec))

(def ARITHMETIC_OPERATORS #{'+ '- '*})

(defn flatten-to-polynomial
  "Flatten or convert a s-expression (or additions and multiplications)
  to a vector-based polynomial.
  Example Usage:
  (flatten-to-polynomial '(* (+ a b) (+ a b)))
  => [[1 \"a\" \"a\"] [2 \"a\" \"b\"] [1 \"b\" \"b\"]]
  "
  [expr]
  (simplify-polynomial
    (walk/postwalk
      (fn node-walker [node]
        (cond
          (string? node) [[1 node]]
          (contains? ARITHMETIC_OPERATORS node) node
          (symbol? node) [[1 (str node)]]
          (number? node) [[node]]                           ;; A constant is just [coeff] with no vars
          (list? node) (let [[op & args] node]
                         (case op
                           + (apply add-polynomials args)
                           * (let [res (reduce multiply-polynomials args)]
                               (simplify-polynomial res))
                           - (let [[first-poly & rest-polys] args]
                               (if (seq rest-polys)
                                 (apply add-polynomials first-poly (map #(multiply-polynomials [[-1]] %) rest-polys))
                                 (multiply-polynomials [[-1]] first-poly)))
                           node))
          :else (throw (ex-info "Unexpected node" {:node node :type (type node)}))))
      expr)))

;; /Polynomial Expansion

(defn remove-coeffcient-of-one
  [polynomial]
  (mapv (fn [[a & more :as term]]
          (if (and (= a 1) (> (count term) 1))
            (vec more)
            term))
        polynomial))

(defn parse-var-name
  [s]
  ;; Note: "#a" is not valid, so it is not matched by this regex.
  (let [[_ dollar? dollar-hash? full-name]
        (when s (re-matches #"(\$)?(\$#)?([a-zA-Z][-a-zA-z0-9_.]*)" s))]
    (when full-name
      (let [segments (string/split full-name #"\.")
            varname (last segments)
            [prefix0 & suffix] (when (> (count segments) 1) segments)]
        (cond-> {:full-name full-name
                 :varname   varname}
                dollar? (assoc :dollar? true)
                dollar-hash? (assoc :dollar? true :dollar-hash? true)
                prefix0 (assoc :prefix0 prefix0)
                suffix (assoc :suffix (string/join "." suffix)))))))

(defn mk-qvar
  ([o suffix]
   (mk-qvar o suffix false))
  ([o suffix temp?]
   (error/wrap-context-for-error-and-logging
     {:sym o :suffix suffix}
     (let [args (cond-> []
                        suffix (conj suffix)
                        temp? (conj :temp))]
       (cond
         (map? o) (qutil/dot (cons (or (::q/var-name-prefix o)
                                       (throw (ex-info "key is a Map, but no `:quandary.quandary/var-name-prefix`" {})))
                                   args))
         (string? o) (qutil/dot (cons o args))
         :else (throw (ex-info "unrecognized type for mk-var" {:type (type o)})))))))

(defn $
  "This exists for dsl/api alias purposes"
  [varname-or-map suffix]
  (mk-qvar varname-or-map suffix))

(defn $#
  "This exists for dsl/api alias purposes"
  [varname-or-map suffix]
  (mk-qvar varname-or-map suffix :temp))

(defn- qdsl-process-varname
  [k]
  (let [{:keys [dollar? dollar-hash? full-name prefix0 suffix varname]} (parse-var-name (name k))
        sym-prefix0 (when prefix0 (symbol prefix0))]
    (cond (and dollar-hash? prefix0) `(mk-qvar ~sym-prefix0 ~suffix :temp)
          (and dollar? prefix0) `(mk-qvar ~sym-prefix0 ~suffix)
          dollar-hash? `(mk-qvar ~varname nil :temp)
          dollar? `(mk-qvar ~varname nil)
          :else full-name)))

(defn- fully-qualify-dsl-special-forms
  [[sym & args]]
  ;; Identify the ns explicitly becuase *ns* evaluates to the ns where the
  ;; macro is called from.
  `(~(symbol "quandary.impl" (name sym)) ~@args))

(defn- qdsl-process-vars-in-equations
  [env? i x]
  (cond (vector? x)
        (vec (map-indexed (partial qdsl-process-vars-in-equations env?) x))
        (keyword? x) `~x
        (number? x) x
        (string? x) x

        (list? x)
        (cond (contains? #{'$ '$#} (first x))
              (fully-qualify-dsl-special-forms x)

              (contains? ARITHMETIC_OPERATORS (first x))
              (cons 'list (map-indexed (partial qdsl-process-vars-in-equations env?) x))

              :else
              `~x)

        (symbol? x)
        (cond (and (symbol? x) (zero? i)) `'~x              ;; the operation should not be evaluated
              (and (symbol? x) (resolve x)) `~x             ;; Other symbols can eval
              (and (symbol? x) (env? x)) `~x

              ;; TODO Unsure if we want to support this.
              ;;   A symbol which is not resolvable or in local env. We expect it to be a
              ;;   var name defined in the domain => so return it as a String.
              ;;   For example, we could do (qdsl {} {r [:range 0 10]} [= r 5])
              ;;     Here, we use naked symbol r, instead of requiring "r" (string)
              ;;   Other than demos, though, this seems uncommon. Would expect $-vars or resolvable vars
              ;(symbol? x) (name x)
              )
        ))

(defn min-max-domain
  [domain-spec]
  (cond (= domain-spec [:boolean])
        [0 1]
        (and (sequential? domain-spec) (= (first domain-spec) :range))
        [(second domain-spec) (nth domain-spec 2)]
        (sequential? domain-spec)
        [(reduce min domain-spec) (reduce max domain-spec)]))

(defn temp-int-var
  "Return a domain spec for a new temp var and a range domain with min/max"
  [{:keys [domain] :as context} v1 v2 txt]
  (let [[d1min d1max] (->> v1 (get domain) min-max-domain)
        [d2min d2max] (->> v2 (get domain) min-max-domain)
        ;; not just min * min, max * max; b/c some of these values could be negative
        extremes (vector (* d1min d2min)
                         (* d1min d2max)
                         (* d1max d2min)
                         (* d1max d2max))
        t-min (reduce min extremes)
        t-max (reduce max extremes)
        varname (qutil/dot v1 v2 (name (gensym (or txt ""))) :temp)]
    {varname [:range t-min t-max]}))

(defn- apply-conditional
  "Support for :only-enforce-if / :only-enforce-else.
  Which converts nested equations into onlyEnforceIf equations.
  The value immediately following :only-enforce-if is the String varname
  for the boolean to test.
  Input example, `args`:
    [:only-enforce-if \"on\"
     ['= \"a\" 3]
     :only-enforce-else
     ['= \"a\" 7] ]
  "
  [polynomial-arg-fn args]
  (let [[_ bool-varname & more] args
        [block1 [else-op & block2]] (split-with (complement keyword?) more)
        f (fn [bvar [op & eq-args :as equation]]
            (vec (concat [op]
                         (map polynomial-arg-fn eq-args)
                         [:only-if bvar])))]
    (concat (mapv (partial f bool-varname) block1)
            (when else-op
              (when (not= else-op :only-enforce-else)
                (throw (ex-info "Unrecognized op in `:only-enforce-if`, expected `:only-enforce-else`"
                                {:else-op else-op})))
              (let [bvar-complement (q/! bool-varname)]
                (mapv (partial f bvar-complement) block2))))))

(defn qdsl-internal
  [{:keys [tap-entries initial-context] :as options} entries]
  (let [polynomial-arg-fn (comp remove-coeffcient-of-one flatten-to-polynomial)]
    (when tap-entries
      (tap> {:source          `qdsl-internal
             :initial-context initial-context
             :entries         entries}))
    (-> entries
        ;; collate first, so subsequent steps have a consolidated domain
        (->> (apply qutil/collate initial-context))
        (update :equations
                (partial mapcat (fn [[op & args :as equation]]
                                  (cond (#{:only-enforce-if ":only-enforce-if"} op)
                                        (apply-conditional polynomial-arg-fn equation)
                                        :else
                                        [(cons op (mapv polynomial-arg-fn args))]))))
        )))

(defn- normalize-$-$# [x]
  (if (and (list? x) (#{'$ '$#} (first x)))
    (fully-qualify-dsl-special-forms x)
    `~x))

(defn dqsl-process-body
  [env? body]
  (loop [acc [] [item & more] body]
    (if item
      (cond (map? item)
            (recur
              (conj acc
                    {:domain (into (empty item)
                                   (map (fn [[k v]] [(normalize-$-$# k) v]))
                                   item)})
              more)

            (vector? item)
            (let [last-item (last acc)
                  eq (vec (map-indexed (partial qdsl-process-vars-in-equations env?) item))]
              (recur (if (-> last-item :equations)
                       (conj (pop acc) (update last-item :equations conj eq))
                       (conj acc {:equations [eq]}))
                     more))

            :else
            (throw (ex-info "Unexpected in qdsl" {:item item})))
      acc)))

(defn read-all-edn-objects-from-file
  [path]
  (with-open [rdr (io/reader (io/resource path))]
    (let [pushback-reader (PushbackReader. rdr)
          eof (Object.)
          all-objects (take-while #(not= % eof)
                                  (repeatedly #(edn/read {:eof eof} pushback-reader)))]
      (vec all-objects))))

;; Overview ;;
;; 1. `dqsl-process-body` is used by the macro s.q.api/qdsl. We intentionally keep its job as small as
;;    possible: Rework the input form to make it displayable as a pure data structure. Mainly this
;;    means the macro accepts bare symbols and either generates strings from them or leaves them
;;    alone to evaluated if they are bound.
;; 2. Steps after this are pure functions, no macros.
;; 3. `qdsl-internal` coordinates a few layers of transformation. The idea is to go from the
;;    "human-friendly" input to computer friendly data structure for ultimate processing.
;;    While the human version allows for a variety of syntactic sugar, the final version is
;;    strict and consistent.
;;    N.B. However, every stage, including the fnal one outputs simple data structures that can
;;    be evaluated, displayed, persisted, etc.
