(ns quandary.qlogic-debug
  "Utilities for diagnosing solver failures — primarily 'No solutions' and infeasible constraint issues.
  Use this namespace to inspect the raw equations and domain before/during a solve attempt."
  (:require [com.rpl.specter :as sp]
            [clojure.tools.logging :as log])
  (:import (java.util.regex Pattern)))

(defonce tapped (atom []))

(defn seq1?
  "Returns true if x is a sequential collection with exactly one element."
  [x]
  (and (sequential? x) (= (count x) 1)))

(defn equations-with-assignments
  "Recursively substitutes known variable assignments into an equation (or nested equation structure).
   Negated vars (prefixed with '!') are flipped: 1→0, 0→1. Unresolved vars are left as-is."
  [assignments x]
  (cond (sequential? x) (mapv #(equations-with-assignments assignments %) x)
        (and (string? x) (.startsWith x "!")) (let [vname (subs x 1)
                                                    d (get assignments vname)]
                                                (cond (= d 1) 0
                                                      (= d 0) 1
                                                      :else x))
        (string? x) (get assignments x x)
        :else x))

(defn eval-only-if
  "Removes equations whose :only-if conditions evaluate to false (0).
   Call after equations-with-assignments so conditions have been substituted."
  [equations]
  (remove (fn [eq]
            (let [conditions (->> eq
                                  (drop-while #(not (keyword? %)))
                                  (partition-all 2)
                                  (filter #(= (first %) :only-if))
                                  (map second))]
              ;; Are any of the only-if values false (0):
              (and (seq conditions)
                   (some (partial = 0) conditions))))
          equations))

(defn eval-with-rules
  "Creates simplified sets of rules and constraints that can then be used with 
   qdebug/eqs-from and qdebug/domains-from.
   Returns a map with stripped down `domain`, fixed `:assignments`, and simplified equations."
  [{:keys [domain equations] :as rules} additional-assignments]
  (log/info (str "Processing inputs " {:domain (count domain) :equations (count equations)}))
  (let [assignments (merge-with (fn [a b] (throw (ex-info "assignment conflict" {:a a :b b})))
                                (into {}
                                      (for [[k v] domain
                                            :when (seq1? v)]
                                        [k (first v)]))
                                additional-assignments)]
    {:domain      (into {} (doall (for [[k v] domain
                                        :when (not (.startsWith k "TEMP."))]
                                    [k v])))
     :assignments assignments
     :equations   (->> equations
                       (equations-with-assignments assignments)
                       eval-only-if
                       doall)}))

(defmulti as-string-pred
  "Coerces a filter value to a string predicate fn.
   Accepts: string (exact match), regex (re-find), set (membership), or fn (used as-is)."
  (fn [x] (cond (string? x) :string
                (instance? Pattern x) :regex
                (set? x) :set
                (ifn? x) :fn)))

(defmethod as-string-pred :string [x] (partial = x))

(defmethod as-string-pred :regex [x] (partial re-find x))

(defmethod as-string-pred :set [x] (partial contains? x))

(defmethod as-string-pred :fn [x] x)

(defn eqs-from
  "Returns equations from eval-with-rules result that contain at least one variable name
   matching f. f can be a string (exact), regex (re-find), set (membership), or fn."
  [f eval-with-rules-result]
  (let [f2 (as-string-pred f)]
    (->> eval-with-rules-result
         :equations
         (filter (fn [eq] (seq (sp/select [(sp/walker #(and (string? %) (f2 %)))] eq)))))))

(defn domains-from
  "Returns domain entries from eval-with-rules result whose variable name matches f,
   sorted by name. f can be a string (exact), regex (re-find), set (membership), or fn."
  [f eval-with-rules-result]
  (let [f2 (as-string-pred f)]
    (->> (:domain eval-with-rules-result)
         (filter (fn [[k _]] (f2 k)))
         sort)))

(comment

  ;; Tips on debugging "No solutions" and other qlogic (CP-lver) issues.

  (require '[quandary.qlogic-debug :as qdebug])

  ;; Enable taps
  (add-tap #(swap! tapped conj %))

  ;; Run a problem
  ;; Be sure to enable taps.
  ;; Enable one or more :disable-logic options below. Each one will disable some parts of the solver logic. 
  ;; Fewer equations and domain entries makes it easier to debug. Disable things
  ;;   until you get a minimal set that still evokes the problem you are trying to debug.
  (def l (run-solver))

  ;; The final map can be used to inject specific additional assignments, to see them plugged into the equations.
  (def rez (qdebug/eval-with-rules rules {}))

  ;; Filter `rez` to see relevant equations
  (qdebug/eqs-from #"wall.b.S.0.*width" rez)
  (qdebug/eqs-from #"wall.a.S.0.C.0[^.]*(width|filler)[^.]*" rez)
  ;; View domains (supports string, regex, set)
  (qdebug/domains-from #{"wall.a.S.0.C.0.filler-width" "wall.a.S.1.C.0.filler-width"} rez)

  ;; Also look at logic-solution from the tap, and use it for lookups.
  (get logic-solution "wall.b.S.0.C.0.panels-width"))
