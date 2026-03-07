(ns quandary.relations
  (:require
   [quandary.quandary :as q]
   [quandary.util :refer [next-id] :as qutil]
   [flatland.ordered.map :refer [ordered-map]]))

(defn sqrto
  "Finds the floor square root.
  WARNING: Do not try to use this relation in reverse to get the square of a value.
  Instead, just multiply the value.  E.g. y = x * x:
    [\"=\" [\"y\"] [[\"x\" \"x\"]]]"
  [target x ub]
  (let [squared (next-id "TEMP.squared")]
    {:domain    {squared [:range 0 ub]}
     :equations [["=" [squared] [[target target]]]
                 ["<=" [squared] [x]]
                 ["<=" [x [-1 squared]] [[2 target]]]]}))

(defn ceil-div-o
  "A form of integer division. A relation for `target = ceil(x / y)`.
  Also see quandary's `add-division-equality` eop, which also does division, but then
  does `floor` on the result instead."
  [target x y]
  {:equations [[">=" [[y target]] [x]]
               ["<" [[y target]] [x y]]]})

(defn mean
  "Calculate the mean average for the given terms. The result is equated to the `target` varname.
  `terms` can be single varnames, with the values to compute.
  Or tuples of [n value], meaning `n` occurrences of `value`.
  N.B. In the latter case, the count, `n`, must be first."
  [target terms total-lb total-ub]
  (when (zero? (count terms))
    (throw (ex-info "`mean` with an empty list of vars is NaN." {:target-var target})))
  (let [total (next-id "TEMP.total-for-mean")
        N0 (next-id "TEMP.N0-for-mean")
        shadow-target (next-id "TEMP.shadow-target-for-mean")
        N (next-id "TEMP.N-for-mean")
        shadowN (next-id "TEMP.shadowN-for-mean")
        N-rules (cond
                  (every? #(or (string? %) (int? %)) terms)
                  {:domain (ordered-map N [(count terms)]
                                        shadowN [(count terms)])}
                  (every? vector? terms)
                  {:domain    (ordered-map N [:range 0 total-ub]
                                           shadowN [:range 1 total-ub])
                   :equations [["=" [N] (mapv first terms)]]})]
    ;; TODO If we also provide domain-map, we could determine total-ub/total-lb by looking at all
    ;;   the min's and max's of the domains of each var.
    (if (zero? (count terms))
      ;; But if we have > 0 terms, the n-value of all "tuple' shaped terms could be zero.
      {:domain (ordered-map target [0])}

      (qutil/collate {:domain    (ordered-map total [:range total-lb total-ub]
                                              N0 [:boolean]
                                              shadow-target [:range total-lb total-ub])
                      :equations [["=" [total] terms]
                                  ["=" [N] [0] :only-if N0]
                                  [">" [N] [0] :only-if (q/! N0)]

                                  ["=" [shadowN] [1] :only-if N0]
                                  ["=" [target] [0] :only-if N0]

                                  ["=" [shadowN] [N] :only-if (q/! N0)]
                                  ;; shadow variable is needed to avoid divide by zero, since we can't use
                                  ;; :only-if with "add-division-equality"
                                  ["add-division-equality" [shadow-target] [total] [shadowN]]
                                  ["=" [target] [shadow-target] :only-if (q/! N0)]]}
                     N-rules))))

(defmethod qutil/collate-merge ::sq-diff-vars
  [_ vleft vright]
  (concat vleft vright))

(defn variance-from
  "Calculate the variance for the given terms. The result is equated to the `target`
  varname.
  Args
    target - varname to set the result into
    terms - can be single varnames, with the values to compute.
      Or tuples of [n value], meaning `n` occurrences of `value`.
      N.B. In the latter case, the count, `n`, must be first.
    mean-avg - A var or a long
    total-lb - Lower bound for temp vars (large enough to hold the total of all variables)
    total-ub - Upper bound for temp vars
  NOTE: a safe total-ub can be derived as `MAX(upper-bound of each term)^2 / 4`"
  [target mean-avg terms total-lb total-ub]
  ;; Reference: https://www.scribbr.com/statistics/variance/
  (let [sum-sq-diffs (next-id "TEMP.sum-sq-diffs")

        N (next-id "TEMP.N-for-variance")
        shadowN (next-id "TEMP.shadowN-for-variance")

        N0 (next-id "TEMP.N0-for-variance")
        shadow-target (next-id "TEMP.shadow-target-for-variance")

        shape (cond
                (every? #(or (string? %) (int? %)) terms)
                :singles
                (every? vector? terms)
                :nv)
        N-rules (case shape
                  :singles {:domain (ordered-map N [(count terms)]
                                                 shadowN [(count terms)])}
                  :nv {:domain    (ordered-map N [:range 0 total-ub]
                                               shadowN [:range 1 total-ub])
                       :equations [["=" [N] (mapv first terms)]]})
        other-rules (qutil/collate
                     (for [v (case shape
                               :singles terms
                               :nv (map second terms))
                           :let [diff-v (next-id (qutil/dot v "diff" :temp))
                                 sq-diff-v (next-id (qutil/dot v "sq-diff" :temp))]]
                        ;; NOTE: we multiply by n in the next step below.
                       {:domain        (ordered-map
                                        diff-v [:range (- total-ub) total-ub]
                                        sq-diff-v [:range total-lb (* total-ub total-ub)])
                        :equations     [["=" [diff-v] [mean-avg [-1 v]]]
                                        ["=" [sq-diff-v] [[diff-v diff-v]]]]
                        ::sq-diff-vars [sq-diff-v]}))]
    (if (zero? (count terms))

      {:domain (ordered-map target [0])}

      (qutil/collate
       N-rules
       other-rules
       {:domain    (ordered-map sum-sq-diffs [:range total-lb (* total-ub total-ub)]
                                N0 [:boolean]
                                shadow-target [:range total-lb (* total-ub total-ub)])
        :equations [["="
                     [sum-sq-diffs]
                     (case shape
                       :singles (::sq-diff-vars other-rules)
                        ;; In the :nv case, terms vector is parallel with vars vector, so we can do this:
                       :nv (mapv (fn [[n _] v]
                                   (vector n v))
                                 terms
                                 (::sq-diff-vars other-rules)))]

                    ["=" [N] [0] :only-if N0]
                    [">" [N] [0] :only-if (q/! N0)]

                    ["=" [shadowN] [1] :only-if N0]
                    ["=" [target] [0] :only-if N0]

                     ;["add-division-equality" [target] [sum-sq-diffs] [N]]
                    ["=" [shadowN] [N] :only-if (q/! N0)]
                     ;; shadow variable is needed to avoid divide by zero, since we can't use
                     ;; :only-if with "add-division-equality"
                    ["add-division-equality" [shadow-target] [sum-sq-diffs] [shadowN]]
                    ["=" [target] [shadow-target] :only-if (q/! N0)]]}

       (if (string? mean-avg)
         (mean mean-avg terms total-lb total-ub))))))

(defn variance
  "Calculate the variance for the given terms. See `variance-from` for more details.
  NOTE: a safe total-ub can be derived as `MAX(upper-bound of each term)^2 / 4`"
  [target terms total-lb total-ub]
  (if (zero? (count terms))
    {:equations [["=" [target] [0]]]}
    (let [mean-avg-varname (next-id "TEMP.mean-avg")]
      (qutil/collate
       {:domain (ordered-map mean-avg-varname [:range total-lb total-ub])}
       (variance-from target mean-avg-varname terms total-lb total-ub)))))

(defn is-exclusive-betweeno
  "Returns domain and equations that set boolean `target-var` to true iff `pt` is strictly
  inside the open interval `(start, stop)`. When `target-var` is false, `pt` is constrained
  to be at or outside the interval boundaries."
  [pt start stop target-var]
  ;     start     stop
  ;       *--------*
  ;   012334567890123456789
  ;     ^ pt
  (let [tmp1 (qutil/dot pt "tmp1" :temp)
        tmp2 (qutil/dot pt "tmp2" :temp)]
    {:domain    {tmp1 [:boolean]
                 tmp2 [:boolean]}
     :equations [["<" [start] [pt] [stop] :only-if target-var]
                 ["<=" [pt] [start] :only-if tmp1]
                 [">=" [pt] [stop] :only-if tmp2]
                 ; Enforce at least one of these constraints
                 ["add-bool-or" [target-var tmp1 tmp2]]]}))
