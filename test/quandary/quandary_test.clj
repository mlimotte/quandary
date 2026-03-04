(ns quandary.quandary-test
  (:require [clojure.test :refer :all]
            [quandary.quandary :refer :all])
  (:import [clojure.lang ExceptionInfo]
           [com.google.ortools.sat CpModel LinearArgument Literal]))

(deftest test-new-int-var-domain-from-mult
  (let [new-int-var-domain-from-mult @(ns-resolve 'quandary.quandary 'new-int-var-domain-from-mult)
        _     @load-native
        model (CpModel.)]
    ;; positive × positive: [0,3] × [0,4] → [0,12]
    (let [dm     (mk-domain-map model {"a" [:range 0 3] "b" [:range 0 4]})
          result (new-int-var-domain-from-mult model (get dm "a") (get dm "b"))]
      (is (= 0  (-> result .getDomain .min)))
      (is (= 12 (-> result .getDomain .max))))
    ;; mixed signs: [1,3] × [-2,4] → extremes {-6,-2,4,12} → [-6,12]
    (let [dm     (mk-domain-map model {"x" [:range 1 3] "y" [:range -2 4]})
          result (new-int-var-domain-from-mult model (get dm "x") (get dm "y"))]
      (is (= -6 (-> result .getDomain .min)))
      (is (= 12 (-> result .getDomain .max))))
    ;; both negative: [-5,-1] × [-3,-1] → extremes {1,3,5,15} → [1,15]
    (let [dm     (mk-domain-map model {"p" [:range -5 -1] "q" [:range -3 -1]})
          result (new-int-var-domain-from-mult model (get dm "p") (get dm "q"))]
      (is (= 1  (-> result .getDomain .min)))
      (is (= 15 (-> result .getDomain .max))))))

(deftest test-eval-in-domain
  ;; number key converts to long without consulting the domain map
  (is (= 5  (eval-in-domain {} 5)))
  (is (= 42 (eval-in-domain {} 42)))
  ;; string/keyword key looks up in domain map (name coercion applied)
  (is (= :var (eval-in-domain {"a" :var} "a")))
  (is (= :var (eval-in-domain {"a" :var} :a)))
  ;; nil key throws
  (is (thrown? ExceptionInfo (eval-in-domain {} nil)))
  ;; missing key throws
  (is (thrown? ExceptionInfo (eval-in-domain {"x" :x} "missing"))))

(deftest test-maybe-numeric-arg
  (let [maybe-numeric-arg @(ns-resolve 'quandary.quandary 'maybe-numeric-arg)]
    (is (= 5 (maybe-numeric-arg 5)))
    ;; single-element sequential with number returns long
    (is (= 3 (maybe-numeric-arg [3])))
    ;; If not numeric => nil. Which allows
    (is (nil? (maybe-numeric-arg "x")))
    (is (nil? (maybe-numeric-arg [1 2])))))

(deftest test-int-var-arg
  (is (= :my-var (int-var-arg {"a" :my-var} ["a"])))
  ;; wrong element count throws
  (is (thrown? ExceptionInfo (int-var-arg {} [])))
  (is (thrown? ExceptionInfo (int-var-arg {} ["a" "b"])))
  ;; key missing from domain throws
  (is (thrown? ExceptionInfo (int-var-arg {} ["z"]))))

(deftest test-long-arg
  (is (= 7 (long-arg [7])))
  (is (= 0 (long-arg [0])))
  ;; wrong element count throws
  (is (thrown? ExceptionInfo (long-arg [])))
  (is (thrown? ExceptionInfo (long-arg [1 2]))))

(deftest test-linear-arg-or-long
  (let [linear-arg-or-long @(ns-resolve 'quandary.quandary 'linear-arg-or-long)
        _                  @load-native
        model              (CpModel.)
        domain-map         (mk-domain-map model {"a" [:range 0 10]
                                                 "b" [:range 0 10]})]
    ;; empty polynomial → throws
    (is (thrown? ExceptionInfo (linear-arg-or-long model domain-map [])))
    ;; single-var term → LinearArgument
    (is (instance? LinearArgument (linear-arg-or-long model domain-map ["a"])))
    ;; constant term → LinearArgument
    (is (instance? LinearArgument (linear-arg-or-long model domain-map [[5]])))
    ;; coeff * var → LinearArgument
    (is (instance? LinearArgument (linear-arg-or-long model domain-map [[2 "a"]])))
    ;; var * var → LinearArgument (creates temp var)
    (is (instance? LinearArgument (linear-arg-or-long model domain-map [["a" "b"]])))
    ;; coeff * var * var → LinearArgument
    (is (instance? LinearArgument (linear-arg-or-long model domain-map [[3 "a" "b"]])))
    ;; invalid 2-element term (two numbers) → throws
    (is (thrown? ExceptionInfo (linear-arg-or-long model domain-map [[5 6]])))))

(deftest test-linear-arg
  (let [linear-arg @(ns-resolve 'quandary.quandary 'linear-arg)
        _          @load-native
        model      (CpModel.)
        domain-map (mk-domain-map model {"a" [:range 0 10]})]
    ;; numeric polynomial → constant LinearArgument (via maybe-numeric-arg)
    (is (instance? LinearArgument (linear-arg model {} [5])))
    (is (instance? LinearArgument (linear-arg model {} 5)))
    ;; single-var polynomial → var-backed LinearArgument
    (is (instance? LinearArgument (linear-arg model domain-map ["a"])))))

(deftest test-literal-arg
  (let [literal-arg @(ns-resolve 'quandary.quandary 'literal-arg)
        _           @load-native
        model       (CpModel.)
        domain-map  (mk-domain-map model {"b" [:boolean]})]
    ;; number → long (pure, no model/domain access)
    (is (= 5 (literal-arg {} 5)))
    (is (= 5 (literal-arg {} [5])))
    ;; plain var → BoolVar from domain
    (is (lvar? (literal-arg domain-map "b")))
    (is (lvar? (literal-arg domain-map ["b"])))
    ;; negated var → a Literal (from BoolVar.not())
    (is (instance? Literal (literal-arg domain-map "!b")))
    (is (instance? Literal (literal-arg domain-map ["!b"])))))

(defn solve-with-1-worker
  [domain equations & [options]]
  (first (solve-equations domain equations (merge {:num-workers 1} options))))

(deftest test-strip-temp-vars
  (is (= (strip-temp-vars {:foo 1 "bar" 2 "TEMP.baz" 3})
         {:foo 1 "bar" 2}))
  (is (= (strip-temp-vars [{:foo 1 "bar" 2 "TEMP.baz" 3}
                           {"a" 1 "TEMP.baz" 3}])
         [{:foo 1 "bar" 2}
          {"a" 1}])))

(deftest test-the-parser
  (is (= (the-parser "2x - 3y = z
                      z < 12")
         [[:EQ
           [:EXPR
            [:TERM [:NUMBER "2"] "x"]
            [:MINUS "-"]
            [:TERM [:NUMBER "3"] "y"]]
           "="
           [:EXPR [:TERM "z"]]]
          [:EQ
           [:EXPR [:TERM "z"]]
           "<"
           [:EXPR [:TERM [:NUMBER "12"]]]]]))
  (is (= (the-parser "y = a b")
         (the-parser "y = a * b")))
  (is (= (the-parser "y = 2 a b")
         (the-parser "y = 2 a * b")
         (the-parser "y = 2 * a * b"))))

(deftest test-negative-term
  (is (= (negative-term 5) [-5]))
  (is (= (negative-term 5 "x") [-5 "x"]))
  (is (= (negative-term "a" "b") [-1 "a" "b"]))
  (is (= (negative-term 3 "a" "b") [-3 "a" "b"])))

(deftest test-parse-equations
  (is (= (parse-equations "") []))
  (is (= (parse-equations nil) []))
  (is (= (parse-equations "y = a * b") [["=" [["y"]] [["a" "b"]]]]))
  (is (= (parse-equations "  " "2x - 3y = z" "z < 12")
         [["=" [[2 "x"] [-3 "y"]] [["z"]]]
          ["<" [["z"]] [[12]]]])))

(deftest test-!
  (is (= (! "xerox") "!xerox"))
  (is (= (! "!xerox") "xerox"))
  (is (thrown? ExceptionInfo (! ""))))

(deftest test-divide-seq-by
  (is (= (divide-seq-by identity []) []))
  (is (= (divide-seq-by identity nil) []))
  (is (= (divide-seq-by pos? [-1 0 -2 3 3 -1 -2]) [[-1 0 -2] [3] [3 -1 -2]])))

(deftest test-solve-equations
  (is (#{11 12 13}
       (-> (solve-with-1-worker {"a" [:range 10 15]}
                                [[">=" ["a"] [11]]
                                 ["<=" ["a"] [13]]])
           (get "a"))))
  (is (#{11 12 13}
       (-> (solve-with-1-worker {"a"      [:range 10 15]
                                 "TEMP.b" [13]}
                                [[">=" ["TEMP.b"] ["a"] [11]]])
           (get "a"))))

  (is (= (solve-with-1-worker {"a"      [:range 0 20]
                               "TEMP.b" [10]}
                              [["add-modulo-equality" ["a"] ["TEMP.b"] [3]]])
         {"a" 1}))

  ;; >=
  (are [domain-b eqs expected]
       (= (-> (solve-with-1-worker {"a" [10 11] "b" domain-b} eqs) (get "a"))
          expected)
    [11 12] [[">=" ["a"] ["b"]]] 11
    [11] [[">=" ["a"] [7]]] 10                              ; b can be any domain w/ 1 option (just to fulfill mk-domain-map
    [11] [[">=" [10] ["a"]]] 10)

  ;; >
  (are [domain-b eqs expected]
       (= (-> (solve-with-1-worker {"a" [10 11] "b" domain-b} eqs) (get "a"))
          expected)
    [11 12] [[">" ["a"] ["b"]]] nil
    [10 11 12] [[">" ["a"] ["b"]]] 11
    [11] [[">" ["a"] [10]]] 11
    [11] [[">" [11] ["a"]]] 10)

  ;; 3-arg >
  (is (= (-> (solve-with-1-worker {"a" [20 21]
                                   "b" [14 15]}
                                  [[">" ["a"] ["b"] [14]]])
             ((juxt #(get % "a") #(get % "b"))))
         [20 15]))

  ;; add-division-equality
  (is (= (-> (solve-with-1-worker {"a" [:range 10 15]
                                   "b" [5]
                                   "c" [:range 0 100]}
                                  [["add-division-equality" ["c"] [[2 "a"]] ["b"]]])
             ((juxt #(get % "a") #(get % "c"))))
         ;[15 6]
         ;[14 5]
         ;[13 5]
         ;[12 4]
         ;[11 4]
         [10 4])))

(deftest test-solve-equations-add-max-equality
  (is (= (solve-with-1-worker {"a"      [20]
                               "b"      [2]
                               "answer" [:range 0 100]}
                              [["add-max-equality" [["answer"]] [["a"]] [["b"]]]])
         {"a" 20 "b" 2 "answer" 20})))

(deftest test-solve-equations-sum
  (is (= (solve-with-1-worker {"a" [:range 0 100]}
                              [["=" ["a"] [10 11 12]]])
         {"a" 33})))

(deftest test-solve-equations-add-division-equality
  (is (= (solve-with-1-worker {"a"      [20]
                               "b"      [2]
                               "answer" [:range 0 100]}
                              [["add-division-equality" ["answer"] ["a"] ["b"]]])
         {"a" 20 "b" 2 "answer" 10}))
  ;; Per CP-Sat docs, OnlyEnforceIf only works on bool_or, bool_and, and linear constraints.
  (is (thrown? ExceptionInfo
               (solve-with-1-worker {"answer" [:range 0 100]
                                     "foo"    [:boolean]}
                                    [["add-division-equality" ["answer"] [13] [2] :only-if "foo"]]
                                    {:assumptions ["foo"]})
               []))
  ;; Using constants
  (is (= (solve-with-1-worker {"answer" [:range 0 100]}
                              [["add-division-equality" ["answer"] [21] [3]]])
         {"answer" 7})))

(deftest test-solve-equations-add-bool-or
  (is (= (-> (solve-with-1-worker {"a" [:boolean]
                                   "b" [:boolean]
                                   "c" [:boolean]}
                                  [["add-bool-or" ["a" "b" "c"]]
                                   ["!=" ["b"] ["c"]]])
             ((fn [{:strs [a b c]}] [a b c])))
         [1 0 1])))

(deftest test-implication
  (is (= (-> (solve-with-1-worker {"a" [:boolean]
                                   "b" [:boolean]}
                                  [["=>" ["a"] [(quandary.quandary/! "b")]]])
             ((fn [{:strs [a b]}] [a b])))
         [0 0])))

(deftest test-solve-equations-only-if
  ;; Basic only-if test
  (is (= (-> (solve-with-1-worker {"a" [:range 10 15]
                                   "b" [11]
                                   "c" [:boolean]}
                                  [["<=" ["b"] [13] ["a"] :only-if "c"]
                                   ["<=" ["b"] [10] ["a"] :only-if "!c"]])
             ((fn [{:strs [a b c]}] [a b c])))
         [13 11 1]))
  ;; Test chained only-if modifiers
  (let [result (->> (solve-equations {"a" [:range 10 12]
                                      "b" [:boolean]
                                      "c" [:boolean]}
                                     [["=" ["a"] [11] :only-if "b" :only-if "c"]]
                                     {:enumerate-all true})
                    (mapv (juxt #(get % "a") #(get % "b") #(get % "c")))
                    sort)]
    ;; When b and c are true, then a must be 11
    (is (= (filter (fn [[_ b c]] (= b c 1)) result)
           [[11 1 1]]))
    (is (= (count result) 10))))

(deftest test-solve-equations-with-assumptions
  ;; Basic only-if test
  (is (= (solve-with-1-worker {"a" [:range 10 15] "b" [:boolean]}
                              [["=" ["a"] [12] :only-if "b"]]
                              {:assumptions ["b"]})
         {"a" 12, "b" 1}))

  ;; Not working
  ;; NOT (!) doesn't work, but not sure why.
  ;(is (= (solve-equations {"a" [:range 10 15] "b" [:boolean]}
  ;                        [["=" ["a"] [12] :only-if "b"]]
  ;                        {:assumptions ["!b"]})
  ;       [{"a" 12, "b" 1}]))
  )

(deftest test-solve-interval-no-overlap2d
  (let [result (map (fn [{:strs [x2 y2]}] [x2 y2])
                    (solve-equations {"x1"    [0]
                                      "y1"    [0]
                                      "x1int" [:fixed-size-interval "x1" 4]
                                      "y1int" [:fixed-size-interval "y1" 4]
                                      "x2"    [:range 0 6]
                                      "y2"    [:range 0 4]
                                      "x2int" [:fixed-size-interval "x2" 4]
                                      "y2int" [:fixed-size-interval "y2" 4]}
                                     [["no-overlap-2d" [["x1int" "y1int"] ["x2int" "y2int"]]]]
                                     {:enumerate-all true}))]
    (is (= (filter (fn [[x y]] (< x 4)) result) [[3 4] [2 4] [1 4] [0 4]]))
    (is (= (count (filter (fn [[x y]] (= x 4)) result)) 5))))

(deftest test-solve-optional-interval-no-overlap2d
  (let [domain {"x1"          [0]
                "y1"          [0]
                "present-1-2" [:boolean]
                "x1int"       [:optional-fixed-size-interval "x1" "present-1-2" 4]
                "y1int"       [:optional-fixed-size-interval "y1" "present-1-2" 4]
                "x2"          [:range 0 6]
                "y2"          [:range 0 4]
                "x2int"       [:optional-fixed-size-interval "x2" "present-1-2" 4]
                "y2int"       [:optional-fixed-size-interval "y2" "present-1-2" 4]}
        result-f (fn [present-value]
                   (map (fn [{:strs [x2 y2]}] [x2 y2])
                        (solve-equations domain
                                         [["no-overlap-2d" [["x1int" "y1int"] ["x2int" "y2int"]]]
                                          ["=" ["present-1-2"] [present-value]]]
                                         {:enumerate-all true})))
        result-with-present (result-f 1)
        result-not-present (result-f 0)]

    ;; result-with-present
    (is (= (filter (fn [[x y]] (< x 4)) result-with-present) [[3 4] [2 4] [1 4] [0 4]]))
    (is (= (count (filter (fn [[x y]] (= x 4)) result-with-present)) 5))

    ;; result-not-present
    (is (count (filter (fn [[x y]] (< x 4)) result-not-present)) (* 4 5))
    (is (= (count (filter (fn [[x y]] (= x 4)) result-not-present)) 5))))

(deftest test-solve-optional-interval-no-overlap2d-mixed-presence
  (let [domain {"x1"        [0]
                "y1"        [0]

                "prsnt"     [:boolean]
                "not-prsnt" [:boolean]

                "x1int"     [:optional-fixed-size-interval "x1" "prsnt" 2]
                "y1int"     [:optional-fixed-size-interval "y1" "prsnt" 2]

                "x2"        [:range 0 2]
                "y2"        [0]
                "x2int"     [:optional-fixed-size-interval "x2" "not-prsnt" 2]
                "y2int"     [:optional-fixed-size-interval "y2" "not-prsnt" 2]

                "x3"        [:range 0 2]
                "y3"        [0]
                "x3int"     [:optional-fixed-size-interval "x3" "prsnt" 2]
                "y3int"     [:optional-fixed-size-interval "y3" "prsnt" 2]}
        result (sort
                 (map (fn [{:strs [x2 y2 x3 y3]}] [x2 y2 x3 y3])
                      (solve-equations domain
                                      [["no-overlap-2d" [["x1int" "y1int"] ["x2int" "y2int"] ["x3int" "y3int"]]]
                                       ["=" ["prsnt"] [1]]
                                       ["=" ["not-prsnt"] [0]]]
                                      {:enumerate-all true})))]
    ;; (1) is fixed at [0, 0], (2) is free b/c it's constraints are "not present", and (3) can not overlap (1)
    (is (= result [[0 0 2 0]
                   [1 0 2 0]
                   [2 0 2 0]]))))

(deftest test-allowed-assignments-tuples
  (let [allowed-wh [[10 20]
                    [3 21]]
        domain {["x1" "y1"] [:tuples allowed-wh]
                ["x2" "y2"] [:tuples allowed-wh]}
        result (sort
                (map (fn [{:strs [x1 y1 x2 y2]}] [x1 y1 x2 y2])
                     (solve-equations domain
                                      [["<=" ["y1"] ["y2"]]
                                       [">=" ["x1"] [5]]
                                       [">=" ["x2"] [1]]]
                                      {:enumerate-all true})))]
    (is (= result [[10 20 3 21]
                   [10 20 10 20]]))))
