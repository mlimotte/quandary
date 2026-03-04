(ns quandary.impl-test
  (:require [clojure.test :refer :all]
            [quandary.impl :refer :all]))

;; Polynomial Expansion

(deftest test-multiply-terms
  (let [multiply-terms @(ns-resolve 'quandary.impl 'multiply-terms)]
    (are [t1 t2 expected]
      (= expected (multiply-terms t1 t2))
      [2 "a"] [3 "b"] [6 "a" "b"]                           ;; basic multiply + concat vars
      [1 "b"] [1 "a"] [1 "a" "b"]                           ;; variables are sorted
      [4] [5 "x"] [20 "x"]                                  ;; constant term × var term
      [2 "a" "b"] [3 "c"] [6 "a" "b" "c"])))                ;; multi-var term × single-var term

(deftest test-multiply-polynomials
  (let [multiply-polynomials @(ns-resolve 'quandary.impl 'multiply-polynomials)]
    (are [poly1 poly2 expected]
      (= expected (multiply-polynomials poly1 poly2))
      ;; Single-term × single-term
      [[1 "a"]] [[1 "b"]] [[1 "a" "b"]]
      ;; 2-term × 1-term: cartesian product
      [[2 "x"] [3 "y"]] [[1 "z"]] [[2 "x" "z"] [3 "y" "z"]]
      ;; 2-term × 2-term: 4-way cartesian product (unsimplified)
      [[1 "a"] [1 "b"]] [[1 "a"] [1 "b"]] [[1 "a" "a"] [1 "a" "b"] [1 "a" "b"] [1 "b" "b"]]
      ;; Constant terms
      [[5]] [[3]] [[15]])))

(deftest test-add-polynomials
  (let [add-polynomials @(ns-resolve 'quandary.impl 'add-polynomials)]
    (are [poly1 poly2 expected]
      (= expected (add-polynomials poly1 poly2))
      [[1 "a"]] [[1 "b"]] [[1 "a"] [1 "b"]]
      [[2 "x"] [3 "y"]] [[4 "z"]] [[2 "x"] [3 "y"] [4 "z"]]
      [[1 "a"] [1 "b"]] [[1 "c"]] [[1 "a"] [1 "b"] [1 "c"]])
    (is (= [[1 "a"] [1 "b"] [1 "c"]]
           (add-polynomials [[1 "a"]] [[1 "b"]] [[1 "c"]]))))) ;; variadic: 3 polys

(deftest test-simplify-polynomial
  (let [simplify-polynomial #'quandary.impl/simplify-polynomial]
    ;; "combines like terms"
    (is (= [[5 "a"]]
           (simplify-polynomial [[2 "a"] [3 "a"]])))
    ;; "removes zero-coefficient terms"
    (is (= [[2 "b"]]
           (simplify-polynomial [[3 "a"] [-3 "a"] [2 "b"]])))
    ;; "preserves distinct terms"
    (is (= [[2 "a"] [3 "b"]]
           (simplify-polynomial [[2 "a"] [3 "b"]])))
    ;; "combines multi-variable like terms"
    (is (= [[3 "a" "b"]]
           (simplify-polynomial [[1 "a" "b"] [2 "a" "b"]])))))

(deftest test-flatten-to-polynomial
  (are [expr expected]
    (= expected (flatten-to-polynomial expr))
    ;; atoms
    'a [[1 "a"]]
    5 [[5]]
    -5 [[-5]]
    ;; addition: concat, preserves order
    '(+ a b) [[1 "a"] [1 "b"]]
    '(+ a b c) [[1 "a"] [1 "b"] [1 "c"]]
    ;; subtraction
    '(- a) [[-1 "a"]]
    '(- a b) [[1 "a"] [-1 "b"]]
    '(- a b c) [[1 "a"] [-1 "b"] [-1 "c"]]
    '(- (* 3 a) (* 2 b)) [[3 "a"] [-2 "b"]]
    '(+ a (- b c)) [[1 "a"] [1 "b"] [-1 "c"]]
    '(* a (- b c)) [[1 "a" "b"] [-1 "a" "c"]]
    ;; multiplication
    '(* a b) [[1 "a" "b"]]
    '(* 3 a) [[3 "a"]]
    '(* a b c) [[1 "a" "b" "c"]]
    '(* 2 a b) [[2 "a" "b"]]
    ;; distribution
    '(* a (+ b c)) [[1 "a" "b"] [1 "a" "c"]]
    ;; expansion
    '(* (+ a b) (+ a b)) [[1 "a" "a"] [2 "a" "b"] [1 "b" "b"]]
    '(+ (* a b) (* a b)) [[2 "a" "b"]]
    '(+ (* x b) (* x (+ c d) 2 (+ e f))) [[1 "b" "x"]
                                          [2 "c" "e" "x"]
                                          [2 "c" "f" "x"]
                                          [2 "d" "e" "x"]
                                          [2 "d" "f" "x"]]))

;; / Polynomial Expansion

(deftest test-remove-coeffcient-of-one
  (are [poly expected]
    (= expected (remove-coeffcient-of-one poly))
    [[1 "a" "b"] [-1 "a" "c"]] [["a" "b"] [-1 "a" "c"]]
    [[2 "a" "b"] [-1 "a" "c"]] [[2 "a" "b"] [-1 "a" "c"]]
    [[2 "a"]] [[2 "a"]]
    [["a"]] [["a"]]))

(deftest test-parse-var-name
  (are [s expected]
    (= (parse-var-name s) expected)
    "a" {:full-name "a" :varname "a"}
    "$a" {:dollar? true :full-name "a" :varname "a"}
    "$#a1.b" {:dollar? true :dollar-hash? true :full-name "a1.b" :prefix0 "a1" :suffix "b" :varname "b"}
    "$10" nil                                               ; must start with a letter
    "$abc" {:dollar? true :full-name "abc" :varname "abc"}
    "$ab.c.d" {:dollar? true :full-name "ab.c.d" :suffix "c.d" :prefix0 "ab" :varname "d"}
    "$a" {:dollar? true :full-name "a" :varname "a"}
    nil nil))

;; We need a Global var for the following test
(def five 5)

(deftest test-temp-int-var
  (testing "computes range correctly"
    (are [context v1 v2 expected-range]
      (= expected-range (-> (temp-int-var context v1 v2 nil) vals first))
      ;; Positive × positive
      {:domain {"r" [:range 0 100] "b" [:range 0 200]}}
      "r" "b" [:range 0 20000]
      ;; Mixed signs: min must be d1min × d2max (negative)
      {:domain {"x" [:range -10 10] "y" [:range 0 5]}}
      "x" "y" [:range -50 50]
      ;; Both negative: product is positive, min from largest-magnitude pair
      {:domain {"x" [:range -10 -1] "y" [:range -5 -1]}}
      "x" "y" [:range 1 50]
      ;; Enumerated domain: uses min/max of the values
      {:domain {"x" [1 3 5] "y" [:range 0 10]}}
      "x" "y" [:range 0 50]
      ;; Boolean domain [0 1] × range
      {:domain {"flag" [:boolean] "cost" [:range 0 100]}}
      "flag" "cost" [:range 0 100]))
  (testing "varname is a TEMP var prefixed with both input var names"
    (let [result (temp-int-var {:domain {"r" [:range 0 100] "b" [:range 0 200]}} "r" "b" "foo")
          varname (-> result keys first)]
      (is (.startsWith varname "TEMP.r.b.foo")))))

(deftest test-qdsl-internal
  ;; merges initial-context and entries into one context map
  (is (= (qdsl-internal {:initial-context {:domain {"a" [:range 0 10]}}}
                        [{:domain {"b" [:range 0 20]}}
                         {:equations [['= "a" "b"]]}])
         {:domain    {"a" [:range 0 10] "b" [:range 0 20]}
          :equations [['= [["a"]] [["b"]]]]}))
  ;; nil initial-context is treated as empty
  (is (= (qdsl-internal {} [{:equations [['= "x" 5]]}])
         {:equations [['= [["x"]] [[5]]]]})))

(deftest test-qdsl-internal-only-enforce-if
  (is (= (-> (qdsl-internal {:initial-context {:domain {"a"  [:range 0 10]
                                                        "on" [:boolean]}}}
                            [{:domain {"b" [:range 0 20]}}
                             {:equations [['= "a" "b"]]}
                             {:equations [[:only-enforce-if "on"
                                           ['= "a" 3]
                                           :only-enforce-else
                                           ['= "a" 7]]]}
                             ])
             :equations)
         [['= [["a"]] [["b"]]]
          ['= [["a"]] [[3]] :only-if "on"]
          ['= [["a"]] [[7]] :only-if "!on"]
          ])))

(deftest test-dqsl-process-body
  ;; plain symbol keys in a domain map become string keys
  (is (= (dqsl-process-body (constantly false) ['{"a" [:range 0 10]}])
         [{:domain {"a" [:range 0 10]}}]))
  ;; consecutive equation vectors are consolidated into one :equations entry
  (let [x "X"
        q "Q"]
    (is (= (dqsl-process-body #(boolean (#{"q" "r"} %1))
                              [['= "a" 5]
                                  ['>= "a" 3]
                                  '{($ x "y")     [:range 0 10]
                                    ($ q "foo.bar") [:boolean]
                                    "z.z" [:boolean]}
                               '[= (+ ($ q "foo.bar") 1) ($ q "foo.baz")]
                               ])
           [{:equations [['(quote =) "a" 5]
                         ['(quote >=) "a" 3]]}
            {:domain {"z.z"                                      [:boolean]
                      (list 'quandary.impl/$ 'q "foo.bar") [:boolean]
                      (list 'quandary.impl/$ 'x "y")       [:range 0 10]}}
            {:equations [['(quote =)
                          (list 'list '(quote +) (list 'quandary.impl/$ 'q "foo.bar") 1)
                          (list 'quandary.impl/$ 'q "foo.baz")]]}]))))
