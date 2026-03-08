(ns quandary.api-test
  (:require [clojure.test :refer :all]
            [quandary.api :refer :all]))

(deftest test-example-solve-rabbits-and-pheasants
  (is (= (solve (qdsl {}
                      {"r" [:range 0 100]
                       "p" [:range 0 100]}
                      [= (+ "r" "p") 20]
                      [= (+ (* 4 "r") (* 2 "p")) 56])
                {:enumerate-all true})
         [{"r" 8 "p" 12}])))

(deftest test-qdsl
  (let [x "X"
        q "Q"
        result (qdsl {:tag "test-qdsl"}
                     [= "a" 5]
                     [>= "a" 3]
                     {($ x "y-y")     [:range 0 10]
                      ($ q "foo.bar") [:boolean]
                      "z.z"           [:boolean]}
                     [= (+ ($ q "foo.bar") 1) ($ q "foo.baz")])]
    (is (= "test-qdsl" (-> result meta :tag)))
    (is (= {"X.y-y"     [:range 0 10]
            "Q.foo.bar" [:boolean]
            "z.z"       [:boolean]}
           (:domain result)))
    (is (= [['= [["a"]] [[5]]]
            ['>= [["a"]] [[3]]]
            ;; NOTE: $q.foo.baz is not defined in the domain, but it will be the
            ;; responsibility of `solve` to report that error.
            ['= [["Q.foo.bar"] [1]] [["Q.foo.baz"]]]]
           (:equations result)))))

(deftest test-qdsl-add-max-equality
  (is (= (:equations (qdsl {}
                           {"a"      [:range 0 10]
                            "b"      [20]
                            "answer" [:range 0 1000]}
                           [add-max-equality "answer" "a" "b"]))
         ['(add-max-equality [["answer"]] [["a"]] [["b"]])]))
  (is (= (:equations (qdsl {}
                           {"a"      [:range 0 10]
                            "b"      [20]
                            "answer" [:range 0 1000]}
                           [add-max-equality (+ "answer" 1) "a" (* 2 "b")]))
         ['(add-max-equality [["answer"] [1]] [["a"]] [[2 "b"]])]))
  (is (= (->> (solve (qdsl {}
                           {"a"      [:range 0 10]
                            "b"      [20]
                            "answer" [:range 0 1000]}
                           [add-max-equality (+ "answer" 1) "a" (* 2 "b")]))
              (map #(get % "answer"))
              set)
         #{39})))

(deftest test-qdsl-only-enforce-if
  (let [f (fn [on-value]
            (qdsl {:tap-entries true :tag "test-qdsl-only-enforce-if"}
                  {"a"  [:range 0 10]
                   "on" [:boolean]
                   "b"  [:range 0 20]}
                  [= "on" on-value]                             ; true
                  [= "a" "b"]
                  [:only-enforce-if "on"
                   [= "a" 3]
                   :only-enforce-else
                   [= "a" 7]]))]
    (is (= (->> (f 1) solve (map #(get % "b")) set) #{3}))
    (is (= (->> (f 0) solve (map #(get % "b")) set) #{7}))))

(deftest test-qdsl-add-bool-or
  ;; Mirror of test-solve-equations-add-bool-or in quandary_test.clj, using qdsl macro.
  ;; add-bool-or takes a single polynomial argument — variables are packed with +.
  (is (= (->> (solve (qdsl {}
                           {"a" [:boolean]
                            "b" [:boolean]
                            "c" [:boolean]}
                           [add-bool-or (+ "a" "b" "c")]
                           [!= "b" "c"])
                     {:enumerate-all true})
              (map (fn [{:strs [a b c]}] [a b c]))
              set)
         #{[0 0 1] [1 0 1] [0 1 0] [1 1 0]})))

(deftest test-qdsl-other-function-calls
  (let [x 9]
    (is (= (-> (qdsl {}
                     {"answer" [:range 0 1000]}
                     [= "answer" (inc x)])
               :equations
               first)
           ['= [["answer"]] [[10]]]))))
