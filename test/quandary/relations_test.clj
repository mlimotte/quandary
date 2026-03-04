(ns quandary.relations-test
  (:require
    [clojure.test :refer :all]
    [flatland.ordered.map :refer [ordered-map]]
    [quandary.relations :refer :all]
    [quandary.quandary :as q]
    [quandary.util :as qutil]))

(deftest test-wip-sqrto
  (binding [qutil/*NEXT_ID* (atom 0)]
    (let [x 25
          de (sqrto "answer" "x" 1000)]
      (is (= (-> (q/solve-equations (merge (:domain de)
                                           {"x"      [:range 0 1000]
                                            "answer" [:range 0 1000]})
                                    (concat (:equations de)
                                            [["=" ["x"] [x]]]))
                 first
                 (get "answer"))
             5)))
    (let [de (qutil/collate (sqrto "y" "squared" 1000)
                            {:domain {"y"       [:range 0 100]
                                      "squared" [4]}})]
      (is (= (first (q/solve-equations (:domain de) (:equations de)))
             {"squared" 4 "y" 2})))))

(deftest test-ceil-div-o
  (is (= (-> (q/solve-equations {"x"      [30]
                                 "answer" [:range 0 10000]}
                                (:equations (ceil-div-o "answer" "x" 2)))
             first
             (get "answer"))
         15))
  (is (= (-> (q/solve-equations {"x"      [:range 0 100]
                                 "y"      [5]
                                 "answer" [6]}
                                (:equations (ceil-div-o "answer" "x" "y")))
             first
             (get "x"))
         ; Any of these numbers, when divided by 5 will produce 6 (after `ceil` fn)
         ;[30 29 28 27 26]  ; TODO or maybe just test for any number in this set
         26))
  (is (= (-> (q/solve-equations {"x"      [12]
                                 "y"      [4]
                                 "answer" [:range 0 100]}
                                (:equations (ceil-div-o "answer" "x" "y")))
             first
             (get "answer"))
         3))
  ;; Applying only-if on `ceil` relation
  (let [eqs (mapv
              (fn [eq] (conj eq :only-if "foo"))
              (:equations (ceil-div-o "answer" "x" "y")))]
    (is (= (q/solve-equations {"x"      [12]
                               "y"      [4]
                               "answer" [:range 0 100]
                               "foo"    [:boolean]}
                              eqs
                              {:assumptions ["foo"]})
           [{"answer" 3
             "foo"    1                                     ; true
             "x"      12
             "y"      4}]))))

(deftest test-mean
  (binding [qutil/*NEXT_ID* (atom 0)]
    ;; With single values
    (let [as (ordered-map (map (fn [a] [(str "a" a) [(* 2 a)]]) (range 0 5)))
          rules (qutil/collate
                  {:domain (assoc as "answer" [:range 0 100])}
                  (mean "answer" (map first as) 0 1000))]
      (is (qutil/ordered-domain? {:source "test-mean"
                                  :domain (:domain rules)}
                                 rules))
      (is (= (-> (q/solve-equations (:domain rules) (:equations rules))
                 first
                 (get "answer"))
             ;; Total = 0 + 2 + 4 + 6 + 8 = 20; mean = 20 / 5 = 4
             4)))
    ;; With terms [numeric varname]
    (let [xs (map (fn [x] [(str "x" x) [(* 2 x)]]) (range 0 5))
          rules (qutil/collate
                  {:domain (assoc (into {} xs) "answer" [:range 0 100])}
                  (mean "answer"
                        (map-indexed (fn [n [k _]] (vector n k)) xs)
                        0 1000))]
      (is (= (-> (q/solve-equations (:domain rules) (:equations rules))
                 first
                 (get "answer"))
             ;; Total = 0 * 0 + 1 * 2 + 2 * 4 + 3 * 6 + 4 * 8 = 60
             ;; mean = 60 / 10 = 6
             6)))
    ;; With terms (both vars)
    (let [xs (map (fn [x] [(str "x" x) [(* 2 x)]]) (range 0 5))
          ns (map (fn [n] [(str "n" n) [n]]) (range 0 5))
          rules (qutil/collate
                  {:domain (assoc (into {} (concat xs ns)) "answer" [:range 0 100])}
                  (mean "answer"
                        (map (fn [[nk _] [xk _]] (vector nk xk)) ns xs)
                        0 1000))]
      (is (= (-> (q/solve-equations (:domain rules) (:equations rules))
                 first
                 (get "answer"))
             ;; Total = 0 * 0 + 1 * 2 + 2 * 4 + 3 * 6 + 4 * 8 = 60
             ;; mean = 60 / 10 = 6
             6)))
    ;; With terms (both vars, n = 0)
    (let [xs (map (fn [x] [(str "x" x) [(* 2 x)]]) (range 0 2))
          ns (map (fn [n] [(str "n" n) [0]]) (range 0 2))
          rules (qutil/collate
                  {:domain (assoc (into {} (concat xs ns)) "answer" [:range 0 100])}
                  (mean "answer"
                        (map (fn [[nk _] [xk _]] (vector nk xk)) ns xs)
                        0 1000))]
      (is (= (-> (q/solve-equations (:domain rules) (:equations rules))
                 first
                 (get "answer"))
             ;; Total = 0 * 0 + 1 * 2 + 2 * 4 + 3 * 6 + 4 * 8 = 60
             ;; mean = 60 / 10 = 6
             0)))

    ))

(deftest test-variance
  (binding [qutil/*NEXT_ID* (atom 0)]

    ;; Shape: singles
    (let [rules (qutil/collate
                  {:domain (ordered-map [["a" [2]] ["b" [4]] ["answer" [:range 0 100]]])}
                  (variance "answer" ["a" "b"] 0 1000))]
      (is (qutil/ordered-domain? {:source "test-variance"
                                  :domain (:domain rules)} rules))
      (is (= (-> (q/solve-equations (:domain rules) (:equations rules))
                 first
                 (get "answer"))
             ;; Total = 2 + 4 = 6; mean = 6 / 2 = 3
             ;; Diffs = 1   1
             ;; Sum of sq of diffs = 1 + 1 = 2
             ;; floor(2 / 2) = 1
             1)))

    ;; Shape: singles
    (let [as (map (fn [a] [(str "a" a) [(* 2 a)]]) (range 0 5))
          rules (qutil/collate
                  {:domain (assoc (into {} as) "answer" [:range 0 100])}
                  (variance "answer" (map first as) 0 1000))]
      (is (= (-> (q/solve-equations (:domain rules) (:equations rules))
                 first
                 (get "answer"))
             ;; Total = 0 + 2 + 4 + 6 + 8 = 20; mean = 20 / 5 = 4
             ;; Diffs = 4  2    0   2   4
             ;; Sum of sq of diffs = 16 + 4 + 4 + 16 = 40
             ;; floor(40 / 5) = 8
             8)))

    ;; Shape: tuple terms
    (let [as (map (fn [a] [(str "a" a) [(* 2 a)]]) (range 0 5))
          rules (qutil/collate
                  {:domain (assoc (into {} as) "answer" [:range 0 100])}
                  (variance "answer"
                            (map-indexed (fn [n [k _]] (vector n k)) as)
                            0 1000))]
      (is (= (-> (q/solve-equations (:domain rules) (:equations rules))
                 first
                 (get "answer"))
             ;; Total = 60; mean = 6 (see `test-mean`)
             ;; Diffs = 6    4     2    0    2
             ;; Sum of sq of diffs = 0*36 + 1*16 + 2*4 + 3*0 + 4*4 = 40
             ;; floor(40 / 10) = 4
             4)))

    ;; Shape: tuple terms (both vars)
    (let [xs (map (fn [x] [(str "x" x) [(* 2 x)]]) (range 0 5))
          ns (map (fn [n] [(str "n" n) [n]]) (range 0 5))
          rules (qutil/collate
                  {:domain (assoc (into {} (concat xs ns)) "answer" [:range 0 100])}
                  (variance "answer"
                            (map (fn [[nk _] [xk _]] (vector nk xk)) ns xs)
                            0 1000))]
      (is (= (-> (q/solve-equations (:domain rules) (:equations rules))
                 first
                 (get "answer"))
             ;; Same answer as above
             4)))

    ;; Shape: tuple terms (both vars), n = 0
    (let [xs (map (fn [x] [(str "x" x) [(* 2 x)]]) (range 0 2))
          ns (map (fn [n] [(str "n" n) [0]]) (range 0 2))
          rules (qutil/collate
                  {:domain (assoc (into {} (concat xs ns)) "answer" [:range 0 100])}
                  (variance "answer"
                            (map (fn [[nk _] [xk _]] (vector nk xk)) ns xs)
                            0 1000))]
      (is (= (-> (q/solve-equations (:domain rules) (:equations rules))
                 first
                 (get "answer"))
             ;; Same answer as above
             0)))
    ))

(deftest test-betweeno
  (let [de (qutil/collate {:domain {"a"      [:range 5 6]
                                    "b"      [8]
                                    "c"      [4 5 6 7 8 9]
                                    "answer" [:boolean]}}
                          (is-exclusive-betweeno "c" "a" "b" "answer"))
        result (q/solve-equations (:domain de)
                                  (concat (:equations de)
                                          [["=" ["answer"] [1]]]))]
    (is (= (->> result
                first
                ((fn [{:strs [a b c]}] [a b c])))
           [5 8 6]
           ;; Other solutions below (maybe we should add an objective fn, to be sure we get a specific one?):
           ;[5 8 7]
           ;[6 8 7]
           ))))
