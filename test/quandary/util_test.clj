(ns quandary.util-test
  (:require
   [quandary.util :refer :all]
   [clojure.test :refer :all])
  (:import
   clojure.lang.ExceptionInfo))

(deftest test-dot
  (is (= (dot nil) ""))
  (is (= (dot "") ""))
  (is (= (dot "foo") "foo"))
  (is (= (dot "foo" "bar") "foo.bar"))
  (is (= (dot ["foo" "bar"]) "foo.bar"))
  (is (= (dot "foo" "bar" :temp) "TEMP.foo.bar"))
  (is (= (dot "TEMP.foo" "bar" :temp) "TEMP.foo.bar"))
  (is (= (dot ["foo"]) "foo"))
  (is (= (dot "foo" "bar" "baz") "foo.bar.baz"))
  (is (= (dot []) ""))
  (is (= (dot ["foo" :temp]) "TEMP.foo"))
  (is (= (dot ["TEMP.foo" :temp]) "TEMP.foo")))

(deftest test-merge-with-using-key
  (is (= (merge-with-using-key (fn [k v1 v2] (+ v1 v2)) {:a 1 :b 2} {:b 3})
         {:a 1 :b 5}))
  (is (= (merge-with-using-key (fn [k v1 v2] (+ v1 v2)) {:a 1 :b 2} {})
         {:a 1 :b 2}))
  (is (= (merge-with-using-key (fn [k v1 v2] (+ v1 v2)) {:a 1 :b 2} nil)
         {:a 1 :b 2}))
  (is (= (merge-with-using-key (fn [k v1 v2] k) {:a 1 :b 2} {:b 3})
         {:a 1 :b :b})))

(deftest test-collate-merge
  (is (= (collate-merge :domain {} {}) {}))
  (is (= (collate-merge :domain {"a" :apple} {"b" :banana}) {"a" :apple "b" :banana}))
  (is (thrown? ExceptionInfo (collate-merge :domain {"a" :apple} {"a" :avocado})))
  (is (= (collate-merge :equations [2 4 6] [1 3 5]) [2 4 6 1 3 5]))
  (is (= (collate-merge :equations '(2 4 6) '(1 3 5)) [2 4 6 1 3 5]))
  (is (thrown? clojure.lang.ExceptionInfo (collate-merge :unknown-key {:a 1} {:a 2}))))

(deftest test-collate
  (is (= (collate nil) {}))
  (is (= (collate) {}))
  (is (= (collate {:equations [1 2 3]}
                  {:equations [4 5 6]})
         {:equations [1 2 3 4 5 6]}))
  (is (= (collate {:equations [1 2 3]}
                  nil)
         {:equations [1 2 3]}))
  (is (= (collate {:equations [1 2 3]}
                  {:equations []})
         {:equations [1 2 3]}))
  (is (= (collate {:equations [1 2 3]}
                  [{:equations [4 5 6]}
                   {:equations [7 8 9]}])
         {:equations (range 1 10)}))
  (is (= (collate {:domain    {"a" :apple}
                   :equations [1 2 3]}
                  {:domain {"b" :banana}}
                  {:equations [4 5 6]})
         {:domain    {"a" :apple "b" :banana}
          :equations [1 2 3 4 5 6]}))
  (is (= (collate {:domain    {"a" :apple}
                   :equations [1 2 3]}
                  (collate
                   [{:domain {"b" :banana}}
                    {:equations [4 5 6]}]))
         {:domain    {"a" :apple "b" :banana}
          :equations [1 2 3 4 5 6]})))

(deftest test-collate-tagging
  (let [rules1 (collate (with-meta {:domain    {"a" [:boolean]}
                                    :equations [[1 2 3]]}
                          {:tag "A"})
                        (with-meta {:domain {"b" [:boolean]}} {:tag "B"})
                        (with-meta {:equations [[4 5 6]]} {:tag "C"}))]
    (is (= (->> (collate rules1) :domain vals (map meta) set)
           #{{:tag "A"} {:tag "B"}}))
    (is (= (->> (collate rules1) :equations (map meta))
           [{:tag "A"} {:tag "C"}]))))

(deftest test-dollar-vars
  (let [foo {:quandary.quandary/var-name-prefix "FOO"}
        bar {:quandary.quandary/var-name-prefix "BAR"}]
    (is (= (dollar-vars {:x (format "%s,%s,%s" $foo.a $foo.b $bar.x-y)})
           {:x "FOO.a,FOO.b,BAR.x-y"}))
    (is (= (dollar-vars {:equations [["=" [$foo.a] [$foo.b]]]}
                        {:equations [["=" [$bar.x-y] [1]]]})
           {:equations [["=" ["FOO.a"] ["FOO.b"]]
                        ["=" ["BAR.x-y"] [1]]]}))))
