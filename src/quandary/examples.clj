(ns quandary.examples
  (:require
   [quandary.quandary :as q]
   [clojure.pprint :as pprint])
  (:import
   [com.google.ortools.sat CpModel CpSolver LinearExpr]))

(defn rabbits-and-pheasants
  "Solve the Rabbits and Pheasants problem:

  In a field of rabbits and pheasants, there are 20 heads and 56 legs. How many
  rabbits and pheasants are there?
  Hint: Rabbits have 4 legs, pheasants have 2.

  See OR-Tools solutions in other languages at
  https://git.xkool.org/hw/or-tools/-/blob/e59073c45f054867ef4c80c99db94d985c9b5679/ortools/sat/doc/integer_arithmetic.md#rabbits-and-pheasants-examples
  "
  []
  @q/load-native
  (q/solve-equations {"r" [:range 0 100]
                      "p" [:range 0 100]}
                     (q/parse-equations "
                       r + p = 20
                       4 r + 2 p = 56 ")))

;(defn cp-example2
;  "Using the CP-SAT solver for integers. Optimal Solution"
;  []
;  @load-native
;  (let [model  (CpModel.)
;        [x y z :as vars] (doall (for [v ["x" "y" "z"]] (.newIntVar model (int 0) (int 2) v)))
;        solver (CpSolver.)
;        _      (do (.addDifferent model x y))
;        status (.solve solver model)]
;    {:status (str status)
;     :values (mapv #(.value solver %) vars)}))

(defn cp-example3
  "Using the CP-SAT solver for integers. All solutions."
  []
  @q/load-native
  (let [model    (CpModel.)
        x        (.newIntVarFromDomain model (q/domain 10 20 30) "x")
        y        (.newIntVar model 0 5 "y")
        xy       (.newIntVar model 0 100 "xy")
        solver   (doto (CpSolver.)
                   (-> .getParameters (.setEnumerateAllSolutions true)))
        cb-state (atom {})
        cb       (q/mk-solutions-callback cb-state [y x xy] {})]
    ;; Constraints
    ;; xy = x * y
    (.addMultiplicationEquality model xy y x)
    ;; Solve
    (.solve solver model cb)
    ;; Return State
    (->> cb-state deref :solutions (map :values))))

(defn basic-le
  []
  @q/load-native
  (let [model    (CpModel.)
        expr     (LinearExpr/newBuilder)
        x        (.newIntVar model 1 3 "x")
        y        (.newIntVar model 1 4 "y")
        result   (.newIntVar model 0 10 "result")
        solver   (doto (CpSolver.)
                   (-> .getParameters (.setEnumerateAllSolutions true)))
        cb-state (atom {})
        cb       (q/mk-solutions-callback cb-state [y x result] {})]
    ;; Expression     expr = 2x + 3y
    (.addTerm expr x 2)
    (.addTerm expr y 3)
    (.addEquality model result expr)
    ;; Run it and return results
    (.solve solver model cb)
    ;; Return solutions
    (->> cb-state deref :solutions (map :values))))

;(defn basic-with-wrappers
;  []
;  @q/load-native
;  (let [model   (CpModel.)
;        expr    (LinearExpr/newBuilder)
;        [expr-v expr-f] (experiments/var-for-expr model expr 0 10)
;        x       (.newIntVar model 1 3 "x")
;        y       (.newIntVar model 1 4 "y")
;        wrapper (q/solver-with-callback [x y expr-v] {:name-value false})]
;    ;; Expression     expr = 2x + 3y
;    (.addTerm expr x 2)
;    (.addTerm expr y 3)
;    (expr-f)
;    ;; Return solutions
;    (q/solve! wrapper model)
;    (->> wrapper :solved-state deref :solutions (map :values))))

;; DEMO
(defn basic-le-with-quandary
  []
  (println "Multiple Strings, 3 Vars")
  (let [domain {"x" [:range 0 20]
                "y" [:range 0 20]
                "z" [9 10 11 12 13]}
        eqs1   (q/parse-equations "2x - 3y = z"
                                  "z < 12")]
    (pprint/pprint (q/solve-equations domain eqs1)))

  (println "\nData Structure Syntax, 2 Vars")
  (let [domain {"x" [:range 0 20]
                "y" [:range 0 20]}
        eqs2   [["=" ["y"] [[2 "x"] 3]] ; y = 2x + 3
                ["<" ["x"] [4]] #_(x < 4)]]
    (pprint/pprint (q/solve-equations domain eqs2)))

  (println "\nSingle string syntax, long var names")
  (let [domain    {"xerox" [:range 0 20]
                   "yoyo"  [:range 0 20]}
        eqs2-strs (q/parse-equations "
          yoyo = 2 xerox + 3
          xerox < 4
          xerox != 2
          ")]
    (pprint/pprint (q/solve-equations domain eqs2-strs)))

  (println "\nA non-linear constraint")
  (let [domain {"x" [:range 0 20]
                "y" [:range 0 20]
                "z" [36]}
        ; NOTE: Not "2xy = z" (Balance: Math domain, human syntax, composable and machine-readable syntax)
        eqs1   (q/parse-equations "2 x y = z")]
    (pprint/pprint (q/solve-equations domain eqs1))))

(defn basic-le-with-implies-and-only-if
  []
  @q/load-native
  (let [domain {"x"     [:range 0 5]
                "y"     [:range 0 10]
                "z"     [:range 0 10]
                "a"     [:boolean]
                "b"     [:boolean]
                "total" [:range 0 100]}]
    [["=" ["y"] [[2 "x"] 3]] ; y = 2x + 3
     ["<" ["x"] [4]] #_(x < 4)

     ; Example: only if
     ["=" ["z"] ["x"] :only-if "a"]
     '[= [z] [y] :only-if !a]

                        ; Example: implication
                        ["=>" ["a"] ["b"]]

                        ; Example: sum
                        ["=" ["total"] ["x" "y" "z"]]]))

;; MP Solver

;(defn basic-example-with-mp-solver
;  []
;  @load-native
;  ;; Create the linear solver with the GLOP backend.
;  (let [solver    (MPSolver/createSolver "GLOP")
;        ;; Define variables
;        x         (.makeNumVar solver 0.0 1.0 "x")
;        y         (.makeNumVar solver 0.0 2.0 "y")
;
;        ;; Create a linear constraint, 0 <= x + y <= 2.
;        ct        (doto (.makeConstraint solver 0.0 2.0 "ct")
;                    (.setCoefficient x 1)
;                    (.setCoefficient y 1))
;
;        ;; Create the objective function, 3 * x + y.
;        objective (doto (.objective solver)
;                    (.setCoefficient x 3)
;                    (.setCoefficient y 1)
;                    (.setMaximization))]
;    (println "Number of variables = " (.numVariables solver))
;    (println "Number of constraints = " (.numConstraints solver))
;    (.solve solver)
;    (println "Solution:\n" {:value (.value objective)
;                            :x     (.solutionValue x)
;                            :y     (.solutionValue y)})
;    {:objective objective
;     :variables {:x x
;                 :y y}}))
