(ns quandary.quandary
  "A wrapper for Google's OR-Tools for linear programming and satisfaction solver.
  OR-Tools has several solvers, we use the CP-SAT Solver: https://developers.google.com/optimization/cp/cp_solver

  A demonstration of a minimal Linear Programming example to showcase calling the solver.

  Some reference links:
    - Help on Integer logic with Or Tools (sample):
      https://git.xkool.org/hw/or-tools/-/blob/e59073c45f054867ef4c80c99db94d985c9b5679/ortools/sat/doc/integer_arithmetic.md
    - https://or-tools.github.io/docs/javadoc/index.html
      Look for `com.google.ortools.sat.CpModel`

  Set up local (Mac):
    (1) JDK and export JDK_HOME (or let \"jenv\" do it)
    export JAVA_HOME=/Library/Java/JavaVirtualMachines/openjdk.jdk

    (2) OR Tools binaries
    TODO: Confirm if this is necessary; or will maven do this for us?
    https://developers.google.com/optimization/install/java

    (3) Maven
    # brew install maven
    # mvn -v

    (4) Versions installed: 
    # https://search.maven.org/artifact/com.google.ortools/ortools-java/
  "
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clojure.set]
            [com.rpl.specter :as sp]
            [clojure.tools.logging :as log]
            [instaparse.core :as insta]
            [quandary.error :as error]
            [quandary.util :as qutil])
  (:import [java.lang Long]
           [com.google.ortools Loader]
           [com.google.ortools.sat CpModel
            CpSolver
            CpSolverSolutionCallback
            IntVar
            BoolVar
            Literal
            LinearExpr
            SatParameters$SearchBranching
            LinearArgument CpSolverStatus]
           [com.google.ortools.util Domain]))

(defonce load-native (delay (Loader/loadNativeLibraries)))

(defn nil-or-blank?
  "True if `s` is a nil or \"\".
  Note: string/blank on it's own will throw an exception for
  non-string values (e.g. numbers). This fn will just return false instead."
  [s]
  (or (nil? s)
      (and (string? s) (string/blank? s))))

(s/def ::non-blank-string? (complement nil-or-blank?))

(s/def ::domain-map (s/map-of string? any?))
(s/def ::domain-map-light map?)                             ; A lighter test, so we don't keep walking the entire map
(s/def ::model (partial instance? CpModel))
(s/def ::linear-arg (partial instance? LinearArgument))
(s/def ::literal-arg (partial instance? Literal))

(s/def ::var-name-prefix string?)

;; API

(def math-ceil #(long (Math/ceil %)))

;; Primitives

(defn lvar?
  [x]
  (or (instance? IntVar x)
      (instance? BoolVar x)))

(s/def ::varname string?)
(s/def ::varname-or-int (s/or :varname ::non-blank-string? :int int?))

(defn named? [x]
  (or (string? x) (instance? clojure.lang.Named x)))

(defn domain
  [& [v0 :as values]]
  (let [values_ (if (coll? v0) (apply concat values) values)
        typ (cond
              (instance? Long (first values_)) Long/TYPE
              (instance? Integer (first values_)) Long/TYPE
              :else (throw (ex-info "Unrecognized type" {:values_ values_
                                                         :v0type  (-> values_ first type)})))]
    (Domain/fromValues (into-array typ values_))))

(defn strip-temp-vars
  "Remove map entries where the key starts with \"TEMP.\".
   Works on a map or a seq of maps."
  [x]
  (cond
    (map? x)
    (into (empty x) (remove #(and (string? (key %)) (.startsWith (key %) "TEMP.")) x))
    (sequential? x)
    (mapv strip-temp-vars x)))

(defn as-2d-int-array
  [tuples]
  (object-array (mapv int-array tuples)))

;;;;;;;

(defn mk-solutions-callback
  [state-ref
   int-vars
   {:keys [name-value]
    :or   {name-value true}
    :as   options}]
  (proxy
   [CpSolverSolutionCallback]
   []
    (onSolutionCallback []
      (swap! state-ref update :solutions conj
             {:objective    (.objectiveValue this)
              :wall-time    (.wallTime this)
              :current-time (qutil/utc-now)
              :values       (walk/prewalk
                             (fn [v] (cond (and (lvar? v) name-value)
                                           [(.getName v) (.value this v)]
                                           (lvar? v)
                                           (.value this v)
                                           :else
                                           v))
                             int-vars)}))))

(defrecord SolverWrapper [solver callback solved-state value-fn])

(defn solver-with-callback
  [vars
   {:keys [timeout relative-gap-limit num-workers enumerate-all]
    :or   {timeout     20.0
           num-workers 0}
    :as   options}]
  (log/info (format "Configuring solver with timeout %s" timeout))
  ;; Java doc with the list of parameters:
  ;; https://or-tools.github.io/docs/javadoc/com/google/ortools/sat/SatParameters.Builder.html

  (let [solver (doto (CpSolver.)
                 (-> .getParameters

                     ;(.setLogSearchProgress true)

                     (cond->
                      timeout
                       (.setMaxTimeInSeconds timeout)

                       ;; EnumerateAllSolutions is NOT supported with parallel
                       ;;  "Check failed: !params.enumerate_all_solutions() Enumerating all solutions in parallel is not supported."
                       enumerate-all
                       (.setEnumerateAllSolutions enumerate-all)

                       ;; Within a percentage of optimal
                       relative-gap-limit
                       (.setRelativeGapLimit (float relative-gap-limit))

                       ;; Default is 0 (use all cores). not compatible with EnumerateAllSolutions.
                       num-workers
                       (.setNumWorkers num-workers))))
        solved-state (atom {})
        cb (mk-solutions-callback solved-state vars options)]
    (->SolverWrapper solver cb solved-state #(.value solver %))))

(defn solve!
  "Establish the JNI wrappers first with the Delay\"
    `@load-native`
  "
  [solver-wrapper model]
  (.solve (:solver solver-wrapper) model (:callback solver-wrapper)))

(def var-name-from qutil/var-name-from)

(def eq-grammar
  "
  <START> = (<NL_OR_WS> EQ <NL_OR_WS>)*
  <NEWLINE> = '\n'
  <S> = #'[ \\t\\r\\f]*'?
  <NL_OR_WS> = (<S>|<NEWLINE>)*
  NUMBER = #'-?[0-9]+'
  <VAR> = #'[a-zA-Z_]([a-zA-Z0-9_-]|\\?)*'

  <EOP> = #'=|<=|>=|<|>|!='
  <OP> = PLUS | MINUS
  PLUS = '+'
  MINUS = '-'

  EQ = EXPR <S> EOP <S> EXPR
  EXPR = TERM (<S> OP <S> TERM)*
  <MULT> = S '*'? S
  TERM = (MINUS? VAR <MULT> VAR) / (MINUS? VAR) / (NUMBER <MULT> VAR) / (NUMBER <MULT> VAR <MULT> VAR) / NUMBER
  ")

(defn negative-term
  ([t1]
   [(* -1 t1)])
  ([t1 t2]
   (if (number? t1)
     [(* -1 t1) t2]
     [-1 t1 t2]))
  ([n1 t1 t2]
   [(* -1 n1) t1 t2]))

(def the-parser (insta/parser eq-grammar))

(defn parse-equations
  [& args]
  ; NOTE: For the output, I am attempting to strike a balance between something
  ;  that is human-readable and writeable and also machine-readable.
  (let [s (string/join "\n" args)
        ret (insta/transform
             {:NUMBER #(Integer/parseInt %)
              :EQ     (fn [l eop r]
                         ; Return lists to make the output form more readble
                        [eop (apply list l) (apply list r)])
              :PLUS   (constantly :PLUS)
              :MINUS  (constantly :MINUS)
              :EXPR   (fn [& args]
                        (first
                         (reduce (fn [[acc minus?] term]
                                   (cond (= term :PLUS)
                                         [acc false]
                                         (= term :MINUS)
                                         [acc true]
                                         minus?
                                         [(conj acc (apply negative-term term)) false]
                                         :else
                                         [(conj acc term) false]))
                                 [[] false]
                                 args)))
              :TERM   (fn
                        ([t1]
                         [t1])
                        ([t1 t2]
                         (cond
                           (= t1 :MINUS)
                           [-1 t2]
                           :else
                           [t1 t2]))
                        ([t1 t2 t3]
                         (cond
                           (= t1 :MINUS)
                           [-1 t2 t3]
                           (number? t1)
                           [t1 t2 t3]
                           :else
                           (throw (ex-info "Parse failure, For a term triple, the first element must be a number"
                                           {:t1 t1 :t2 t2 :t3 t3})))))}
             (the-parser s))]
    (if (insta/failure? ret)
      (throw (ex-info "Parse failed" {:string s :failure ret}))
      ret)))

(defn mk-domain-map
  "Given a model and a domain spec, create the domain variables in the model and return a
  same-shaped map of Vars.
  A domain spec looks like this, for example:
    {\"x\" [:range 0 20]
     \"y\" [:boolean]
     \"z\" [9 10 11 12 13]}
  In the `:range` case, the second value is inclusive."
  [model m]
  (let [kvs (doall
             (mapcat
              (fn [[varname-or-names domain-spec]]
                (error/wrap-ex-info-context
                 {:varname varname-or-names :domain-spec domain-spec}

                 (if (and (sequential? domain-spec) (= (first domain-spec) :tuples))

                      ;; addAllowedAssignments
                      ;; See https://developers.google.com/optimization/assignment/assignment_groups
                   (let [[_ tuples] domain-spec]
                     (when-not (and (sequential? varname-or-names) (sequential? tuples))
                       (throw (ex-info "For domain `:tuples` varname-or-names must be a seq of string and domain spec must have a seq of Ints"
                                       {:tuples tuples :varnames varname-or-names})))
                     (when (some #(not= (count varname-or-names) (count %))
                                 tuples)
                       (throw (ex-info "length of varnames must match length of each tuple" {})))
                     (let [kvs2 (doall (map-indexed
                                        (fn [i varname2]
                                          [varname2 (.newIntVarFromDomain model
                                                                          (apply domain (mapv #(nth % i) tuples))
                                                                          varname2)])
                                        varname-or-names))
                           tbl-constraint (.addAllowedAssignments model (mapv second kvs2))]
                          ;// Define the allowed groups of workers
                          ;model.addAllowedAssignments(new IntVar[] {work[0], work[1], work[2], work[3]})
                          ;.addTuples(group1);
                       (doseq [t tuples]
                         (.addTuple tbl-constraint (int-array t)))
                       kvs2))

                      ;; other
                   [[varname-or-names
                     (cond (= domain-spec [:boolean])
                           (.newBoolVar model varname-or-names)

                           (= (first domain-spec) :range)
                           (.newIntVar model (nth domain-spec 1) (nth domain-spec 2) varname-or-names)

                           (= (first domain-spec) :fixed-size-interval)
                           (fn [processed-domain-map]
                             (let [interval-start-varname (nth domain-spec 1)
                                   intv-start (get processed-domain-map interval-start-varname)
                                   n (nth domain-spec 2)]
                               (when-not (instance? LinearArgument intv-start)
                                 (throw (ex-info "A LinearArgument is required" {:type (type intv-start)
                                                                                 :x    (str intv-start)})))
                               (when-not (int? n)
                                 (throw (ex-info "Second spec arg must be a n int" {:type (type n)
                                                                                    :n    (str n)})))
                               (.newFixedSizeIntervalVar model intv-start (long n) varname-or-names)))

                           (= (first domain-spec) :optional-fixed-size-interval)
                           (fn [processed-domain-map]
                                ;; [:optional-fixed-size-interval varStart varIsPresent intSize]
                             (let [interval-start-varname (nth domain-spec 1)
                                   intv-start (get processed-domain-map interval-start-varname)

                                   is-present-varname (nth domain-spec 2)
                                   is-present (get processed-domain-map is-present-varname)

                                   size (nth domain-spec 3)]
                               (when-not (instance? LinearArgument intv-start)
                                 (throw (ex-info "A LinearArgument is required" {:type (type intv-start)
                                                                                 :x    (str intv-start)})))
                               (when-not (int? size)
                                 (throw (ex-info "Second spec arg must be a size int" {:type (type size)
                                                                                       :n    (str size)})))
                               (.newOptionalFixedSizeIntervalVar model intv-start (long size) is-present varname-or-names)))

                           (= (count domain-spec) 1)
                           (.newConstant model (first domain-spec))

                           :else
                           (.newIntVarFromDomain model (apply domain domain-spec) varname-or-names))]])))

              m))
        m2 (into (empty m) kvs)]
    (sp/transform [(sp/walker fn?)] #(% m2) m2)))

(defn- new-int-var-domain-from-mult
  [model v1 v2]
  (let [d1min (-> v1 .getDomain .min)
        d1max (-> v1 .getDomain .max)
        d2min (-> v2 .getDomain .min)
        d2max (-> v2 .getDomain .max)
        ;; Need to consider all 4 cases, because one value might be negative.
        extremes (vector (* d1min d2min)
                         (* d1min d2max)
                         (* d1max d2min)
                         (* d1max d2max))
        t-min (apply min extremes)
        t-max (apply max extremes)
        varname (qutil/dot (.getName v1) (.getName v2) (name (gensym "")) :temp)]
    (.newIntVar model t-min t-max varname)))

(s/fdef eval-in-domain
  :args (s/cat :domain-map ::domain-map :key any?)
  :ret lvar?)
(defn eval-in-domain
  [domain-map key]
  (cond
    (named? key) (let [k (name key)]
                   (or (get domain-map k)
                       (throw (ex-info (format "No var found in domain for \"%s\" " k)
                                       (cond-> {:key k}
                                         (>= (count k) 5)
                                         (assoc :suggestions (filter #(.contains % k) (keys domain-map))))))))
    (nil? key) (throw (ex-info "eval-in-domain with a nil key" {}))
    (number? key) (long key)))

(defn- maybe-numeric-arg
  [x]
  (cond
    (number? x) (long x)
    (and (sequential? x) (= (count x) 1) (number? (first x))) (long (first x))
    :else nil))

(defn int-var-arg
  [domain-map polynomial]
  (when (not= (count polynomial) 1)
    (throw (ex-info "polynomial must have 1 element" {:polynomial polynomial})))
  (eval-in-domain domain-map (first polynomial)))

(defn long-arg
  [polynomial]
  (when (not= (count polynomial) 1)
    (throw (ex-info "polynomial must have 1 element" {:polynomial polynomial})))
  (long (first polynomial)))

(s/fdef linear-arg-or-long
  :args (s/cat :model ::model :domain-map ::domain-map-light :polynomial (s/and sequential? seq))
  :ret ::linear-arg)
;(defn- linear-arg-or-long
;  "Return a LinearArgument.  Numeric terms are wrapped in a LinearExpression, so
;  you can use `[2]` as an arg where the signature expects a LinearArgument."
;  ;; TODO It would be cleaner to convert longs with .newConstant instead of wrapping in an expr
;  [model domain-map polynomial]
;  (when-not (seq polynomial)
;    (throw (ex-info "No terms supplied for this linear-argument" {})))
;  ; `polynomial` is a sequence of `terms`, each term can be (for example):
;  ;  [1 "y"]
;  ;  ["y"]
;  ;  [2 "x"]
;  ;  [3 "x" "y"]
;  ;  [3]
;  (let [expr (LinearExpr/newBuilder)]
;    (doseq [term-entry polynomial
;            :let [[t1 t2 t3 :as term] (if (sequential? term-entry)
;                                        term-entry
;                                        [term-entry])]]
;
;      ;; We don't use `LinearExpr/sum` because it's implementation is just a loop that adds each
;      ;; term one at a time to the expression (it's more straightforward to just do that ourselves)
;
;      (case (count term)
;        0
;        (timbre/warn "Empty term in polynomial" {:polynomial polynomial})
;
;        1
;        (.add expr (eval-in-domain domain-map t1))
;
;        2
;        (cond
;          (every? string? term)
;          (let [tmp (new-int-var-domain-from-mult
;                      model (eval-in-domain domain-map t1) (eval-in-domain domain-map t2))]
;            (.addMultiplicationEquality
;              model tmp (eval-in-domain domain-map t1) (eval-in-domain domain-map t2))
;            (.add expr tmp))
;          (and (number? t1) (string? t2))
;          (.addTerm expr (eval-in-domain domain-map t2) (long t1))
;          (and (string? t1) (number? t2))
;          (.addTerm expr (eval-in-domain domain-map t1) (long t2))
;          :else
;          (throw (ex-info "Invalid term.  A 2 arg term must have at least one var (String)"
;                          {:t1 t1 :t2 t2})))
;
;        3                                                   ; E.g.  [3 "x" "y"]
;        (do
;          (when-not (and (number? t1)
;                         (named? t2)
;                         (named? t3))
;            (throw (ex-info "Invalid term.  A 3 arg term, must be [NUMBER STRING STRING]"
;                            {:t1 t1 :t2 t2 :t3 t3})))
;          (let [tmp (new-int-var-domain-from-mult
;                      model (eval-in-domain domain-map t2) (eval-in-domain domain-map t3))]
;            (.addMultiplicationEquality
;              model tmp (eval-in-domain domain-map t2) (eval-in-domain domain-map t3))
;            (.addTerm expr tmp t1)))
;
;        ;; else
;        ;;  E.g. [3 "x" "y" "z"]
;        ;;  E.g. ["a" "b" "c"]
;
;        ))
;
;    expr))

(defn add-multiplication-factors!
  "Adds all the factors using .addMultiplicationEquality and creating tmp vars as needed."
  [model domain-map string-factors]
  (reduce (fn [qvar t]
            (let [qvar' (if (string? qvar)
                          (eval-in-domain domain-map t)
                          qvar)
                  tvar (eval-in-domain domain-map t)
                  tmp (new-int-var-domain-from-mult model qvar' tvar)]
              (.addMultiplicationEquality model tmp qvar' tvar)
              tmp))
          string-factors))

(defn validate-rest-strings!
  [term]
  (when-not (every? string? (rest term))
    (throw (ex-info "In a term with 3 or more factors, first can be a numeric, rest must be String." {}))))

(defn- linear-arg-or-long
  "Return a LinearArgument.  Numeric terms are wrapped in a LinearExpression, so
  you can use `[2]` as an arg where the signature expects a LinearArgument."
  [model domain-map polynomial]
  (when-not (seq polynomial)
    (throw (ex-info "No terms supplied for this linear-argument" {})))
  ; `polynomial` is a sequence of `terms`, each term can be (for example):
  ;  [1 "y"]
  ;  ["y"]
  ;  [2 "x"]
  ;  [3 "x" "y"]
  ;  [3]
  (let [expr (LinearExpr/newBuilder)]
    (doseq [term-entry polynomial
            :let [[t1 t2 :as term] (if (sequential? term-entry)
                                     term-entry
                                     [term-entry])
                  term-types (mapv (fn [factor]
                                     (cond (string? factor) :str
                                           (number? factor) :num
                                           :else (throw (ex-info "Unexpected factor type"
                                                                 {:factor factor :term term :type (type factor)}))))
                                   term)]]
      (error/wrap-context-for-error-and-logging
       {:polynomial polynomial :term term :term-types term-types}

        ;; We don't use `LinearExpr/sum` because it's implementation is just a loop that adds each
        ;; term one at a time to the expression (it's more straightforward to just do that ourselves)

       (case (take 3 term-types)

         []
         (log/warn "Empty term in polynomial")

         [:num]
         (.add expr (.newConstant model t1))

         [:str]
         (.add expr (eval-in-domain domain-map t1))

         [:num :str]
         (.addTerm expr (eval-in-domain domain-map t2) (long t1))

         [:str :num]                                       ;; DEPRECATE - use impl/simplify-polynomial to normalize
         (.addTerm expr (eval-in-domain domain-map t1) (long t2))

         [:str :str]
         (let [tmp (new-int-var-domain-from-mult
                    model (eval-in-domain domain-map t1) (eval-in-domain domain-map t2))]
           (.addMultiplicationEquality
            model tmp (eval-in-domain domain-map t1) (eval-in-domain domain-map t2))
           (.add expr tmp))

          ;; 3 or more terms...
         [:num :str :str]
         (let [_ (validate-rest-strings! term)
               tmp (add-multiplication-factors! model domain-map (rest term))]
           (.addTerm expr tmp t1))

          ;; 3 or more terms...
         [:str :str :str]
         (let [_ (validate-rest-strings! term)
               tmp (add-multiplication-factors! model domain-map (rest term))]
           (.add expr tmp))

         (throw (ex-info "Invalid term in linear-arg-or-long. See s.q.impl/simplify-polynomial." {})))))

    expr))

(defn- linear-arg
  [model domain-map polynomial]
  (if-let [narg (maybe-numeric-arg polynomial)]
    (.newConstant model narg)
    (linear-arg-or-long model domain-map polynomial)))

(defn- interval-arg
  [domain-map arg]
  (eval-in-domain domain-map arg))

(s/fdef literal-arg
  :args (s/cat :domain-map ::domain-map-light :arg any?))
(defn- literal-arg
  [domain-map arg]
  (let [[t1] (if (sequential? arg)
               arg
               [arg])]
    (cond (number? t1)
          (long t1)
          (.startsWith (name t1) "!")
          (.not (eval-in-domain domain-map (subs (name t1) 1)))
          :else
          ^Literal (eval-in-domain domain-map t1))))

(s/fdef !
  :args (s/cat :s (s/and string? seq)))
(defn !
  "Given a string variable, return the negated equivalent"
  [s]
  (cond
    (string/blank? s) (throw (ex-info "Negation on empty variable name" {}))
    (.startsWith s "!") (subs s 1)
    :else (str "!" s)))

(defn divide-seq-by
  "Split a sequence into multiple sequences, starting a new one each time `(pred item)`
   is truthy. The `item` is included in the new seq."
  [pred sequence]
  (reduce (fn [acc item]
            (if (pred item)
              (conj acc [item])
              (-> acc butlast vec (conj (conj (vec (last acc)) item)))))
          []
          sequence))

(defmulti apply-modifier
  (fn [domain-map constraint modifier] (first modifier)))

(defmethod apply-modifier :only-if
  [domain-map constraint [_ q1]]
  (.onlyEnforceIf constraint (literal-arg domain-map [q1])))

(defn eop-<=
  [model linarg lin-long-arg x y & [z]]

  ;(eop-<= model linarg lin-long-arg y x)
  ;(eop-<= model linarg lin-long-arg ["a"] [11])

  (cond
    (and (nil? z) (maybe-numeric-arg x))
    (.addGreaterOrEqual model (linarg y) (maybe-numeric-arg x))

    (nil? z)
    (.addLessOrEqual model (linarg x) (lin-long-arg y))

    (and (maybe-numeric-arg x) (maybe-numeric-arg z))
    (.addLinearConstraint model (linarg y) (maybe-numeric-arg x) (maybe-numeric-arg z))

    (maybe-numeric-arg x)
    [(.addGreaterOrEqual model (linarg y) (maybe-numeric-arg x))
     (.addLessOrEqual model (linarg y) (lin-long-arg z))]

    (maybe-numeric-arg y)
    [(.addLessOrEqual model (linarg x) (maybe-numeric-arg y))
     (.addGreaterOrEqual model (linarg z) (maybe-numeric-arg y))]

    :else
    [(.addLessOrEqual model (linarg x) (lin-long-arg y))
     (.addLessOrEqual model (linarg y) (lin-long-arg z))]))

(defn eop-<
  [model linarg lin-long-arg x y & [z]]
  (cond
    (and (nil? z) (maybe-numeric-arg x))
    (.addGreaterThan model (linarg y) (maybe-numeric-arg x))

    (nil? z)
    (.addLessThan model (linarg x) (lin-long-arg y))

    (and (maybe-numeric-arg x) (maybe-numeric-arg z))
    (.addLinearConstraint model (linarg y) (dec (maybe-numeric-arg x)) (inc (maybe-numeric-arg z)))

    (maybe-numeric-arg x)
    [(.addGreaterThan model (linarg y) (maybe-numeric-arg x))
     (.addLessThan model (linarg y) (lin-long-arg z))]

    (maybe-numeric-arg y)
    [(.addLessThan model (linarg x) (maybe-numeric-arg y))
     (.addGreaterThan model (linarg z) (maybe-numeric-arg y))]

    :else
    [(.addLessThan model (linarg x) (lin-long-arg y))
     (.addLessThan model (linarg y) (lin-long-arg z))]))

(defn submit-equations
  "Processes equations data structure.
  For example:
    ([\"=\" ([2 \"x\"] [4 \"y\"]) ([124])] ; 2x + 4y = 124
     [\"<\" ([1 \"y\"]) ([4])]
     [\"<\" ([1 \"x\"] [1 \"y\"]) ([40])] )

  I.e. multiplication and addition/subtraction are implicit with \"=\"/.
  "
  [model domain-map equations]
  (let [linarg (partial linear-arg model domain-map)
        intvarg (partial interval-arg domain-map)
        lin-long-arg (partial linear-arg-or-long model domain-map)
        litarg (partial literal-arg domain-map)
        ;; Note operators like `*` and `+` are not here. Instead use `=` and tuples:
        ;; ["=" [1 2 3] x]   ; x = 1 + 2 + 3
        ;; ["=" [[5 a] x]    ; x = 5 * a
        eop-fns {"="                     #(.addEquality model (linarg %1) (lin-long-arg %2))
                 "!="                    #(.addDifferent model (linarg %1) (lin-long-arg %2))
                 "<="                    (partial eop-<= model linarg lin-long-arg)
                 "<"                     (partial eop-< model linarg lin-long-arg)
                 ">="                    (fn [x y & [z]]
                                           (if (some? z)
                                             (eop-<= model linarg lin-long-arg z y x)
                                             (eop-<= model linarg lin-long-arg y x)))
                 ">"                     (fn [x y & [z]]
                                           (if (some? z)
                                             (eop-< model linarg lin-long-arg z y x)
                                             (eop-< model linarg lin-long-arg y x)))
                 "=>"                    #(.addImplication model (litarg %1) (litarg %2))
                 "at-least-one"          (fn at-least-one [left] (.addAtLeastOne model (mapv litarg left)))

                 "add-bool-or"           (fn add-bool-or [left] (.addBoolOr model (mapv litarg left)))
                 "add-bool-xor"          (fn add-bool-xor [left] (.addBoolXor model (mapv litarg left)))

                 "add-bool-and"          (fn add-bool-and [left] (.addBoolAnd model (mapv litarg left)))
                 "add-hint"              #(.addHint model (int-var-arg domain-map %1) (long-arg %2))
                 "add-max-equality"      (fn [target & other]
                                           (.addMaxEquality model (linarg target) (mapv linarg other)))
                 "add-min-equality"      (fn [target & other]
                                           (.addMinEquality model (linarg target) (mapv linarg other)))
                 "add-abs-equality"      (fn [target right]
                                           (.addAbsEquality model (linarg target) (linarg right)))
                 "add-modulo-equality"   (fn [target var mod]
                                           (.addModuloEquality
                                            model
                                            (linarg target) (linarg var) (linarg mod)))
                 ;; WARNING: Don't use `add-division-equality` with `:only-if`. It will never find solutions or
                 ;;   report CP-Sat model is invalid. only-if works for "linear" and basic logic constraints.
                 ;;   add-division-equality is non-linear.
                 "add-division-equality" (fn [target num denom]
                                           (.addDivisionEquality ; "rounded toward 0"
                                            model
                                            (linarg target) (linarg num) (linarg denom)))

                 "no-overlap-2d"         (fn [rectangles]
                                           (let [c (.addNoOverlap2D model)]
                                             (doseq [[x-interval y-interval] rectangles]
                                               (.addRectangle c (intvarg x-interval) (intvarg y-interval)))
                                             c))}]

    (doseq [eq equations]
      (error/wrap-ex-info-context
       {:eq eq}
       (let [[stuff & modifiers] (divide-seq-by keyword? eq)
             [eop & args] stuff
             eop-fn (get eop-fns (name eop))
             constraints (as-> (apply eop-fn args) $ (if (sequential? $) $ [$]))]
         (doseq [modifier modifiers
                 constraint constraints]
           (apply-modifier domain-map constraint modifier)))))))

(defn domain-vs-equations-inbalance?
  ([collation]
   (domain-vs-equations-inbalance? (:domain collation) (:equations collation) (:assumptions collation)))
  ([domain equations assumptions]
   (let [domain-vars (->> domain keys flatten set)
         lhs-domain-vars (sp/select [sp/MAP-VALS vector? sp/ALL string?] domain)
         domain-vars-single-value (->> domain
                                       (filter (fn [[k v]] (and (sequential? v)
                                                                (= (count v) 1)
                                                                (number? (first v)))))
                                       keys
                                       set)
         assumption-vars (set assumptions)
         eq-vars (->> equations
                      (map rest)
                      flatten
                      (filter string?)
                      (map #(if (re-find #"^!" %) (subs % 1) %))
                      set)
         unused-domain (clojure.set/difference domain-vars
                                               lhs-domain-vars
                                               domain-vars-single-value
                                               eq-vars
                                               assumption-vars)
         missing-domain (clojure.set/difference eq-vars domain-vars)]
     (if (or (seq unused-domain) (seq missing-domain))
       {:unused-domain  unused-domain
        :missing-domain missing-domain}))))

(defn solve-equations
  "Solve the set of equations and return all/some solutions.
  domain is a Map of variables names to domains. E.g.
    {\"x\" [:range 0 20]
     \"y\" [:range 0 20]
     \"z\" [9 10 11 12 13]

     ;; Tuple constraint: key is a vector of variable names, value is [:tuples <allowed-assignments>]
     ;; where <allowed-assignments> is a seq of integer vectors, each the same length as the key.
     ;; Constrains the named variables to only take on the listed combinations of values
     ;; (uses OR-Tools addAllowedAssignments). E.g.:
     ;;   {[\"a\" \"b\"] [:tuples [[1 2] [3 4] [5 6]]]}
     ;; means (a=1,b=2) or (a=3,b=4) or (a=5,b=6) are the only allowed assignments.
     }
  equations are of the form:
    eq = [operator left right]
    operator = (= | < | > | != | <= | etc)
    left = polynomial
    right = polynomial
    polynomial = [term+]
    term = varname | integer | [integer varname] | [integer varname varname]

  options
    In addition to the options defined in this fn signature, options is also passed to
    `solver-with-callback`.
  "
  [domain equations &
   [{[objective-instruction objective-varname] :objective
     :keys                                     [model-fn retain-temp? limit assumptions
                                                check-inbalance? load-native? export-pb-to-file]
     :or                                       {check-inbalance? true
                                                load-native?     true}
     :as                                       options}]]
  ;; This is different from the python and C# approaches that accept symbolic equations
  ;; Which include reified variable types (e.g. IntVars that have already been
  ;; created with their respective domains).
  (when load-native?
    @load-native)
  (when-let [problems (and check-inbalance?
                           (domain-vs-equations-inbalance? domain equations assumptions))]
    (throw (ex-info (str "Your domain is either missing variables or has unused variables. "
                         "Unconstrained variables will make the solver run slow")
                    problems)))
  (let [model (CpModel.)
        domain-map (mk-domain-map model domain)
        objective-var (if objective-instruction (get domain-map objective-varname))
        wrapper (solver-with-callback
                 (cond-> domain-map
                   (not retain-temp?)
                   strip-temp-vars)
                 (assoc options
                        :name-value false))]

    (when objective-instruction
      (case objective-instruction
        :minimize (.minimize model objective-var)
        :maximize (.maximize model objective-var)))

    (when (seq assumptions)
      (let [ary (into-array Literal (map (partial literal-arg domain-map) assumptions))]
        (.addAssumptions model ary)))

    ;; Set up the model
    (submit-equations model domain-map equations)

    ;; Apply optional model mutations
    (if model-fn
      (model-fn domain-map model))

    (when export-pb-to-file
      (.exportToFile model export-pb-to-file))

    ;; Run it and return results
    (log/info (str "Running solver " {:options         options
                                      :domain-count    (count domain)
                                      :equations-count (count equations)}))
    ;; Use `(.name status)`, for example
    (let [status (solve! wrapper model)]
      (let [solutions (-> wrapper :solved-state deref :solutions)]
        ;; Throw on invalid model
        (when (= status CpSolverStatus/MODEL_INVALID)
          (throw (ex-info "The CP-Sat model is invalid" {})))
        ;; Return solution values
        (with-meta (-> solutions
                       ;; TODO a better way to implement `limit` would be to stop the solver in `all-solutions-solver-with-callback`
                       (cond->> (and limit (pos? limit)) (take limit))
                       (->> (map (fn [solution]
                                   (with-meta (:values solution)
                                     {::wall-time    (:wall-time solution)
                                      ::current-time (:current-time solution)})))))
          {::status status})))))
