(ns quandary.util
  (:require
   [com.rpl.specter :as sp]
   [taoensso.timbre :as timbre]
   [clojure.spec.alpha :as s]
   [clojure.set]
   [clojure.string :as string]
   [java-time.api :as jtime]))

(s/def ::equations sequential?)
(s/def ::domain map?)

(s/def ::collatable (s/keys :opt-un [::equations ::domain]))

(defn utc-now
  []
  (jtime/with-zone-same-instant (jtime/zoned-date-time) "UTC"))

(defn dot
  "Join the strings with dots (\".\")"
  ([v0 v1 & more]
   (dot (concat [v0 v1] more)))
  ([values]
   (if (string? values)
     (dot [values])
     (if (= (last values) :temp)
       (if (re-find #"^TEMP" (first values))
         (string/join "." (butlast values))
         (string/join "." (cons "TEMP" (butlast values))))
       (string/join "." values)))))

(defonce ^:dynamic *NEXT_ID* nil)

(defn next-id
  "Append the next ID (a sequential four digit numeric) String onto String `s`.
  *NEXT_ID* must be bound in the local thread context to an atom with a long value, e.g.:
  (binding [qutil/*NEXT_ID* (atom 0)]
    (next-id \"foo\") ...)"
  ([s]
   (let [current @*NEXT_ID*]
     (swap! *NEXT_ID* inc)
     (format "%s%04d" s current)))
  ([] (next-id "")))

(defn merge-with-using-key
  ;; Copied from clojure.core/merge-with, but include the key in `f` call.
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by calling (f key val-in-result val-in-latter)."
  {:added  "1.0"
   :static true}
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
                        (let [k (key e) v (val e)]
                          (if (contains? m k)
                            (assoc m k (f k (get m k) v))
                            (assoc m k v))))
          merge2 (fn [m1 m2]
                   (reduce merge-entry (or m1 {}) (seq m2)))]
      (reduce merge2 maps))))

(defmulti collate-merge
  "Given a collection of Maps, collate the domain and equations and (given strategies
          via defmethod) other keys into a single Map."
  (fn [k vleft vright] k))

(defmethod collate-merge :domain
  [_ vleft vright]
  (merge-with-using-key
   (fn [k v1 v2] (throw (ex-info "Duplicate domain key" {:k k :v1 v1 :v2 v2})))
   vleft vright))

(defmethod collate-merge :equations
  [_ vleft vright]
  (concat (remove nil? vleft) (remove nil? vright)))

(defmethod collate-merge :assumptions
  [_ vleft vright]
  (concat (remove nil? vleft) (remove nil? vright)))

(defmethod collate-merge :default
  [k vleft vright]
  (throw (ex-info "Unknown key in collate-merge. Extend collate-merge with defmethod to support this key."
                  {:k k :vleft vleft :vright vright})))

(defn- tag-collatable
  "Transforms a collatable map's :domain values and :equations to carry metadata m."
  [m x]
  (cond-> x
    (:domain x) (update :domain update-vals #(with-meta % m))
    (:equations x) (update :equations (partial mapv #(with-meta % m)))))

(defn collate
  "Given a collection of Maps, collate the domain and equations and \"other\" keys into a single Map.
  The \"other\" keys are merged using `collate-merge`, which can be extended with defmethod.
  When input maps have metadata, that metadata is stored on the objects within the collation."
  [& xs]
  (loop [acc {}
         x (first xs)
         more (rest xs)]
    (cond
      (map? x)
      (let [new-acc (merge-with-using-key
                     collate-merge acc (if-let [m (meta x)] (tag-collatable m x) x))]
        (if (empty? more)
          new-acc
          (recur new-acc (first more) (rest more))))
      (and (empty? x) (seq more))
      (recur acc (first more) (rest more))
      (empty? x)
      acc
      (coll? x)
      (recur acc (first x) (concat (rest x) more))
      :else
      (throw (ex-info (format "collate accepts Maps and collections of Map, not %s" (type x))
                      {:item x})))))

(defn- vars-for-eq
  [eq]
  (->> eq
       rest
       flatten
       (filter string?)
       (map #(if (re-find #"^!" %) (subs % 1) %))
       set))

(defn- map-vars-to-eqs
  [x]
  (if (map? x)
    (map-vars-to-eqs (:equations x))
    (-> x
        (->> (mapcat (fn [eq] (for [varname (vars-for-eq eq)] [varname eq])))
             (group-by first))
        (update-vals (partial map second)))))

(defn examine-equations
  [equations]
  ;; Then try
  ;; (let [vname "wall.a.S.3.C.1.width"] [(get domain vname) (get ee vname)])
  (->> equations
       map-vars-to-eqs
       (remove #(re-find #"^TEMP.wall.a.S.\d.C.\d.(right|left).cs.\d+" (key %)))
       (remove #(re-find #"^scorer\." (key %)))
       (into {})))

(defn parse-cvar
  [s]
  (if-let [[_ prefix wallid slot cidx suffix]
           (and s (re-matches #"(wall\.([a-z])\.S\.(\d)\.C\.(\d))\.(.*)" s))]
    [prefix wallid slot cidx suffix]))

(defn ordered-domain?
  "A validation function to test if domain is ordered. An ordered domain is not a requirement,
  but is helpful for debugging and determinism."
  [context rules]
  (let [ids (->> (:domain rules)
                 keys
                 (keep (fn [k] () (re-find #"\d{4,}$" k)))
                 vec)
        ordered? (and (zero? (Integer/parseInt (first ids)))
                      (= (dec (count ids)) (Integer/parseInt (last ids))))]
    (when-not ordered?
      (timbre/warn "Domain is not ordered"
                   (merge context
                          {:count (count ids)
                           :range [(first ids) (last ids)]
                           :ids   ids})))
    ordered?))

(defn var-name-from
  [obj suffix & [temp?]]
  (if temp?
    (dot "TEMP" (:quandary.quandary/var-name-prefix obj) suffix)
    (dot (:quandary.quandary/var-name-prefix obj) suffix)))

(defmacro dollar-vars
  "A macro to make varname references easy.  Will walk the form and replace any symbols of the form
  `$foo.x` with the varname string for x from object foo.  (i.e. appending x to the `var-name-prefix` from obj)."
  [& body]
  (let [form2 (sp/transform [(sp/walker #(and (symbol? %) (.startsWith (name %) "$")))]
                            #(let [[_ obj varname] (re-matches #"\$([a-zA-Z0-9-_]+)\.([a-zA-Z0-9-_]+)" (name %))]
                               (list `var-name-from (symbol obj) varname))
                            `(collate ~@body))]
    `~form2))
