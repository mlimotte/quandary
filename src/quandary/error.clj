(ns quandary.error
  (:require [clojure.tools.logging :as log])
  (:import
   (clojure.lang ExceptionInfo)))

(def ^{:dynamic true} *wrap-ex-info-context* {})

(defmacro wrap-ex-info-context
  "This macro has two functions, achieved by wrapping some `body` of code:
  1. If an exception is thrown and uncaught, it will be caught here, the `context` values
     will be merged into the ex-data (if it is an ExceptionInfo), or just logged to ERROR
     (if not).

  2. For exceptions (or other scenarios) that are caught within the block and do not
     rise up to this scope, we don't see the exception and, therefore, can't adjust it.
     Instead, we provide a thread-local binding (which is recursively merged as we go down
     the stack). This binding (`*wrap-ex-info-context*`) can be accessed and utilized as
     desired within.

  REMINDER for #2: This is a thread-local binding so, of course, any parallel operations
  (e.g. pmap) will not see the bindings.
  "
  [context & body]
  `(try
     ;; Vars bound with binding are thread-local.
     (binding [*wrap-ex-info-context* (merge *wrap-ex-info-context* ~context)]
       ~@body)
     (catch ExceptionInfo e#
       (let [e2# (doto (ex-info (.getMessage e#)
                                (merge ~context (ex-data e#))
                                (.getCause e#))
                   (.setStackTrace (.getStackTrace e#)))]
         (throw e2#)))
     (catch Exception e#
       (log/error e# (str "wrap-ex-info-context caught non-ex-info exception "
                          (assoc ~context ::message (.getMessage e#))))
       (throw e#))))

(defmacro wrap-context-for-error-and-logging
  "Merges `context` into the ex-data of any exceptions thrown from `body`.
  An alias for `wrap-ex-info-context`; the name reflects its original intent of also
  enriching log messages (via timbre context), which is no longer in effect.
  NOTE: Uses of this macro are nestable."
  [context & body]
  `(wrap-ex-info-context ~context ~@body))
