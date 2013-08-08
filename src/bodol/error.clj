(ns bodol.error
  (:require [bodol.types :as t]))

(defn error [scope sexp type msg]
  (ex-info msg {:scope scope
                :sexp sexp
                :pos (t/-pos sexp)
                :type type}))

(defn sexpless-error [scope pos type msg]
  (ex-info msg {:scope scope
                :sexp nil
                :pos pos
                :type type}))

(defn raise [scope sexp type msg]
  (throw (error scope sexp type msg)))

(defn sexpless-raise [scope pos type msg]
  (throw (sexpless-error scope pos type msg)))

(defn- report-pos [pos]
  (str
   (if-let [location (:location pos)]
     (str "At \"" location "\" line ") "At line ")
   (:span pos) "\n"))

(defn report [error]
  (let [{:keys [scope sexp pos type]} (ex-data error)
        msg (.getMessage error)]
    (str
     (report-pos pos)
     "Error " type "\n" msg "\n")))

(defn error-type [error]
  (when (instance? clojure.lang.ExceptionInfo error)
    (:type (ex-data error))))

(defn error? [error]
  (not (nil? (error-type error))))

(defmacro do-catch [& body]
  `(try
     ~@body
     (catch clojure.lang.ExceptionInfo e# e#)))
