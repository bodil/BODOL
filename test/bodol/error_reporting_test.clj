(ns bodol.error-reporting-test
  (:require [clojure.test :as test :refer [deftest is]]
            [bodol.repl :as repl]
            [bodol.parser :as parser]
            [bodol.scope :as scope]
            [bodol.error :refer [do-catch error? error-type]]))

(defmacro run [forms & body]
  `(let ~(apply vector
                (mapcat (fn [[binding code]]
                          [binding `(do-catch (repl/eval ~code))])
                        (partition 2 forms)))
     ~@body))

(defmacro deftest-throws [name type code]
  `(deftest ~name
     (run [r# ~code]
       (is (error? r#))
       (is (= ~type (error-type r#))))))

(deftest report-file-name
  (let [r (do-catch
           (repl/eval-ast (scope/scope)
                          (parser/parse "not-in-scope" "hello.bodol")))]
    (is (error? r))
    (is (= "hello.bodol" (:location (:pos (ex-data r)))))))

(deftest-throws report-unbound-symbols
  :unbound-symbol "variable-not-in-scope")

(deftest-throws report-arity-mismatch
  :arity-mismatch "((λ i -> i) 1 2)")
