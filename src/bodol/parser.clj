(ns bodol.parser
  (:require [instaparse.core :as insta]
            [clojure.edn :as edn]
            [bodol.types :as t]
            [bodol.lambda :as l])
  (:import [bodol.types LBoolean LNumber LString LSymbol LCons]))

(def parser
  (insta/parser
   "
<PROGRAM> = SPACE* SEXPS SPACE*
<SEXPS> = (SEXP SPACE+)* SEXP
<SEXP> = (QUOTED / DEFUN / LAMBDA / DOTTED / LIST / VECTOR / SYMBOL / NUMBER / STRING / BOOLEAN)
QUOTED = <QUOTE> SEXP
QUOTE = <'\\''>
DOTTED = <'('> SPACE* SEXP SPACE <'.'> SPACE SEXP SPACE* <')'>
LIST = <'('> SPACE* !((DEFUN_KEYWORD | LAMBDA_KEYWORD) (SPACE+ | <')'>)) SEXPS* SPACE* <')'>
VECTOR = <'['> SPACE* SEXPS* SPACE* <']'>
DEFUN = <'('> SPACE* <DEFUN_KEYWORD> SPACE+ SYMBOL SPACE+ CLAUSES SPACE* <')'>
DEFUN_KEYWORD = 'ƒ' | 'defn' | 'defun'
LAMBDA = <'('> SPACE* <LAMBDA_KEYWORD> SPACE+ CLAUSES SPACE* <')'>
LAMBDA_KEYWORD = 'λ' | 'fn' | 'lambda'
<CLAUSES> = (CLAUSE SPACE+)* CLAUSE
CLAUSE = ARGS ARROW SPACE+ BODY
ARGS = (!(ARROW SPACE+) SEXP SPACE+)*
<ARROW> = <'->'> | <'→'>
BODY = SEXP
BOOLEAN = '#t' | '#f'
NUMBER = NEGATIVE* (FRACTION | DECIMAL | INTEGER)
<NEGATIVE> = '-'
<FRACTION> = INTEGER '/' INTEGER
<DECIMAL> = INTEGER '.' INTEGER
<INTEGER> = #'\\p{Digit}+'
STRING = '\\\"' #'([^\"\\\\]|\\\\.)*' '\\\"'
SYMBOL = #'[\\pL_$&/=+~:<>|§?*-][\\pL\\p{Digit}_$&/=+~.:<>|§?*-]*'
<SPACE> = <#'[ \t\n,]+'>
"))

(defn span [loc & els]
  {:span
   (reduce
    (fn [v1 v2] [(min (first v1) (first v2))
                (max (second v1) (second v2))])
    (map insta/span els))
   :location loc})

(defn mktype [ltype transform loc input-val]
  (ltype (transform (apply str (rest input-val))) (span loc input-val)))

(defn transform [loc node]
  (let [t (partial transform loc)
        tmap (partial map t)]
    (case (first node)
      :LIST (t/llist (tmap (rest node)) (span loc node))
      :DOTTED (apply t/lcons (span loc node) (tmap (rest node)))
      :QUOTED (t/llist (list (t/lsymbol "quote")
                             (t (second node)))
                       (span loc node))
      :SYMBOL (mktype t/lsymbol identity loc node)
      :BOOLEAN (mktype t/lboolean (partial = "#t") loc node)
      :NUMBER (mktype t/lnumber edn/read-string loc node)
      :STRING (mktype t/lstring edn/read-string loc node)
      :ARGS (apply vector (tmap (rest node)))
      :BODY (t (second node))
      :CLAUSE (apply vector (tmap (rest node)))
      :LAMBDA (apply l/lambda (span loc node) (tmap (rest node)))
      :DEFUN (apply l/defun (span loc node) (tmap (rest node))))))

(defn parse
  ([input loc]
     (map (partial transform loc)
          (let [parsed (parser input)]
            (if (insta/failure? parsed)
              (throw (ex-info (pr-str (insta/get-failure parsed)) {}))
              parsed))))
  ([input] (parse input nil)))

(defn incomplete? [input]
  ;; Naive version, only looks for missing close-paren.
  (let [p (insta/parse parser input)]
    (and (insta/failure? p)
         (some #(= % {:tag :string :expecting ")"}) (:reason p)))))

(defn parse-file [filename]
  (parse (slurp filename) filename))
