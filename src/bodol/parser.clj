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

(defmacro ltype [type]
  `(fn [value#] (~(symbol (str (name type) ".")) value#)))

(defn parse [input]
  (insta/transform
   {:BOOLEAN (comp (ltype LBoolean) (partial = "#t") str)
    :NUMBER (comp (ltype LNumber) edn/read-string str)
    :STRING (comp (ltype LString) edn/read-string str)
    :SYMBOL (comp (ltype LSymbol) str)
    :DOTTED #(LCons. %1 %2)
    :LIST t/cons-list
    :VECTOR vector
    :QUOTED #(LCons. (LSymbol. "quote") (LCons. % nil))
    :ARGS vector
    :BODY identity
    :CLAUSE vector
    :LAMBDA l/lambda
    :DEFUN l/defun}
   (let [parsed (parser input)]
     (if (insta/failure? parsed)
       (throw (ex-info (pr-str (insta/get-failure parsed)) {}))
       parsed))))
