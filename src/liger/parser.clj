(ns liger.parser
  (:require [instaparse.core :as insta]
            [clojure.edn :as edn]
            [liger.types :as t])
  (:import [liger.types LBoolean LNumber LString LSymbol LCons]))

(def parser
  (insta/parser
   "
<PROGRAM> = SPACE* SEXPS SPACE*
<SEXPS> = (SEXP SPACE)* SEXP
<SEXP> = (QUOTED / DOTTED / LIST / VECTOR / SYMBOL / NUMBER / STRING / BOOLEAN)
QUOTED = <QUOTE> SEXP
QUOTE = <'\\''>
DOTTED = <'('> SPACE* SEXP SPACE <'.'> SPACE SEXP SPACE* <')'>
LIST = <'('> SPACE* SEXPS* SPACE* <')'>
VECTOR = <'['> SPACE* SEXPS* SPACE* <']'>
BOOLEAN = '#t' | '#f'
NUMBER = NEGATIVE* (FRACTION | DECIMAL | INTEGER)
<NEGATIVE> = '-'
<FRACTION> = INTEGER '/' INTEGER
<DECIMAL> = INTEGER '.' INTEGER
<INTEGER> = #'\\p{Digit}+'
STRING = '\\\"' #'([^\"\\\\]|\\\\.)*' '\\\"'
SYMBOL = #'[\\p{IsAlphabetic}_$&/=+~:<>|§?*-][\\p{IsAlphabetic}\\{Digit}_$&/=+~.:<>|§?*-]*'
<SPACE> = <(' ' | '\t' | '\n' | ',')+>
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
    :QUOTED #(LCons. (LSymbol. "quote") (LCons. % nil))}
   (let [parsed (parser input)]
     (if (insta/failure? parsed)
       (throw (ex-info (pr-str (insta/get-failure parsed)) {}))
       parsed))))
