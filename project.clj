(defproject org.bodil/bodol "0.1.0"
  :description "The BODil Oriented Language (working title)"
  :url "https://github.com/bodil/bodol"
  :license {:name "Apache License, version 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [instaparse "1.2.2"]
                 [org.clojure/core.match "0.2.0-rc5"]
                 [org.clojure/core.logic "0.8.4"]]
  :jvm-opts ["-Dfile.encoding=utf-8"]
  :main bodol.repl)
