;; Copyright © 2014-2017, JUXT LTD.

(def VERSION "1.4.0-alpha1")

(defproject yada VERSION
  :description "A complete batteries-included bundle of yada"
  :license {:name "The MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :pedantic? :abort

  :dependencies [[yada/aleph ~VERSION]
                 [yada/async ~VERSION]
                 [yada/bidi ~VERSION]
                 [yada/core ~VERSION]
                 [yada/json ~VERSION]
                 [yada/json-html ~VERSION]
                 [yada/jwt ~VERSION]
                 [yada/multipart ~VERSION]
                 [yada/oauth2 ~VERSION]
                 [yada/swagger ~VERSION :exclusions [clj-time]]
                 [clj-time/clj-time "0.14.3"]
                 [yada/transit ~VERSION]
                 [yada/webjars ~VERSION]]

  :profiles {:test {:dependencies [[org.clojure/clojure "1.8.0"]
                                   [org.webjars/bootstrap "3.3.6"]]}})
