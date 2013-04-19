(defproject Euler "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/math.combinatorics "0.0.4"]]
  :main Euler.main
  :repl-options
  {
    :init (do (set! *print-length* 103)
              (set! *print-level* 15))
  })
