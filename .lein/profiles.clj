{:user {:dependencies [[org.clojure/tools.namespace "0.2.11"]
                       [org.clojure/tools.nrepl "0.2.11"]
                       [spyscope "0.1.5"]
                       [criterium "0.4.1"]]

        :injections [(require '(clojure.tools.namespace repl find))
                                        ; try/catch to workaround an issue where `lein repl` outside a project dir
                                        ; will not load reader literal definitions correctly:
                     (try (require 'spyscope.core)
                          (catch RuntimeException e))]

        :plugins [[lein-difftest "1.3.8"]
                  [lein-marginalia "0.7.1"]
                  [lein-pprint "1.1.1"]
                  [lein-swank "1.4.4"] 
                  [lein-antlr "0.1.0"]
                  [lein-immutant "0.14.1"]
                  [lein-exec "0.3.0"]
                  [lein-midje "3.1.1"]
                  [cider/cider-nrepl "0.10.0-SNAPSHOT"]
                  [lein-bikeshed "0.3.0"]
                  ]}}
