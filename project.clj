(defproject evaluate-hand "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot evaluate-hand.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:plugins [[cider/cider-nrepl "0.8.0-SNAPSHOT"]]
                   :dependencies [[org.clojure/tools.namespace "0.2.5"]]
                   :repl-options {:init-ns user}
                   :source-paths ["dev"]}})
