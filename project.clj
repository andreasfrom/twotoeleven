(defproject twotoeleven "0.1.0-SNAPSHOT"
  :description "Clone of 2048 with undo/redo and branching in ClojureScript with Om"
  :url "https://github.com/andreasfrom/twotoeleven"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2202"]
                 [org.clojure/core.async "0.1.298.0-2a82a1-alpha"]
                 [om "0.6.2"]
                 [com.cemerick/piggieback "0.1.3"]]

  :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

  :plugins [[lein-cljsbuild "1.0.3"]]

  :source-paths ["src"]

  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src"]
              :compiler {
                :output-to "dev.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}

             {:id "release"
              :source-paths ["src"]
              :compiler {
                :output-to "release.js"
                :optimizations :advanced
                :pretty-print false
                :preamble ["react/react.min.js"]
                :externs ["react/externs/react.js"]}}]})
