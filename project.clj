(defproject org.clojure.clr/tools.nrepl "0.2.1-SNAPSHOT"
  :description "Port of clojure.org/tools.nrepl to ClojureCLR"
  :url "https://github.com/clojure/clr.tools.nrepl"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies []
  :min-lein-version "2.0.0"
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo/"
                                    :sign-releases  false}]]	 
  :clr {:cmd-templates
        {:clj-exe   ["mono" [CLJCLR16_00 %1]]
         :nuget-ver ["mono" [*PATH "nuget.exe"] "install" %1 "-Version" %2]
         :nuget-any ["mono" [*PATH "nuget.exe"] "install" %1]}
        :main-cmd      [:clj-exe "Clojure.Main.exe"]
        :compile-cmd   [:clj-exe "Clojure.Compile.exe"]}
  :main ^:skip-aot clojure.tools.nrepl.main)
