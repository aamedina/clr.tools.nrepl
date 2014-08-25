(ns clojure.tools.nrepl.main
  (:gen-class)
  (:require [clojure.tools.nrepl.ack :as ack]
            [clojure.tools.nrepl.server :as server]))

(def ack-server
  (delay (server/start-server
          :handler (ack/handle-ack server/unknown-op))))

(defn -main
  [& args]
  (ack/reset-ack-port!)
  (let [server (server/start-server
                :ack-port (:port @ack-server))]
    (println "server started: " server)))
