(ns clojure.tools.nrepl.middleware.complete
  (:require [clojure.tools.nrepl.transport :as transport]
            clojure.tools.nrepl.middleware.pr-values
            [clojure.tools.nrepl.debug :as debug]
            [clojure.tools.nrepl.misc :refer [response-for]]
            [clojure.string :as s]
            [clojure.reflect :as r]
            [clojure.tools.nrepl.middleware.complete.ns-mappings :as ns-maps]
            [clojure.tools.nrepl.middleware.complete.special-forms :as sf]
            [clojure.tools.nrepl.middleware.complete.context
             :refer [cache-context]]
            [clojure.tools.nrepl.middleware.complete.local-bindings :as lb]
            [clojure.tools.nrepl.middleware.complete.namespaces-and-classes
             :as nc]
            [clojure.tools.nrepl.middleware.complete.class-members :as cm]
            clojure.main)
  (:use [clojure.tools.nrepl.misc :only (response-for returning)]
        [clojure.tools.nrepl.middleware :only (set-descriptor!)])
  (:import clojure.lang.LineNumberingTextReader                                 
           (System.IO StringReader TextWriter)                                  
           clojure.lang.AtomicLong                                              
           (System.Threading Thread ThreadStart WaitCallback
                             ThreadAbortException)))

(defn complete
  [{:keys [symbol ns context] :as msg}]
  (println context)
  (let [ns (when ns (clojure.core/symbol ns))
        prefix (str symbol)
        ctx (cache-context context)]
    (->> (for [candidates [ns-maps/candidates sf/candidates lb/candidates
                           nc/candidates
                           cm/members-candidates
                           cm/static-members-candidates
                           ]]
           (candidates prefix ns ctx))
         flatten
         (remove nil?)
         (sort-by count))))

(defn completion-doc
  [{:keys [symbol ns] :as msg}]
  (let [symbol-str (str symbol)
        ns (clojure.core/symbol ns)]
    (->> (for [doc [ns-maps/doc sf/doc nc/doc
                    cm/members-doc cm/static-member-doc
                    ]]
           (doc symbol-str ns))
         (remove nil?)
         (interpose "\n\n")
         (s/join))))

(defn complete-reply
  [{:keys [transport] :as msg}]
  (let [results (complete msg)]   
    (try
      (transport/send transport (response-for msg :value results))
      (catch Exception e
        (transport/send
         transport (response-for msg :exception (.Message e)))))
    (transport/send transport (response-for msg :status :done))))

(defn doc-reply
  [{:keys [transport] :as msg}]
  (let [results (completion-doc msg)]
    (transport/send transport (response-for msg :value results))
    (transport/send transport (response-for msg :status :done))))

(defn wrap-complete
  [f]
  (fn [{:keys [op] :as msg}]
    (cond
      (= "complete" op) (complete-reply msg)
      (= "complete-doc" op) (doc-reply msg)
      :else (f msg))))

(set-descriptor!
 #'wrap-complete
 {:handles {"complete"
            {:doc "Return a list of symbols matching the specified (partial) symbol."
             :requires {"symbol" "The symbol to lookup"
                        "ns" "The current namespace"}
             :returns {"status" "done"}}}})
