(ns clojure.tools.nrepl.middleware.info
  (:require [clojure.string :as str]
            [clojure.clr.io :as io]
            [clojure.tools.nrepl.middleware.complete.utils :as u]
            [clojure.repl :as repl]
            [clojure.tools.nrepl.transport :as transport]
            [clojure.tools.nrepl.middleware :refer [set-descriptor!]]
            [clojure.tools.nrepl.misc :refer [response-for]]))

(defn maybe-protocol
  [info]
  (if-let [prot-meta (meta (:protocol info))]
    (merge info {:file (:file prot-meta)
                 :line (:line prot-meta)})
    info))

(def var-meta-whitelist
  [:ns :name :doc :file :arglists :macro :protocol :line :column :static :added :deprecated :resource])

(defn- map-seq [x]
  (if (seq x)
    x
    nil))

(defn var-meta
  [v]
  (-> v meta maybe-protocol (select-keys var-meta-whitelist) map-seq))

(defn ns-meta
  [ns]
  (merge
   (meta ns)
   {:ns ns
    :file (-> (ns-publics ns)
              first
              second
              var-meta
              :file)
    :line 1}))

(defn resolve-var
  [ns sym]
  (if-let [ns (find-ns ns)]
    (try (ns-resolve ns sym)
         (catch Exception _
           nil)
         (catch Exception _
           nil))))

(defn resolve-aliases
  [ns]
  (if-let [ns (find-ns ns)]
    (ns-aliases ns)))

(defn info-clj
  [ns sym]
  (cond
   (var-meta (resolve-var ns sym)) (var-meta (resolve-var ns sym))
   (get (resolve-aliases ns) sym) (ns-meta (get (resolve-aliases ns) sym))
   (find-ns sym) (ns-meta (find-ns sym))
   :else nil
   ))

(defn info
  [{:keys [ns symbol class member] :as msg}]
  (let [[ns symbol class member] (map u/as-sym [ns symbol class member])]
    (info-clj ns symbol)))


(defn file-path
  "For a file path, return a URL to the file if it exists and does not
  represent a form evaluated at the REPL."
  [x]
  (when (seq x)
    (let [f (io/as-file x)]
      (when (and (.Exists f)
                 (not (-> f .GetName (.StartsWith "form-init"))))
        (io/as-uri f)))))

(declare format-response)

(defn format-nested
  "Apply response formatting to nested `:candidates` info for Java members."
  [info]
  (if-let [candidates (:candidates info)]
    (assoc info :candidates
           (into {} (for [[k v] candidates]
                      [k (format-response v)])))
    info))

(defn blacklist
  "Remove anything that might contain arbitrary EDN, metadata can hold anything"
  [info]
  (let [blacklisted #{:arglists :forms}]
    (apply dissoc info blacklisted)))

(defn format-response
  [info]
  (when info
    (-> info
        (merge (when-let [ns (:ns info)]
                 (:ns (str ns)))
               (when-let [args (:arglists info)]
                 {:arglists-str (pr-str args)})
               (when-let [forms (:forms info)]
                 {:forms-str (->> (map #(str "  " (pr-str %)) forms)
                                  (str/join \newline))}))
        format-nested
        blacklist
        u/transform-value)))

(defn info-reply
  [{:keys [transport] :as msg}]
  (try
    (transport/send
     transport (response-for msg :value (format-response (info msg))))
    (catch Exception e
      (transport/send
       transport (response-for msg :exception (.Message e)))))
  (transport/send transport (response-for msg :status :done)))

(defn wrap-info
  "Middleware that looks up info for a symbol within the context of a particular namespace."
  [handler]
  (fn [{:keys [op] :as msg}]
    (if (= "info" op)
      (info-reply msg)
      (handler msg))))

(set-descriptor!
 #'wrap-info
 {:handles
  {"info"
   {:doc "Return a map of information about the specified symbol."
    :requires {"symbol" "The symbol to lookup"
               "ns" "The current namespace"}
    :returns {"status" "done"}}}})
