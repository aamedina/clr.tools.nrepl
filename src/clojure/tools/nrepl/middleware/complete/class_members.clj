(ns clojure.tools.nrepl.middleware.complete.class-members
  "Completion for both static and non-static class members."
  (:require [clojure.tools.nrepl.middleware.complete.utils
             :refer [parts-match? resolve-class]]
            [clojure.string :refer [split join]]
            [clojure.reflect :as r]
            [clojure.set :as set])
  (:import (clojure.reflect Method Field)))

(defn ns-classes
  [ns]
  (filter class? (vals (ns-map ns))))

(defn method?
  [member]
  (instance? Method member))

(defn class-methods
  [cls]
  (filter method? (:members (r/reflect cls))))

(defn class-fields
  [cls]
  (remove method? (:members (r/reflect cls))))

(defn static?
  "Tests if class member is static."
  [member]
  (contains? (:flags member) :static))

(defn maybe-reflect
  [cls]
  (try
    (:members (r/reflect cls))
    (catch NullReferenceException e)))

;; ## Regular (non-static) members

(def ^{:doc "Stores cache of all non-static members for every
  namespace."}
  members-cache (atom {}))

(defn populate-members-cache
  "Populates members cache for a given namespace. `classes-cnt` is a
  number that indicates the current number of imported classes in this
  namespace."
  [ns classes-cnt]
  (loop [cache (transient {})
         [c & r] (->> (map maybe-reflect (ns-classes ns))
                      (remove nil?)
                      (reduce into []))]
    (if c
      (let [full-name (:name c)]
        (if (cache full-name)
          (recur (assoc! cache full-name (conj (cache (:name c)) c)) r)
          (recur (assoc! cache full-name [c]) r)))
      (swap! members-cache assoc ns {:classes-cnt classes-cnt
                                     :methods (persistent! cache)}))))

(defn update-cache
  "Updates members cache for a given namespace if necessary."
  [ns]
  (let [imported-cls-cnt (count (filter class? (vals (ns-map ns))))]
    (when (or (nil? (@members-cache ns))
              (not= (get-in @members-cache [ns :classes-cnt])
                    imported-cls-cnt))
      (populate-members-cache ns imported-cls-cnt))))

(defn get-all-members
  "Returns all non-static members for a given namespace."
  [ns]
  (update-cache ns)
  (get-in @members-cache [ns :methods]))

(defn class-member-symbol?
  "Tests if a symbol name looks like a non-static class member."
  [x]
  (.StartsWith x "."))

(defn camel-case-matches?
  "Tests if prefix matches the member name following camel case rules.
  Thus, prefix `getDeF` matches member `getDeclaredFields`."
  [prefix, member-name]
  (let [regex #"[A-Z]?[a-z]*"
        prefix-parts (re-seq regex prefix)
        cl-parts (re-seq regex member-name)]
    (parts-match? prefix-parts cl-parts)))

(defn try-get-object-class
  "Tries to get the type of the object from the context, which the
  member will be applied to. Object should be a Var."
  [context]
  (when (= (:idx (first context)) 0)
    (let [sym (second (:form (first context)))]
      (when (and (symbol? sym)
                 (= (type (resolve sym)) clojure.lang.Var))
        (type (deref (resolve sym)))))))

(defn members-candidates
  "Returns a list of Java non-static fields and methods candidates."
  [prefix ns context]
  (when (class-member-symbol? prefix)
    (let [prefix (subs prefix 1)
          inparts? (re-find #"[A-Z]" prefix)
          klass (try-get-object-class context)]
      (for [[member-name members] (get-all-members ns)
            :when (if inparts?
                    (camel-case-matches? prefix (name member-name))
                    (.StartsWith (name member-name) prefix))
            :when
            (or (not klass)
                (some #(= klass (:declaring-class %)) members))]
        (str "." member-name)))))

;; ### Member documentation

(defn type-to-pretty-string
  "Takes a type (either a class or a primitive) and returns it's
  human-readable name."
  [t]
  (pr-str t))

(defn doc-method-parameters
  "Takes a list of method parameters and stringifies it."
  [parameters]
  (->> parameters
       (map type-to-pretty-string)
       (interpose " ")
       join
       (format "(%s)")))

(defn create-members-doc
  "Takes a list of members (presumably with the same name) and turns
  them into a docstring."
  [members]
  (->> members
       (group-by (fn [m] (:declaring-class m)))
       (map (fn [[class, members]]
              (let [f-mem (first members)]
                (str (.FullName class) "." (:name f-mem)
                     (if (instance? Field f-mem)
                       (str " = " (try nil (catch Exception e "?"))
                            " (" (type-to-pretty-string (:type f-mem)) ")\n"
                            (->> (map name (:flags f-mem))
                                 (join " ")))
                       (join
                        (map (fn [member]
                               (when (instance? Method member)
                                 (str "\n  " (doc-method-parameters
                                              (:parameter-types member))
                                      " -> " (type-to-pretty-string
                                              (:return-type member))
                                      " (" (->> (map name (:flags member))
                                                (join " ")) ")")))
                             (distinct members))))
                     "\n"))))
       (interpose "\n")
       join))

(defn members-doc
  "Documentation function for non-static members."
  [member-str ns]
  (when (class-member-symbol? member-str)
    (update-cache ns)
    (when-let [member (get-in @members-cache [ns :methods (subs member-str 1)])]
      (create-members-doc member))))

(defn classname-doc [class]
  (let [members (group-by static? (concat (.GetMethods class)
                                          (.GetFields class)))
        [static non-static] (for [flag [true false]]
                              (->> (for [m (members flag)]
                                     (.FullName m))
                                   distinct
                                   (interpose ", ")
                                   join))]
    (str (.FullName class) "\n\n"
         " Non-static members:\n  " non-static "\n\n"
         " Static members:\n  " static "\n")))

(defn static-member-symbol?
  "Tests if prefix looks like a static member symbol."
  [x]
  (and (not (.StartsWith x ":"))
       (> (.IndexOf x "/") -1)))

(def ^{:doc "Stores cache of all static members for every class."}
  static-members-cache (atom {}))

(defn populate-static-members-cache
  "Populates static members cache for a given class."
  [class]
  (loop [cache {}, [c & r] (->> (:members (r/reflect class))
                                (filter static?))]
    (if c
      (if (static? c)
        (let [full-name (:name c)]
          (if (cache (:name c))
            (recur (update-in cache [full-name] conj c) r)
            (recur (assoc cache full-name [c]) r)))
        (recur cache r))
      (swap! static-members-cache assoc class cache))))

(defn update-static-cache
  "Updates static members cache for a given class if necessary."
  [class]
  (when-not (@static-members-cache class)
    (populate-static-members-cache class)))

(defn static-members
  "Returns all static members for a given class."
  [class]
  (update-static-cache class)
  (keys (@static-members-cache class)))

(defn static-members-candidates
  "Returns a list of static member candidates."
  [prefix ns context]
  (when (static-member-symbol? prefix)
    (let [[cl-name member-prefix] (.Split prefix (char-array "/"))]
      (when-let [cl (resolve-class (symbol cl-name))]
        (let [member-prefix (or member-prefix "")
              inparts? (re-find #"[A-Z]" member-prefix)]
          (for [member (map name (static-members cl))
                :when  (if inparts?
                         (camel-case-matches? member-prefix member)
                         (.StartsWith member member-prefix))]
            (str cl-name "/" member)))))))

(defn static-member-doc
  "Given a member name and class returns its docstring."
  [member-str ns]
  (when (static-member-symbol? member-str)
    (let [[cl-name member-name] (.Split member-str (char-array "/"))
          cl (resolve-class (symbol cl-name))
          member (when cl
                   (update-static-cache cl)
                   (get-in @static-members-cache [cl member-name]))]
      (when member
        (create-members-doc member)))))
