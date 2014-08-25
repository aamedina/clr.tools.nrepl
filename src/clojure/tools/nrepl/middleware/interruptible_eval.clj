(ns ^{:author "Chas Emerick"}
  clojure.tools.nrepl.middleware.interruptible-eval
  (:require [clojure.tools.nrepl.transport :as t]
            clojure.tools.nrepl.middleware.pr-values
            [clojure.tools.nrepl.debug :as debug]			
            clojure.main)
  (:use [clojure.tools.nrepl.misc :only (response-for returning)]
        [clojure.tools.nrepl.middleware :only (set-descriptor!)])
  (:import clojure.lang.LineNumberingTextReader                                 
           (System.IO StringReader TextWriter)                                  
           clojure.lang.AtomicLong                                              
           (System.Threading Thread ThreadStart WaitCallback
                             ThreadAbortException)))

(def ^{:dynamic true
       :doc "The message currently being evaluated."}
  *msg* nil)

(def ^{:dynamic true
       :doc "Function returning the evaluation of its argument."}
  *eval* nil)

(defn evaluate
  "Evaluates some code within the dynamic context defined by a map of `bindings`,
   as per `clojure.core/get-thread-bindings`.

   Uses `clojure.main/repl` to drive the evaluation of :code in a second
   map argument (either a string or a seq of forms to be evaluated), which may
   also optionally specify a :ns (resolved via `find-ns`).  The map MUST
   contain a Transport implementation in :transport; expression results and errors
   will be sent via that Transport.

   Returns the dynamic scope that remains after evaluating all expressions
   in :code.

   It is assumed that `bindings` already contains useful/appropriate entries
   for all vars indicated by `clojure.main/with-bindings`."
  [bindings {:keys [code ns transport session eval] :as msg}]
  (let [explicit-ns-binding (when-let [ns (and ns (-> ns symbol find-ns))]
                              {#'*ns* ns})
        bindings (atom (merge bindings explicit-ns-binding))
        out (@bindings #'*out*)
        err (@bindings #'*err*)]
    (if (and ns (not explicit-ns-binding))
      (t/send transport
              (response-for msg {:status #{:error :namespace-not-found :done}}))
      (with-bindings @bindings
        (try
          #_(debug/prn-thread "Evaluating " code " in " (.ManagedThreadId (Thread/CurrentThread))) ;DEBUG
          (clojure.main/repl
           :init #(do (set! *compile-path* (@bindings #'*compile-path*))
                      (set! *1 (@bindings #'*1))
                      (set! *2 (@bindings #'*2))
                      (set! *3 (@bindings #'*3))
                      (set! *e (@bindings #'*e)))   
           :read (if (string? code)
                   (let [reader (LineNumberingTextReader. (StringReader. code))]
                     #(read reader false %2))
                   (let [^System.Collections.IEnumerator code (.GetEnumerator code)]                     #(or (and (.MoveNext code) (.Current code)) %2)))      
           :prompt (fn [])
           :need-prompt (constantly false)
           :print (fn [v] (reset! bindings (assoc (get-thread-bindings)
                                       #'*3 *2
                                       #'*2 *1
                                       #'*1 v))
                    (.Flush ^TextWriter err)
                    (.Flush ^TextWriter out)
                    #_(debug/prn-thread "Evaluating " code " yields " v) ;DEBUG
                    (t/send transport (response-for msg
                                                    {:value v
                                                     :ns (-> *ns* ns-name str)})))
           :caught (fn [e]
                     (let [root-ex (#'clojure.main/root-cause e)]
                       (when-not (instance? ThreadAbortException root-ex)       
                         (reset! bindings (assoc (get-thread-bindings) #'*e e))
                         (t/send transport
                                 (response-for msg {:status :eval-error
                                                    :ex (-> e class str)
                                                    :root-ex
                                                    (-> root-ex class str)}))
                         (clojure.main/repl-caught e)))))
          (finally
            (.Flush ^TextWriter out)
            (.Flush ^TextWriter err)))))                                      
    @bindings))

(def ^{:private true} session-thread-counter (AtomicLong. 0))

#_(defn- exec-eval [f]
    (let [tstart (gen-delegate ThreadStart []
                               (try 
                                 (debug/prn-thread "exec-eval: Starting in thread " (.ManagedThreadId (Thread/CurrentThread)))
                                 (f) 
                                 (debug/prn-thread "exec-eval: Exiting thread " (.ManagedThreadId (Thread/CurrentThread)))
                                 (catch ThreadAbortException e 
                                   (debug/prn-thread "exec-eval: Aborting thread " (.ManagedThreadId (Thread/CurrentThread)))
                                   #_(Thread/ResetAbort)
                                   nil)))
          thread (doto (Thread. tstart)
                   (.set_Name (format "nREPL-worker-%s" (.getAndIncrement session-thread-counter)))
                   (.set_IsBackground true)
                   (.Start))]
      (debug/prn-thread "exec-eval: Started thread " (.ManagedThreadId thread))
      nil))

(defn- exec-eval [f interrupt-handle]
  (let [done-handle (System.Threading.AutoResetEvent. false)
        handles (make-array System.Threading.WaitHandle 2)
        tstart (gen-delegate ThreadStart []
                             (try 
                               #_(debug/prn-thread "exec-eval: Starting in thread " (.ManagedThreadId (Thread/CurrentThread)))
                               (f) 
                               #_(debug/prn-thread "exec-eval: Exiting thread " (.ManagedThreadId (Thread/CurrentThread)))
                               (catch ThreadAbortException e 
                                 #_(debug/prn-thread "exec-eval: Aborting thread " (.ManagedThreadId (Thread/CurrentThread)))
                                 (Thread/ResetAbort)
                                 nil)
                               (finally (.Set done-handle))))
        thread (doto (Thread. tstart)
                 (.set_Name (format "nREPL-worker-%s" (.getAndIncrement session-thread-counter)))
                 (.set_IsBackground true)
                 (.Start))]
    #_(debug/prn-thread "exec-eval: Started thread " (.ManagedThreadId thread))
    #_(debug/prn-thread "exec-eval: Starting wait")
    (aset handles 0 interrupt-handle)
    (aset handles 1 done-handle)
    (let [i (System.Threading.WaitHandle/WaitAny handles)]
      #_(debug/prn-thread "exec-eval: done waiting, handle = " i)
      (when (= i 0)
        #_(debug/prn-thread "exec-eval: interrupted, aborting thread")
        (.Abort thread))
      (when (= i 1)
        #_(debug/prn-thread "exec.eval: normal exit")))
    nil))

                                        ;DM:end Added

                                        ; A little mini-agent implementation. Needed because agents cannot be used to host REPL
                                        ; evaluation: http://dev.clojure.org/jira/browse/NREPL-17
(defn- prep-session
  [session]
  (locking session
    (returning session
               (when-not (-> session meta :queue)
                 (alter-meta! session assoc :queue (atom clojure.lang.PersistentQueue/EMPTY))))))

(declare run-next)
(defn- run-next*
  [session executor ihandle]                               ;DM: removed ^Executor
  #_(debug/prn-thread "run-next* on session ") ;DEBUG
  (let [qa (-> session meta :queue)]
    (loop []
      (let [q @qa
            qn (pop q)]
        (if-not (compare-and-set! qa q qn)
          (recur)
          (when (seq qn)
            (let [fnext (run-next session executor ihandle (peek qn))]
              (exec-eval fnext ihandle))))))))
                                        ;DM: (.execute executor (run-next session executor (peek qn)))

(defn- run-next
  [session executor ihandle f]
  #(try
     #_(debug/prn-thread "run-next: ready to run f, thread = " (.ManagedThreadId (Thread/CurrentThread)))
     (f)
     #_(debug/prn-thread "run-next: after running f, thread = " (.ManagedThreadId (Thread/CurrentThread)))
     (finally
       #_(debug/prn-thread "run-next: looping, thread = " (.ManagedThreadId (Thread/CurrentThread)))
       (run-next* session executor ihandle))))

(defn- queue-eval
  "Queues the function for the given session."
  [session executor ihandle f]                                                                   ;DM: removed ^Executor
  (let [qa (-> session prep-session meta :queue)]
    (loop []
      (let [q @qa]
        (if-not (compare-and-set! qa q (conj q f))
          (recur)
          (when (empty? q)
            (let [fnext (run-next session executor ihandle f)]
              (exec-eval fnext ihandle))))))))
                                        ;DM: (.execute executor (run-next session executor f))

(defn interruptible-eval
  "Evaluation middleware that supports interrupts.  Returns a handler that supports
   \"eval\" and \"interrupt\" :op-erations that delegates to the given handler
   otherwise."
  [h & {:keys [executor] :or {executor nil}}]                                    ;DM: (configure-executor) replaced with nil
  (let [interrupt-handle (System.Threading.AutoResetEvent. false)]
    (fn [{:keys [op session interrupt-id id transport] :as msg}]
      (case op
        "eval"
        (if-not (:code msg)
          (do #_(debug/prn-thread "IEval: no code: " msg) (t/send transport (response-for msg :status #{:error :no-code})))
          (queue-eval session executor interrupt-handle
                      (comp
                       
                       (partial reset! session)
                       (fn []
                         (alter-meta! session assoc
                                      :thread (Thread/CurrentThread)                                  ;DM: Thread/currentThread
                                      :ihandle interrupt-handle
                                      :eval-msg msg)
                         (binding [*msg* msg]
                           #_(debug/prn-thread "IEval: getting ready to call evaluate, thread = " (.ManagedThreadId (Thread/CurrentThread)))
                           (returning (dissoc (evaluate @session msg) #'*msg*)
                                      #_(debug/prn-thread "IEval: sending status done")
                                      (t/send transport (response-for msg :status :done))
                                      #_(debug/prn-thread "IEval: sending status done again")
                                      ;; (t/send transport (response-for msg :status :done))
                                      (alter-meta! session dissoc :thread :eval-msg :ihandle)))))))
        
        "interrupt"
                                        ; interrupts are inherently racy; we'll check the agent's :eval-msg's :id and
                                        ; bail if it's different than the one provided, but it's possible for
                                        ; that message's eval to finish and another to start before we send
                                        ; the interrupt / .stop.
        (let [{:keys [id eval-msg ihandle]} (meta session)]  ;;; ^Thread thread
          #_(debug/prn-thread "IEval: interrupt received")
          #_(debug/prn-thread "IEval: interrupt-id = " interrupt-id ", id = " (:id eval-msg))
          #_(debug/prn-thread "IEval: ihandle = " ihandle)
          #_(debug/prn-thread "IEval: interrupt thread = " (and thread (.ManagedThreadId thread)))
          #_(if (or (not interrupt-id)
                    (= interrupt-id (:id eval-msg)))
              #_(if-not thread
                  (debug/prn-thread "IEval: interrupt: Sending status :done :session-idle")
                (debug/prn-thread "IEval: interrupt: aborting thread, sending status :interrupted"))
              #_(debug/prn-thread "IEval: interrupt: sending interrupt-id-mismatch"))
          (if (or (not interrupt-id)
                  (= interrupt-id (:id eval-msg)))
            (if-not ihandle                                                              ;;; thread
              (t/send transport (response-for msg :status #{:done :session-idle}))
              (do
                #_(debug/prn-thread "IEval: interrupt: sending :interrupted status message")
                (t/send transport {:status #{:interrupted}
                                   :id (:id eval-msg)
                                   :session id})
                #_(debug/prn-thread "IEval: interrupt: preparing to abort thread " #_(.ManagedThreadId thread))			  
                #_(.Abort thread)                                                   ;DM: .stop
                (.Set ihandle)
                #_(debug/prn-thread "IEval: interrupt: thread .Abort called")
                #_(debug/prn-thread "IEval: interrupt: preparing to send :done status")
                (t/send transport (response-for msg :status #{:done}))
                #_(debug/prn-thread "IEval: interrupt: preparing to send :done status AGAIN")
                (t/send transport (response-for msg :status #{:done}))
                
                ))
            (t/send transport (response-for msg :status #{:error :interrupt-id-mismatch :done}))))
        
        (h msg)))))

(set-descriptor! #'interruptible-eval
                 {:requires #{"clone" "close" #'clojure.tools.nrepl.middleware.pr-values/pr-values}
                  :expects #{}
                  :handles {"eval"
                            {:doc "Evaluates code."
                             :requires {"code" "The code to be evaluated."
                                        "session" "The ID of the session within which to evaluate the code."}
                             :optional {"id" "An opaque message ID that will be included in responses related to the evaluation, and which may be used to restrict the scope of a later \"interrupt\" operation."}
                             :returns {}}
                            "interrupt"
                            {:doc "Attempts to interrupt some code evaluation."
                             :requires {"session" "The ID of the session used to start the evaluation to be interrupted."}
                             :optional {"interrupt-id" "The opaque message ID sent with the original \"eval\" request."}
                             :returns {"status" "'interrupted' if an evaluation was identified and interruption will be attempted
'session-idle' if the session is not currently evaluating any code
'interrupt-id-mismatch' if the session is currently evaluating code sent using a different ID than specified by the \"interrupt-id\" value "}}}})
