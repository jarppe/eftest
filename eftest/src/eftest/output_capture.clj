(ns eftest.output-capture
  (:import (java.io PrintStream PrintWriter ByteArrayOutputStream)
           (java.nio.charset StandardCharsets)
           (javassist.util.proxy ProxyFactory MethodFilter MethodHandler)))

(def captured-output-contexts (atom {}))

(defn thread-id []
  (-> (Thread/currentThread)
      (.getId)))

(defn begin-capture []
  (let [buffer (ByteArrayOutputStream.)
        context {:buffer buffer
                 :stream (PrintStream. buffer)
                 :writer (PrintWriter. buffer)}]
    (swap! captured-output-contexts assoc (thread-id) context)
    context))

(defn get-capture-context []
  (get @captured-output-contexts (thread-id)))

(defn flush-captured-output ^String []
  (let [context (get-capture-context)
        {:keys [^ByteArrayOutputStream buffer ^PrintStream stream ^PrintWriter writer]} context]
    (when buffer
      (.flush stream)
      (.flush writer)
      (-> buffer .toByteArray (String. StandardCharsets/UTF_8)))))

(defn create-proxy [superclass context-key]
  (let [factory (doto (ProxyFactory.)
                  (.setSuperclass superclass)
                  (.setFilter (reify MethodFilter
                                (isHandled [this m] true))))
        handler (reify MethodHandler
                  (invoke [this self method proceed args]
                    (let [target (get (get-capture-context) context-key)]
                      (.invoke method target args))))]
    (.create factory
             (into-array Class [java.io.OutputStream])
             (into-array Object [(ByteArrayOutputStream.)])
             handler)))

(defn create-proxy-print-stream ^PrintStream []
  (create-proxy java.io.PrintStream :stream))

(defn create-proxy-print-writer ^PrintWriter []
  (create-proxy java.io.PrintWriter :writer))

(defn init-capture []
  (let [old-out System/out
        old-err System/err
        new-stream (create-proxy-print-stream)
        new-writer (create-proxy-print-writer)]
    (System/setOut new-stream)
    (System/setErr new-stream)
    {:captured-writer new-writer
     :old-system-out old-out
     :old-system-err old-err}))

(defn restore-capture [{:keys [old-system-out old-system-err]}]
  (System/setOut old-system-out)
  (System/setErr old-system-err))
