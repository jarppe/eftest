(ns eftest.output-capture
  (:import (java.io OutputStream ByteArrayOutputStream PrintStream PrintWriter)
           (java.nio.charset StandardCharsets)))

(def captured-output-contexts (atom {}))

(defn thread-id []
  (-> (Thread/currentThread)
      (.getId)))

(defn begin-capture []
  (let [buffer (ByteArrayOutputStream.)]
    (swap! captured-output-contexts assoc (thread-id) buffer)
    buffer))

(defn get-capture-buffer ^ByteArrayOutputStream []
  (get @captured-output-contexts (thread-id)))

(defn flush-captured-output ^String []
  (some-> (get-capture-buffer)
          .toByteArray
          (String. StandardCharsets/UTF_8)))

(defn create-proxy-output-stream ^OutputStream []
  (proxy [OutputStream] []
    (write
      ([data]
       (let [target (get-capture-buffer)]
         (if (instance? Integer data)
           (.write target ^int data)
           (.write target ^bytes data 0 (alength ^bytes data)))))
      ([data off len]
       (let [target (get-capture-buffer)]
         (.write target data off len))))))

(defn init-capture []
  (let [old-out System/out
        old-err System/err
        proxy-output-stream (create-proxy-output-stream)
        new-stream (PrintStream. proxy-output-stream)
        new-writer (PrintWriter. proxy-output-stream)]
    (System/setOut new-stream)
    (System/setErr new-stream)
    {:captured-writer new-writer
     :old-system-out old-out
     :old-system-err old-err}))

(defn restore-capture [{:keys [old-system-out old-system-err]}]
  (System/setOut old-system-out)
  (System/setErr old-system-err))
