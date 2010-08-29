(ns irclj.io
  (:import (java.net Socket))
  )

(defn open-socket
  "port番号は数値です"
  [host port]
  (let [sock (Socket. host port)]
    [sock
     (.getInputStream sock)
     (.getOutputStream sock)]))

(defmacro with-socket
  [bindings host port & body]
  `(let [[s# i# o#] (open-socket ~host ~port)]
     (let [~bindings [s# i# o#]]
       (try
        ~@body
        (finally
         (.close o#)
         (.close i#)
         (.close s#)
         )))))

(defn- read-available
  [^java.io.InputStream is]
  (let [buf (make-array Byte/TYPE 2000)
        len (.read is buf)]
    (if (not (= len -1))
      (take len buf)
      (.close is))))

(defn sock-read
  "ソケットから読み出せるデータを全部読んで、バイトシーケンスとして返す"
  [^java.io.InputStream is]
  (letfn [(step [is]
                (when-let [value (read-available is)]
                  (concat value (sock-read is))))]
    (lazy-seq (step is))))
