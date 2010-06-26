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

(defn sock-read
  "ソケットから読み出せるデータを全部読む (TODO: 正しく実装する)"
  [^java.io.InputStream is]
  (let [buf (make-array Byte/TYPE 2000) ;; 全部読んでない!
        len (.read is buf)]
    (take len buf)))
