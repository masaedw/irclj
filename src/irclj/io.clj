(ns irclj.io
  (:import (java.net Socket)
           (com.ibm.icu.text CharsetDetector))
  )

(defn open-socket
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

(defn byte-seq
  "InputStreamをバイトシーケンスに変換する"
  [istream]
  (lazy-seq
   (let [buf (make-array Byte/TYPE 2000)
         ret (.read istream buf)]
     (if (not (= ret -1))
       (concat (take ret (seq buf)) (byte-seq istream))
       (.close istream)))))

(defn byte-seq->str
  "バイトシーケンスを文字列に変換する"
  ([seq]
     (byte-seq->str seq nil))
  ([seq coding]
     (let [detector (CharsetDetector.)]
       (.getString detector (into-array Byte/TYPE seq) coding))))
