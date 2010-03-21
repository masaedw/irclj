(ns irclj.io
  (:import (java.net Socket)))

(defn open-socket
  [host port]
  (let [sock (Socket. host port)]
    [sock
     (.getInputStream sock)
     (.getOutputStream sock)]))

(defn byte-seq
  "InputStreamをバイトシーケンスに変換する"
  [istream]
  (lazy-seq
   (let [buf (make-array Byte/TYPE 2000)
         ret (.read istream buf)]
     (if (not (= ret -1))
       (into (take ret (seq buf)) (byte-seq istream))
       (.close istream)))))
