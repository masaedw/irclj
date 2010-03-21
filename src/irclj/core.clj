(ns irclj.core
  (:use irclj.io irclj.util)
  (:import (com.ibm.icu.text CharsetDetector)))

(defn connect
  "connect to irc server"
  ([host port nick]
     (connect host port nick nil))
  ([host port nick password]
     #{:thread nil}))

(defn send-message
  "send message"
  [handler]
  )

(def byte-seq->str
     (let [detector (CharsetDetector.)]
       (fn this
         ([seq]
            (this seq nil))
         ([seq coding]
            (.getString detector (into-array Byte/TYPE seq) coding)))))

(defn raw-command-seq
  [istream]
  (map byte-seq->str
       (partition-with+ #(not (= 10 %))
                        (byte-seq istream))))
