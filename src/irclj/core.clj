(ns irclj.core
  (:use irclj.io irclj.util)
  )

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

(defn raw-command-seq
  [istream]
  (map byte-seq->str
       (partition-with+ #(not (= 10 %))
                        (byte-seq istream))))
