(ns irclj.core
  (:use irclj.io))

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

