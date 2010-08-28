(ns irclj
  (:gen-class)
  (:use irclj.io
        irclj.core
        irclj.bot
        irclj.haskell
        )
  (:import (java.io PrintWriter
                    OutputStreamWriter)))

(defn -main [& args]
  (let [config (load-file "config.clj")]
    (with-socket [s i o] (config :server) (config :port)
      (let [writer (PrintWriter. (OutputStreamWriter. o))]
        (dorun
         (pseudo-map-accum-l (fn [env msg] (irc-response env writer msg))
                             (merge config init-env)
                             (msg-seq i))))))
  (System/exit 0))
