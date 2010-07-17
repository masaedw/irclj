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
  (with-socket [s i o] "tucc.aa0.netvolante.jp" 6667
    (let [writer (PrintWriter. (OutputStreamWriter. o))]
      (dorun
       (pseudo-map-accum-l (fn [env msg] (irc-response env writer msg))
                           init-env
                           (msg-seq i)))))
  (System/exit 0))
