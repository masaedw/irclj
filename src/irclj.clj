(ns irclj
  (:gen-class)
  (:use irclj.io
        irclj.util
        irclj.core)
  (:import (java.io PrintWriter
                    OutputStreamWriter)))

(defn -main [& args]
  (with-socket [s i o] "localhost" 3000
    (let [w (PrintWriter. (OutputStreamWriter. o))]
      (doto w
        (.print (str "GET /projects/ HTTP/1.0\r\n"
                     "User-Agent: hogera\r\n"
                     "Host: localhost\r\n\r\n"))
        (.flush))
      (let [x (raw-command-seq i)]
        (doseq [line x]
          (print line)))
      (flush)))
  (System/exit 0))
