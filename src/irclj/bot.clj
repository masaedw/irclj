(ns irclj.bot
  (:use irclj.core
        clojure.contrib.pprint)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bot

;;
;; structure of env
;;
;; {
;;  ;; config
;;
;;  :server "server name"
;;  :port 6667 ;; port number
;;  :nick "nick"
;;  :user "user"
;;  :realname "realname"
;;  :init-channels '("#initial" "#channel" "#list")
;;
;;  ;; internal data
;;
;;  :mode :mode-of-loop
;;
;;  :channels '(list of <channel-info>)
;; }
;;
;; <channel-info>
;;
;; {
;;  :channel "#name"
;; }
;;

;;
;; filter APIs
;;
;; * privmsg-filter
;;   signature: (fn [nick msg] ...)
;;   nick: sender
;;   msg: message body
;;
;;   privmsg-filter が何かを返したら、noticeされます。
;;
;;


(defn dp [arg]
  (print "-------------------------------------------------------------------------------\n")
  (pprint arg)
  arg)

(def privmsg-filters (ref {}))

(def init-env {:mode :init})

(defmulti irc-process (fn [env _ _] (:mode env)))

(defn print-command
  [writer cmd & arg]
  (doto writer
    (.print (apply str (cons cmd arg)))
    (.flush)))

;; 最初のアクセス → read-motd
(defmethod irc-process :init [env writer msg]
  (let [host (.. java.net.InetAddress getLocalHost getHostName)]
    (print-command writer
                   "NICK " (env :nick) "\r\n"
                   "USER " (env :user) " " host " " host " :" (env :realname) "\r\n")
    [(assoc env :mode :read-motd) msg]
    ))

;; motdよみこみ → loop
(defmethod irc-process :read-motd [env writer msg]
  (if (= "376" (:command msg)) ;; motdが終わった
    (do
      (doseq [channel (env :init-channels)]
        (print-command writer "JOIN :" channel "\r\n"))
      [(assoc env :mode :loop) msg])
    [env msg]
    ))

;; るーぷ
(defmethod irc-process :loop [env writer msg]
  (if (= "PRIVMSG" (:command msg))
    (doseq [[_ filter] @privmsg-filters]
      (let [value (str (filter ((dp (prefix->client (msg :prefix))) :nick)
                               (nth (msg :params) 1)))]
        (if (not (= value ""))
          (print-command writer "NOTICE #develop :" value "\r\n")))))
  [env msg]
  )

(defn irc-response
  [env writer msg]
  (pprint msg)
  (flush)
  (if (= "PING" (:command msg))
    (print-command writer (str "PONG :" (env :server) "\r\n")))
  (irc-process env writer msg))
