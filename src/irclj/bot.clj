(ns irclj.bot
  (:use irclj.core
        clojure.contrib.pprint)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bot

(defn dp [arg]
  (print "-------------------------------------------------------------------------------\n")
  (pprint arg)
  arg)

;; plugin-framework

(defn clj-files-in [dirname]
  (let [files (seq (.listFiles (java.io.File. dirname)))]
    (filter #(re-find #".clj$" (.getPath %)) files)))

(defn update-plugins
  [plugins dirname]
  (reduce (fn [plugins clj]
            (let [plugin (plugins (.getPath clj))]
              (if (or (not plugin) (> (.lastModified clj) (plugin :last-modified)))
                (try
                  (assoc plugins (.getPath clj) {:last-modified (.lastModified clj)
                                                 :proc (var-get (load-file (.getPath clj)))})
                  (catch java.lang.Exception ex
                    (dp (str (.getPath clj) " is not loaded -- " ex))
                    plugins))
                plugins)))
          plugins
          (clj-files-in dirname)))

(defn reload-plugins
  [plugins dirname]
  (dosync (alter plugins update-plugins dirname)))

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
;;  :auth-mode :init / :authorized
;;
;;  :channels {:channel-name <channel-info> ...}
;; }
;;
;; <channel-info>
;;
;; {
;;  :name "#name"
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


(def privmsg-filters (ref {}))
(def privmsg-commands (ref {}))

(def init-env {:auth-mode :init})

(defmulti irc-process (fn [_ _ msg] (:command msg)))

(defn print-command
  [writer cmd & arg]
  (doto writer
    (.print (apply str (cons cmd arg)))
    (.flush)))

(defmethod irc-process :NOTICE
  [env writer msg]
  (if (= (env :auth-mode) :init)
    (let [host (.. java.net.InetAddress getLocalHost getHostName)]
      (print-command writer
                     "NICK " (env :nick) "\r\n"
                     "USER " (env :user) " " host " " host " :" (env :realname) "\r\n")
      (assoc env :auth-mode :authorized))
    env))


(defmethod irc-process :376 ;; ENDOFMOTD
  [env writer msg]
  (doseq [channel (env :init-channels)]
    (print-command writer "JOIN :" channel "\r\n"))
  env)

(defn- apply-plugin
  [plugin writer msg]
  (let [value (str ((plugin :proc) ((prefix->client (msg :prefix)) :nick)
                    (nth (msg :params) 1)))]
    (if (not (= value ""))
      (print-command writer "NOTICE " (first (msg :params)) " :" value "\r\n"))))

(defmethod irc-process :PRIVMSG
  [env writer msg]
  (reload-plugins privmsg-filters "filters")
  (doseq [[_ filter] @privmsg-filters]
    (apply-plugin filter writer msg))

  (reload-plugins privmsg-commands "commands")
  (let [command (str "/" (first (.split (nth (msg :params) 1) " ")) ".clj")]
    (doseq [[name plugin] @privmsg-commands :when (.endsWith name command)]
      (apply-plugin plugin writer msg)))
  env)

(defmethod irc-process :PING
  [env writer msg]
  (print-command writer (str "PONG :" (env :server) "\r\n"))
  env)

(defmethod irc-process :INVITE
  [env writer msg]
  (print-command writer "JOIN :" (nth (msg :params) 1) "\r\n")
  env)

(defmethod irc-process :JOIN
  [env writer msg]
  (let [name (first (msg :params))]
    (assoc-in env [:channels (keyword name)] {:name name})))

(defmethod irc-process :default
  [env writer msg]
  env)

(defn irc-response
  [env writer msg]
  (pprint msg)
  (flush)
  [(irc-process env writer msg) msg])
