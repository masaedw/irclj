(ns irclj.core
  "IRC Interface
   パースの部分は nadoka 中の rice を clojure に移植したもの"
  (:use irclj.io
        irclj.util)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse

;; letter     =  %x41-5A / %x61-7A       ; A-Z / a-z
;; digit      =  %x30-39                 ; 0-9
;; hexdigit   =  digit / "A" / "B" / "C" / "D" / "E" / "F"
;; special    =  %x5B-60 / %x7B-7D
;;                  ; "[", "]", "\", "`", "_", "^", "{", "|", "}"
(def LETTER "A-Za-z")
(def DIGIT "\\d")
(def HEXDIGIT (str DIGIT "A-Fa-f"))
(def SPECIAL "\\x5B-\\x60\\x7B-\\x7D")

;; shortname ( letter / digit ) *( letter / digit / "-" )
;;               *( letter / digit )
;;                 ; as specified in RFC 1123 [HNAME]
;; hostname   =  shortname *( "." shortname )
(def SHORTNAME (str "[" LETTER DIGIT "](?:[-" LETTER DIGIT "]*[" LETTER DIGIT "])?"))
(def HOSTNAME (str SHORTNAME "(?:\\." SHORTNAME ")*"))

;; servername =  hostname
(def SERVERNAME HOSTNAME)

;; nickname   =  ( letter / special ) *8( letter / digit / special / "-" )
(def NICKNAME (str "[" LETTER SPECIAL "][-" LETTER DIGIT SPECIAL "]*"))

;; user       =  1*( %x01-09 / %x0B-0C / %x0E-1F / %x21-3F / %x41-FF )
;;                 ; any octet except NUL, CR, LF, " " and "@"
(def USER "[\\x01-\\x09\\x0B-\\x0C\\x0E-\\x1F\\x21-\\x3F\\x41-\\xFF]+")

;; ip4addr    =  1*3digit "." 1*3digit "." 1*3digit "." 1*3digit
(def IP4ADDR (str "[" DIGIT" ]{1,3}(?:\\.[" DIGIT "]{1,3}){3}"))
;; ip6addr    =  1*hexdigit 7( ":" 1*hexdigit )
;; ip6addr    =/ "0:0:0:0:0:" ( "0" / "FFFF" ) ":" ip4addr
(def IP6ADDR (str "(?:[" HEXDIGIT "]+(?::[" HEXDIGIT "]+){7}|0:0:0:0:0:(?:0|FFFF):" IP4ADDR ")"))
;; hostaddr   =  ip4addr / ip6addr
(def HOSTADDR (str "(?:" IP4ADDR "|" IP6ADDR ")"))

;; host       =  hostname / hostaddr
(def HOST (str "(?:" HOSTNAME "|" HOSTADDR ")"))

;; prefix     =  servername / ( nickname [ [ "!" user ] "@" host ] )
(def PREFIX (str "(?:" NICKNAME "(?:(?:!" USER ")?@" HOST ")?|" SERVERNAME ")"))

;; nospcrlfcl =  %x01-09 / %x0B-0C / %x0E-1F / %x21-39 / %x3B-FF
;;                 ; any octet except NUL, CR, LF, " " and ":"
;;(def NOSPCRLFCL "\\x01-\\x09\\x0B-\\x0C\\x0E-\\x1F\\x21-\\x39\\x3B-\\xFF")
(def NOSPCRLFCL "[^\\x00\\x0A\\x0D\\x20\\x3A]")

;; command    =  1*letter / 3digit
(def COMMAND (str "(?:[" LETTER "]+|[" DIGIT "]{3})"))

;; SPACE      =  %x20        ; space character
;; middle     =  nospcrlfcl *( ":" / nospcrlfcl )
;; trailing   =  *( ":" / " " / nospcrlfcl )
;; params     =  *14( SPACE middle ) [ SPACE ":" trailing ]
;;            =/ 14( SPACE middle ) [ SPACE [ ":" ] trailing ]
;;(def MIDDLE (str "[" NOSPCRLFCL "][:" NOSPCRLFCL "]*"))
;;(def TRAILING (str "[: " NOSPCRLFCL "]*"))
(def MIDDLE (str NOSPCRLFCL "(?::|" NOSPCRLFCL ")*"))
(def TRAILING (str "(?:[: ]|" NOSPCRLFCL ")*"))
(def PARAMS (str "(?:((?: +" MIDDLE "){0,14})(?: +:(" TRAILING "))?|((?: +" MIDDLE "){14}):?(" TRAILING "))"))

;; crlf       =  %x0D %x0A   ; "carriage return" "linefeed"
;; message    =  [ ":" prefix SPACE ] command [ params ] crlf
(def CRLF "\\x0D\\x0A")
(def MESSAGE (str "(?::(" PREFIX ") +)?(" COMMAND ")" PARAMS "\\s*(" CRLF "|\\n|\\r)"))

(def CLIENT_PATTERN  (re-pattern (str "^" NICKNAME "(?:(?:!" USER ")?@ "HOST ")$")))
(def MESSAGE_PATTERN (re-pattern (str "^" MESSAGE "$")))

(defn build-message
  [prefix command params]
  {:prefix prefix
   :command command
   :params params})

(defn line->message
  [line]
  (let [[src _1 _2 _3 _4 _5 _6] (re-find MESSAGE_PATTERN line)]
    (cond (nil? src)
            line
          (not (empty? _3))
            (build-message _1 _2 (concat (rest (.split _3 " ")) (list _4))) ;; TODO _4 が nil だとなんかかっこわるいデータになる
          (not (empty? _5))
            (build-message _1 _2 (concat (rest (.split _5 " ")) (list _6)))
          (not (empty? _4))
            (build-message _1 _2 (list _4))
          (not (empty? _6))
            (build-message _1 _2 (list _6))
          :else
            (build-message _1 _2 '())
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration of network and parse

(defn partition-when-ln [bytes]
  (partition-when #(= 10 %) bytes))

(defn partition-when-ln->str [bytes]
  (map byte-seq->str (partition-when-ln bytes)))

(defn msg-seq
  [istream]
  (concat (map line->message (partition-when-ln->str (sock-read istream)))
          (lazy-seq (msg-seq istream))))
