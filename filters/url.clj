(import '(java.net URL))
(use 'irclj.util)
(use 'irclj.io)

(defn- get-title
  [url]
  (let [stream (.openStream (java.net.URL. url))
        html (byte-seq->str (sock-read stream))
        title (nth (re-find #"<\s*?title\s*?>(.*)<\s*?/title\s*?>" html) 1)]
    (print html "\n")
    (print "-------------------------------------------------------------------------------\n")
    (print title "\n")
    title))

(defn- url->info
  [url]
  (let [urlobj (URL. url)
        con (.openConnection urlobj)]
    (.connect con)
    (let [header (.getHeaderFields con)
          content-type (first (get header "Content-Type"))]
      (if (re-find #"html" content-type)
        (or (get-title url) content-type)
        content-type))))

(defn url-filter [nick msg]
  (if-let [url (re-find #"h?ttps?://[^\s]*" msg)]
    (do
      (print url)
      (url->info url))))
