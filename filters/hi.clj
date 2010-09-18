(defn hi-filter [nick msg]
  (if (re-find #"やあ|hi" msg)
    (str "Hi! " nick)))
