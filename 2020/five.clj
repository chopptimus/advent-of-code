(require '[clojure.string :as string])

(defn ->long [s]
  (let [str-num (string/replace (string/replace s #"[FL]" "0") #"[BR]" "1")]
    (if (string/blank? str-num)
      0
      (Long/parseLong str-num 2))))

(defn seat-id [seat]
  (let [row (subs seat 0 7)
        col (subs seat 7)]
    (+ (* (->long row) 8) (->long col))))

;; part 1
#_(apply max (map seat-id *input*))

;; part 2
(->> *input*
     (map seat-id)
     sort
     (partition 2 1)
     (filter (fn [[a b]]
               (not= (- b a) 1)))
     first)
