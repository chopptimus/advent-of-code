(require '[clojure.string :as string])

(defn passes-part-1?
  [min max char pw]
  (<= min (count (filter #(= char %) pw)) max))

(defn passes-part-2?
  [p1 p2 char pw]
  (let [len (count pw)
        c1 (first (subs pw (dec p1) p1))
        c2 (first (subs pw (dec p2) p2))
        p (= char c1)
        q (= char c2)]
    (and (or p q) (not (and p q)))))

(defn line-passes?
  [line passes?-fn]
  (let [[min-max char pw] (string/split line #" ")
        [min max] (map read-string (string/split min-max #"-"))]
    (passes?-fn min max (first char) pw)))

(defn part-one
  [lines]
  (count (filter #(line-passes? % passes-part-1?) lines)))

(defn part-two
  [lines]
  (count (filter #(line-passes? % passes-part-2?) lines)))

#_(part-one *input*)
(part-two *input*)
