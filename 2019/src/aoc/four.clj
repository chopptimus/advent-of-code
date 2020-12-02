(ns aoc.four)

(defn valid-password?
  [x]
  (loop [y (quot x 10)
         repeated-digit? false
         previous-digit (mod x 10)]
    (let [last-digit (mod y 10)]
      (cond
        (zero? y) repeated-digit?
        (> last-digit previous-digit) false
        :else (recur (quot y 10)
                     (or repeated-digit? (= last-digit previous-digit))
                     last-digit)))))

(defn valid-password-2?
  [x]
  (let [last (mod x 10)
        second-last (mod (quot x 10) 10)
        third-last (mod (quot (quot x 10) 10) 10)]
    (if (and (= last second-last) (not= last third-last))
      true
      (loop [y (quot (quot (quot x 10) 10) 10)
             a (mod (quot (quot x 10) 10) 10)
             b (mod (quot x 10) 10)
             c (mod x 10)]
        (let [last-digit (mod y 10)]
          (cond
            (zero? y) (and (= a b) (not= c a))
            (and (= a b) (not= a last-digit) (not= a c)) true
            :else (recur (quot y 10) (mod y 10) a b)))))))

(def valid-password-3? (every-pred valid-password? valid-password-2?))

(comment

  (def input (range 146810 612564))

  (def password-count
    (count (filter (every-pred valid-password? valid-password-2?) input)))

  )
