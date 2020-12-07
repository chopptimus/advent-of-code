(defn trees-in-the-way
  [input delta-right delta-down]
  (let [pattern-length (count (first input))]
    (->> (iterate #(mod (+ delta-right %) pattern-length) 0)
         (map nth (map first (partition 1 delta-down input)))
         (filter #(= \# %))
         count)))

;; part 1
#_(trees-in-the-way *input* 3 1)

;; part 2
(apply * (map #(apply trees-in-the-way *input* %) [[1 1]
                                                   [3 1]
                                                   [5 1]
                                                   [7 1]
                                                   [1 2]]))
