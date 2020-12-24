;; part 1
#_
(let [{one 1 three 3} (->> *input*
                           (map read-string)
                           sort
                           (partition 2 1)
                           (map (fn [[a b]] (- b a)))
                           (reduce (fn [counts diff]
                                     (update counts diff (fnil inc 1)))
                                   {}))]
  (* one three))

;; part 2
(->> *input*
     (map read-string)
     sort
     (reduce (fn [mem x]
               (cons [x (->> (take-while (fn [[a _]] (< (- x a) 4)) mem)
                             (map second)
                             (apply +))]
                     mem))
             '([0 1]))
     first
     second)
