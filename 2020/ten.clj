;; part 1
(let [{one 1 three 3} (->> *input*
                           (map read-string)
                           sort
                           (partition 2 1)
                           (map (fn [[a b]] (- b a)))
                           (reduce (fn [counts diff]
                                     (update counts diff (fnil inc 1)))
                                   {}))]
  (* one three))
