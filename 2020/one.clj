(let [input (mapv read-string *input*)
      c (count input)]
  (first (for [i (range c)
               j (range (inc i) c)
               z (subvec input j)
               :let [x (nth input i)
                     y (nth input j)]
               :when (= 2020 (+ x y z))]
           (* x y z))))
