(require '[clojure.set :as set]
         '[clojure.string :as string])

#_(->> *input*
     (partition-by string/blank?)
     (remove #(= [""] %))
     (map (fn [answers] (count (apply set/union (map set answers)))))
     (apply +))

(->> *input*
     (partition-by string/blank?)
     (remove #(= [""] %))
     (map (fn [answers] (count (apply set/intersection (map set answers)))))
     (apply +))
