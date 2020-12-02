(ns aoc.sixteen
  (:require [clojure.core.matrix :as m])
  (:import [java.lang Math]))

(defn str->column-vector
  [s]
  (mapv (comp vector read-string str) s))

(defn pattern
  [x]
  (let [pattern (concat (repeat x 0)
                        (repeat x 1) 
                        (repeat x 0) 
                        (repeat x 2))]
    (drop 1 (cycle pattern))))

(defn phase
  [transformation-matrix input]
  (m/emap #(mod (Math/abs %) 10) (m/mmul transformation-matrix input)))

(defn simulate
  [input]
  (let [transformation (mapv (fn [x]
                               (vec (take (count input) (pattern x))))
                             (range 1 (inc (count input))))]
    (iterate #(phase transformation %) input)))

(defn part-2-phase
  [input]
  (loop [next-phase []
         current (reduce + input)
         i 0]
    (if (= i (count input))
      next-phase
      (recur (conj next-phase (mod (Math/abs current) 10))
             (- current (nth input i))
             (inc i)))))

(comment

  (def basic-input (str->column-vector "12345678"))
  (def test-input (str->column-vector "80871224585914546619083218645595"))
  (def input (str->column-vector (apply str (butlast (slurp "sixteen.txt")))))
  (def part-2-input (into [] cat (repeat 1e4 (mapv (comp read-string str) (apply str (butlast (slurp "sixteen.txt")))))))

  (def offset (read-string (apply str (subvec part-2-input 0 7))))

  (last (take 101 (simulate input)))
  (take 8 (map str (last (take 101 (iterate part-2-phase (subvec part-2-input offset))))))
  
  )
