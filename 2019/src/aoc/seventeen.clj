(ns aoc.seventeen
  (:require [aoc.intcode :as ic]))

(defn get-grid
  [grid x y]
  (get (get grid y) x))

(defn shape
  [grid [x y]]
  (str (get-grid grid x (dec y)) ":"
       (get-grid grid (dec x) y)
       (get-grid grid x y)
       (get-grid grid (inc x) y) ":"
       (get-grid grid x (inc y))))

(defn points
  [grid]
  (for [x (range (count (first grid)))
        y (range (count grid))]
    [x y]))

(def intersection? #{"#:###:#"})
(def vertex? #{"#:##.:."
               "#:.##:."
               ".:.##:#"
               ".:##.:#"
               ":##.:#"
               ":.##:#"
               "#:##.:"
               "#:.##:"
               ".:##:#"
               "#:##:."})

(comment

  (do
    (require '[clojure.string :as string])
    (def program (mapv
                  read-string
                  (string/split (string/trim (slurp "seventeen.txt")) #","))))

  (defn draw-scaffold
    [scaffold]
    (println (string/join (map char scaffold))))

  (def scaffold (:out (ic/run-until-input-or-halt {:program program :pc 0})))
  (def grid (mapv vec (string/split (string/join (map char scaffold)) #"\n")))

  ; part 1
  (->> (points grid)
       (filter (comp intersection? #(shape grid %)))
       (map #(apply * %))
       (reduce +))

  (->> (points grid)
       (filter (comp vertex? #(shape grid %)))
       (group-by second)
       sort)

  (def path [\L 12
             \L 6
             \L 8
             \R 6
             \L 8
             \L 8
             \R 4
             \R 6
             \R 6
             \L 12
             \L 6
             \L 8
             \R 6
             \L 8
             \L 8
             \R 4
             \R 6
             \R 6
             \L 12
             \R 6
             \L 8
             \L 12
             \R 6
             \L 8
             \L 8
             \L 8
             \R 4
             \R 6
             \R 6
             \L 12
             \L 6
             \L 8
             \R 6
             \L 8
             \L 8
             \R 4
             \R 6
             \R 6
             \L 12
             \R 6
             \L 8])

  (defn vector->input
    [v]
    (conj
     (into [] cat (map (fn [x]
                         (if (int? x)
                           (map int (str x))
                           [(int x)]))
                       (butlast (interleave v (repeat \,)))))
     10))

  (do 
    (def a [\L 12 \L 6 \L 8 \R 6])
    (def b [\L 8 \L 8 \R 4 \R 6 \R 6])
    (def c [\L 12 \R 6 \L 8])

    (def A (vector->input a))
    (def B (vector->input b))
    (def C (vector->input c))

    (def ascii-program (vector->input [\A \B \A \B \C \C \B \A \B \C])))

  (def input (into [] cat [ascii-program A B C [(int \n) 10]]))

  (draw-scaffold
   (:out (ic/run-until-input-or-halt {:program (assoc program 0 2)
                                      :pc 0
                                      :in input})))

  )
