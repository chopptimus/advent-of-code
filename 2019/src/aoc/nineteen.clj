(ns aoc.nineteen
  (:require [aoc.intcode :as ic]
            [clojure.string :as string]))

(defn pull?
  [interp [x y]]
  (not (zero? (first (:out (ic/run-until-input-or-halt
                            (assoc interp :in [x y])))))))

(defn bottom-edge
  [interp]
  (letfn [(f [[x y]]
            (let [v1 [x (inc y)]]
              (if (pull? interp v1)
                (lazy-seq (cons v1 (f v1)))
                (let [v2 [(inc x) y]]
                  (lazy-seq (cons v2 (f v2)))))))]
    (lazy-seq (cons [5 4] (f [5 4])))))

(comment

(def program (mapv read-string (string/split (slurp "nineteen.txt") #",")))
(def interp {:program program :pc 0})

;; Part 1
(let [interp {:program program, :pc 0}]
  (count (filter (comp not zero?)
                 (for [i (range 50), j (range 50)]
                   (first (:out (ic/run-until-input-or-halt
                                 (assoc interp :in [i j]))))))))

;; Part 2
(def bottom-corner
  (first (filter (fn [[x y]]
                   (when (pull? interp [(+ x 99) (- y 99)])
                     [x y]))
                 (bottom-edge interp))))

(reduce + (update (update bottom-corner 1 #(- % 99)) 0 #(* 1e4 %)))

;; Fun additional solution: write a huge section of the tractor beam to file
;; and use vim to find the solution.

(require '[clojure.java.io :as io])

(defn binary-search
  [f lo hi]
  (let [mid (quot (+ hi lo) 2)]
    (if (= mid lo)
      hi
      (let [b1 (f mid)
            b2 (f (inc mid))]
        (cond
          (and b1 b2) (recur f lo mid)
          (not (or b1 b2)) (recur f mid hi)
          (and (not b1) b2) (inc mid))))))

(defn row
  [count i]
  (let [j (loop [xs (range (quot i 3) count)]
            (if-some [x (first xs)]
              (if (pull? interp [x i])
                x
                (recur (rest xs)))
              count))
        k (binary-search #(not (pull? interp [% i])) j count)]
    (str (apply str (repeat j \.))
         (apply str (repeat (- k j) \#))
         (apply str (repeat (- count k) \.)))))

(require '[clojure.java.io :as io])

(with-open [w (io/writer "tractor.txt")]
  (doseq [i (range 1500)]
    (.write w (row 1500 i))
    (.newLine w)))

 )
