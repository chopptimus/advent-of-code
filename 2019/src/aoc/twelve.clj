(ns aoc.twelve
  (:require [clojure.string :as string])
  (:import [java.lang Math]))

(defn string->positions
  [s]
  (let [rows (map string/trim (string/split s #"\n"))]
    (map (fn [coords]
           (vec (map (comp read-string second) coords)))
         (map #(re-seq #"[xyz]=(-?[0-9]+)" %) rows))))

(defn new-velocity
  [a b velocity]
  (map + (map compare b a) velocity))

(defn apply-gravity
  [bodies body velocity]
  (loop [bodies bodies, velocity velocity]
    (if-some [f (first bodies)]
      (recur (rest bodies) (new-velocity body f velocity))
      velocity)))

(defn time-step
  [bodies velocities]
  (let [new-velocities (map (partial apply-gravity bodies) bodies velocities)]
    [(map #(map + %1 %2) bodies new-velocities) new-velocities]))

(defn energy
  [body velocity]
  (* (reduce + (map #(Math/abs %) body)) (reduce + (map #(Math/abs %) velocity))))

(defn euclid
  [a b]
  (let [rem (mod a b)]
    (if (zero? rem)
      b
      (recur b rem))))

(defn lcm
  [a b]
  (let [gcd (euclid a b)]
    (* (/ a gcd) b)))

(comment

  (def bodies (string->positions (slurp "twelve.txt")))
  (def velocities (take (count bodies) (repeat [0 0 0])))

  (def test-bodies (string->positions "<x=-1, y=0, z=2>
                                      <x=2, y=-10, z=-7>
                                      <x=4, y=-8, z=8>
                                      <x=3, y=5, z=-1>"))

  (def test-bodies-2 (string->positions "<x=-8, y=-10, z=0>
                                        <x=5, y=5, z=10>
                                        <x=2, y=-7, z=3>
                                        <x=9, y=-8, z=-3>"))

  (apply-gravity test-bodies (first test-bodies) [0 0 0])

  (defn repeated-axis?
    [bodies velocities]
    (fn [bodies* velocities*]
      (and (= bodies bodies*) (= velocities velocities*))))

  (def x-repeated? (repeated-axis? (map first bodies) (map first velocities)))
  (def x-period (loop [[bodies velocities] [bodies velocities]
                       n 10
                       i 0]
                  (if (and (not= i 0) (x-repeated? (map first bodies) (map first velocities)))
                    i
                    (do
                      (doseq [b bodies]
                        (doall b))
                      (doseq [v velocities]
                        (doall v))
                      (recur (time-step bodies velocities) n (inc i))))))

  (def y-repeated? (repeated-axis? (map second bodies) (map second velocities)))
  (def y-period (loop [[bodies velocities] [bodies velocities]
                       n 10
                       i 0]
                  (if (and (not= i 0) (y-repeated? (map second bodies) (map second velocities)))
                    i
                    (do
                      (doseq [b bodies]
                        (doall b))
                      (doseq [v velocities]
                        (doall v))
                      (recur (time-step bodies velocities) n (inc i))))))

  (defn third
    [v]
    (nth v 2))

  (def z-repeated? (repeated-axis? (map third bodies) (map third velocities)))
  (def z-period (loop [[bodies velocities] [bodies velocities]
                       n 10
                       i 0]
                  (if (and (not= i 0) (z-repeated? (map third bodies) (map third velocities)))
                    i
                    (do
                      (doseq [b bodies]
                        (doall b))
                      (doseq [v velocities]
                        (doall v))
                      (recur (time-step bodies velocities) n (inc i))))))

  )
