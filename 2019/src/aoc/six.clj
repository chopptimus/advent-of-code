(ns aoc.six
  (:require [clojure.string :as string]))

(defn total-orbits
  ([map] (total-orbits map "COM" 0 0))
  ([map key sum n]
   (if-some [children (get map key)]
     (loop [cs children s sum]
       (if-some [c (first cs)]
         (recur (rest cs) (total-orbits map c (+ s n 1) (inc n)))
         s))
     sum)))

(defn path-to
  ([target map] (path-to target map "COM" []))
  ([target map key path]
   (if (= key target)
     path
     (if-some [children (get map key)]
       (let [new-path (conj path key)]
         (loop [cs children]
           (if-some [c (first cs)]
             (let [path? (path-to target map c new-path)]
               (if path?
                 path?
                 (recur (rest cs))))
             nil)))
       nil))))

(defn str->graph
  [s]
  (->> (string/split s #"\n")
       (map (comp #(string/split % #"\)") string/trim))
       (reduce (fn [m [k v]] (update m k (fnil conj []) v)) {})))

(comment

  (def puzzle-input (str->graph (slurp "six.txt")))

  (def test-text
    "COM)B
    B)C
    C)D
    D)E
    E)F
    B)G
    G)H
    D)I
    E)J
    J)K
    K)L
    K)YOU
    I)SAN")

  (def test-input (str->graph test-text))

  (def you-path (path-to "YOU" puzzle-input))
  (def san-path (path-to "SAN" puzzle-input))
  
  (def divergence-point
    (.indexOf
     you-path
     (loop [paths (map vector you-path san-path)]
       (if-some [[a b] (first paths)]
         (if (not= a b)
           a
           (recur (rest paths)))
         nil))))

  (def path-to-you (subvec you-path divergence-point))
  (def path-to-san (subvec san-path divergence-point))

  (+ (count path-to-you) (count path-to-san))

  )
