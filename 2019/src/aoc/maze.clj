(ns aoc.maze
  (:require [clojure.string :as string]))

(defn next-positions
  [[x y]]
  [[x (dec y)]
   [x (inc y)]
   [(dec x) y]
   [(inc x) y]])

(defn str->maze
  [s]
  (->> (string/split (string/trim s) #"\n")
       (map (comp vec string/trim))
       vec))
