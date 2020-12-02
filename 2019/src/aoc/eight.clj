(ns aoc.eight
  (:require [clojure.string :as string]))

(defn string->digits
  [image]
  (map (comp read-string str) image))

(defn color
  [pixels]
  (case (first pixels)
    0 :black
    1 :white
    2 (recur (rest pixels))))

(defn color->char
  [c]
  (if (= c :white) \0 \space))

(comment 
  
  (def image (string/trim (slurp "eight.txt")))

  (def digits (string->digits image))

  (def colors (partition 25 (->> (partition (* 25 6) digits)
                                 (apply map list)
                                 (map color))))

  (loop [rows colors]
    (if-some [row (first rows)]
      (do
        (println (apply str (map color->char row)))
        (recur (rest rows)))
      nil))
  
  )
(def pixels pixels)

(def c c)
