(ns aoc.eleven
  (:require [aoc.intcode :as ic]))

(defn new-direction
  [direction rotation]
  (if (zero? rotation)
    (mod (dec direction) 4)
    (mod (inc direction) 4)))

(defn new-position
  [[x y] direction]
  (case direction
    0 [x (dec y)]
    1 [(inc x) y]
    2 [x (inc y)]
    3 [(dec x) y]))

(defn painted-panels
  [program]
  (loop [{:keys [position] :as robot} {:position [0 0]
                                       :direction 0}
         interp {:program program :pc 0}
         colors {[0 0] 1}]
    (let [new-interp (-> (update interp :in conj (get colors position 0))
                         ic/run-until-input-or-halt)]
      (if (:halted? new-interp)
        colors
        (let [[color rotation] (:out new-interp)
              direction (new-direction (:direction robot) rotation)]
          (recur (assoc robot
                        :position (new-position position direction)
                        :direction direction)
                 (dissoc new-interp :out)
                 (assoc colors position color)))))))

(comment

  (require '[clojure.string :as string])

  (def program
    (mapv read-string (string/split (string/trim (slurp "eleven.txt")) #",")))

  (def colors (painted-panels program))

  (def length (apply max (map first (keys colors))))
  (def height (apply max (map second (keys colors))))

  (defn color->char
    [color]
    (if (zero? color)
      \space
      \#))

  (def char-map
    (partition (inc length) (for [j (range (inc height))
                                  i (range (inc length))]
                              (get colors [i j] 0))))

  (doseq [row char-map]
    (println (apply str (map color->char row))))

  )
