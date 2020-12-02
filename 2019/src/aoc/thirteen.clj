(ns aoc.thirteen
  (:require [clojure.string :as string]
            [aoc.intcode :as ic]))

(defn ball-position
  [positions]
  (first (filter #(= (nth % 2) 4) positions)))

(defn paddle-position
  [positions]
  (first (filter #(= (nth % 2) 3) positions)))

(defn joystick-position
  [[px _] [bx _]]
  (if (< bx px)
    -1
    (if (= bx px)
      0
      1)))

(defn breakout
  [interp paddle]
  (if (:halted? interp)
    (:out interp)
    (let [positions (partition 3 (:out interp))
          ball (ball-position positions)
          new-paddle (paddle-position positions)
          paddle (if new-paddle new-paddle paddle)
          joystick (joystick-position paddle ball)]
      (recur (ic/run-until-input-or-halt (-> (update interp :in conj joystick)
                                             (dissoc :out)))
             paddle))))

(comment

  (def program-1
    (mapv (comp read-string string/trim) (string/split (slurp "thirteen.txt") #",")))

  (def program-2
    (assoc
     (mapv (comp read-string string/trim) (string/split (slurp "thirteen.txt") #","))
     0 2))

  (breakout
   (-> (ic/run-until-input-or-halt {:program program-2
                                    :pc 0
                                    :in [0]})
       (assoc :out [17 18 4]))
   [18 20])

  )
