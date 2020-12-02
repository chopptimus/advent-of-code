(ns aoc.fifteen
  (:require [aoc.intcode :as ic]))

(defn new-position
  [[x y] direction]
  (case direction
    1 [x (dec y)]
    2 [x (inc y)]
    3 [(dec x) y]
    4 [(inc x) y]))

(defn move
  [interp direction]
  (let [new-interp (ic/run-until-input-or-halt (update interp :in conj direction))]
    (if (zero? (first (:out new-interp)))
      new-interp
      (update new-interp :position new-position direction))))

(defn explore
  [interp]
  (loop [interps (apply list (map #(move interp %) [1 2 3 4]))
         squares {}]
    (if-some [{:keys [position] :as interp} (peek interps)]
      (if (contains? squares position)
        (recur (pop interps) squares)
        (recur (->> [1 2 3 4]
                    (map #(move (dissoc interp :out) %))
                    (into (pop interps)))
               (assoc squares position (first (:out interp)))))
      squares)))

(defn neighbors
  [position]
  (map #(new-position position %) [1 2 3 4]))

(defn shortest-path
  [maze start target]
  (loop [distances (assoc (into {} (map vector maze (repeat ##Inf))) start 0)
         visited? #{}
         current start]
    (if (= current target)
      (get distances current)
      (let [current-distance (inc (get distances current))
            new-distances (merge distances
                                 (->> (neighbors current)
                                      (filter distances)
                                      (remove visited?)
                                      (select-keys distances)
                                      (map (fn [[k v]] [k (min v current-distance)]))
                                      (into {})))]
        (recur new-distances
               (conj visited? current)
               (->> (remove (comp visited? key) new-distances)
                    (apply min-key val)
                    key))))))

(comment

  (do
    (require '[clojure.string :as string])
    (def program (mapv read-string (string/split (slurp "fifteen.txt") #","))))

  (def maze (explore {:program program :pc 0 :position [0 0]}))
  (def oxygen-location (key (first (filter (comp #{2} val) maze))))

  ; part 1
  (time (shortest-path (keys maze) [0 0] (key (first (filter (comp #{2} val) maze)))))

  (def squares (keys maze))
  ; part 2
  (apply max (pmap #(shortest-path squares oxygen-location %) squares))

  )
