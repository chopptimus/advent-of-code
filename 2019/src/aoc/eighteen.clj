(ns aoc.eighteen
  (:require [clojure.string :as string]))

(defn door->key
  [key]
  (char (+ (int key) 32)))

(defn next-positions
  [[x y]]
  [[x (dec y)]
   [x (inc y)]
   [(dec x) y]
   [(inc x) y]])

(defn reachable-keys
  [maze collected start]
  (loop [queue (conj (clojure.lang.PersistentQueue/EMPTY) start)
         distances {start 0}
         keys nil]
    (if-some [p (first queue)]
      (let [nd (inc (get distances p))
            [queue distances keys]
            (loop [nps (next-positions p)
                   q (pop queue)
                   ds distances
                   ks keys]
              (if-some [np (first nps)]
                (let [ch (get-in maze np)]
                  (cond
                    (or (#{nil \#} ch) (contains? distances np)
                        (and (Character/isUpperCase ch)
                             (not (collected (Character/toLowerCase ch)))))
                    (recur (rest nps) q ds ks)
                    (and (Character/isLowerCase ch) (not (collected ch)))
                    (recur (rest nps) q (assoc ds np nd) (assoc ks ch [nd np]))
                    :else (recur (rest nps) (conj q np) (assoc ds np nd) ks)))
                [q ds ks]))]
        (recur queue distances keys))
      keys)))

(defn reachable-multi
  [maze collected starts]
  (apply merge (map-indexed (fn [i start]
                              (->> (reachable-keys maze collected start)
                                   (map (fn [[k [x y]]] [k [x y i]]))
                                   (into {})))
                            starts)))

(declare walk)

(defn walk*
  [maze collected starts]
  (let [reachable (reachable-multi maze collected starts)]
    (if (seq reachable)
      (apply min (map (fn [[k [dist p robot]]]
                        (+ dist (walk maze (conj collected k)
                                      (assoc starts robot p))))
                      reachable))
      0)))

(def walk (memoize walk*))

(defn str->maze
  [s]
  (->> (string/split (string/trim s) #"\n")
       (map (comp vec string/trim))
       vec))

(comment

  (do
    (require '[clojure.string :as string])

    (def maze (str->maze (slurp "eighteen1.txt")))

    (def test-maze-string
      "########################
      #...............b.C.D.f#
      #.######################
      #.....@.a.B.c.d.A.e.F.g#
      ########################")

    (def maze (str->maze test-maze-string))

    )

  (reachable-multi maze #{} [[39 39] [39 41] [41 39] [41 41]])

  (walk (str->maze (slurp "eighteen1.txt")) #{} [[40 40]])
  (walk (str->maze (slurp "eighteen2.txt")) #{} [[39 39] [39 41] [41 39] [41 41]])

  )
