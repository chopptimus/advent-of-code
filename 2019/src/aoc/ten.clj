(ns aoc.ten
  (:require [clojure.string :as string])
  (:import [java.lang Math]))

(defn string->asteroid-map
  [s]
  (mapv (comp vec string/trim) (string/split s #"\n")))

(defn asteroid-positions
  [asteroid-map]
  (for [i (range (count (first asteroid-map)))
        j (range (count asteroid-map))
        :when (= (nth (nth asteroid-map j) i) \#)]
    [i j]))

(defn gradient
  [[x1 y1] [x2 y2]]
  (if (= x1 x2)
    nil
    (/ (- y1 y2) (- x1 x2))))

(defn direction
  [a b]
  (let [dx (- (first b) (first a))]
    (cond
      (zero? dx) (if (neg? (- (second b) (second a))) -1 1)
      (neg? dx) -1
      :else 1)))

(defn asteroids-above
  [positions center]
  (->> (filter #(neg? (direction center %)) positions)
       (filter (comp nil? #(gradient center %)))
       (sort-by second (comp - compare))))

(defn asteroids-below
  [positions center]
  (->> (filter #(pos? (direction center %)) positions)
       (filter (comp nil? #(gradient center %)))
       (sort-by second)))

(defn distance
  [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))

(defn asteroids-left
  [positions center]
  (->> (filter #(neg? (direction center %)) positions)
       (filter (comp some? #(gradient center %)))
       (group-by #(gradient center %))
       sort
       (map (comp (fn [points]
                    (sort-by #(distance center %) points))
                  second))))

(defn asteroids-right
  [positions center]
  (->> (filter #(pos? (direction center %)) positions)
       (filter (comp some? #(gradient center %)))
       (group-by #(gradient center %))
       sort
       (map (comp (fn [points]
                    (sort-by #(distance center %) points))
                  second))))

(defn visible-asteroids
  [positions center]
  (let [left (count (asteroids-left positions center))
        right (count (asteroids-right positions center))
        above (if (seq (asteroids-above positions center)) 1 0)
        below (if (seq (asteroids-below positions center)) 1 0)]
    (+ left right above below)))

(defn spin
  [above right below left]
  (if (first (filter seq [above right below left]))
    (lazy-seq (cons (filter some? (-> (conj [] (first above))
                                      (into (map first right))
                                      (conj (first below))
                                      (into (map first left))))
                    (spin (rest above)
                          (filter seq (map rest right))
                          (rest below)
                          (filter seq (map rest left)))))
    nil))

(comment

  (def test-input-1
"......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####")

  (def test-input-2
".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..")

  (def test-input-3
".#..#
.....
#####
....#
...##")

  (def input
"#.#.##..#.###...##.#....##....###
...#..#.#.##.....#..##.#...###..#
####...#..#.##...#.##..####..#.#.
..#.#..#...#..####.##....#..####.
....##...#.##...#.#.#...#.#..##..
.#....#.##.#.##......#..#..#..#..
.#.......#.....#.....#...###.....
#.#.#.##..#.#...###.#.###....#..#
#.#..........##..###.......#...##
#.#.........##...##.#.##..####..#
###.#..#####...#..#.#...#..#.#...
.##.#.##.........####.#.#...##...
..##...#..###.....#.#...#.#..#.##
.#...#.....#....##...##...###...#
###...#..#....#............#.....
.#####.#......#.......#.#.##..#.#
#.#......#.#.#.#.......##..##..##
.#.##...##..#..##...##...##.....#
#.#...#.#.#.#.#..#...#...##...#.#
##.#..#....#..##.#.#....#.##...##
...###.#.#.......#.#..#..#...#.##
.....##......#.....#..###.....##.
........##..#.#........##.......#
#.##.##...##..###.#....#....###.#
..##.##....##.#..#.##..#.....#...
.#.#....##..###.#...##.#.#.#..#..
..#..##.##.#.##....#...#.........
#...#.#.#....#.......#.#...#..#.#
...###.##.#...#..#...##...##....#
...#..#.#.#..#####...#.#...####.#
##.#...#..##..#..###.#..........#
..........#..##..#..###...#..#...
.#.##...#....##.....#.#...##...##")

  (def big-test-input
".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")

  (def test-map-1 (string->asteroid-map test-input-1))
  (def test-map-2 (string->asteroid-map test-input-2))
  (def test-map-3 (string->asteroid-map test-input-3))
  (def big-map (string->asteroid-map big-test-input))
  (def asteroid-map (string->asteroid-map input))

  (def positions (set (asteroid-positions asteroid-map)))
  (apply max (map #(visible-asteroids (disj positions %) %) positions))
  (apply max-key #(visible-asteroids (disj positions %) %) positions)

  (def center [22 28])

  (let [left (asteroids-left positions center)
        right (asteroids-right positions center)
        below (asteroids-below positions center)
        above (asteroids-above positions center)]
    (nth (apply concat (spin above right below left)) 199))

)
