(ns aoc.twenty
  (:require [aoc.maze :as maze]))

(defn- upper? [ch]
  (if ch (Character/isUpperCase ch) false))

(defn- other-portal-letter
  [maze p]
  (->> (maze/next-positions p)
       (keep #(get-in maze %))
       (filter upper?)
       first))

(defn- portal
  [maze p]
  [(get-in maze p) (other-portal-letter maze p)])

(defn- children
  [maze visited path]
  (let [position (peek path)]
    (when-not (upper? (get-in maze position))
      (into
       []
       (keep (fn [p]
               (let [ch (get-in maze p)]
                 (when-not (or (contains? #{\# nil} ch) (visited p))
                   (conj path p)))))
       (maze/next-positions position)))))

(defn- outer-portal?
  [maze [x y]]
  (let [length (count maze)
        width (count (first maze))]
    (or (contains? #{0 1 (dec length) (- length 2)} x)
        (contains? #{0 1 (dec width) (- width 2)} y))))

(defn- all-starting-points
  [maze]
  (for [x (range (count maze))
        y (range (count (first maze)))
        :when (and (= (get-in maze [x y]) \.)
                   (->> (maze/next-positions [x y])
                        (map #(get-in maze %))
                        (some upper?)))]
    [x y]))

(defn- bfs
  [maze start]
  (loop [queue (conj (clojure.lang.PersistentQueue/EMPTY) [start])
         visited #{}
         portals []]
    (if-some [path (first queue)]
      (let [position (peek path)]
        (recur (into (pop queue) (children maze visited path))
               (conj visited position)
               (let [ch (get-in maze position)]
                 (if (upper? ch)
                   (conj portals
                         {:portal (portal maze position)
                          :outer? (outer-portal? maze position)
                          :distance (dec (count path))})
                   portals))))
      portals)))

(defn- portal-next-to-start
  [maze p]
  (portal maze (first (filter
                       (comp upper? #(get-in maze %))
                       (maze/next-positions p)))))

(defn graph
  [maze]
  (into {} (for [start (all-starting-points maze)]
             [(portal-next-to-start maze start)
              (remove (fn [node] (= (:distance node) 1)) (bfs maze start))])))

;; TODO: some Dijkstra magic

(comment

  (def maze (->maze test-2))

  (all-starting-points maze)
  (bfs maze [2 19])

  (do
    (require '[clojure.string :as string])

    (def test-1 "         A           
         A           
  #######.#########  
  #######.........#  
  #######.#######.#  
  #######.#######.#  
  #######.#######.#  
  #####  B    ###.#  
BC...##  C    ###.#  
  ##.##       ###.#  
  ##...DE  F  ###.#  
  #####    G  ###.#  
  #########.#####.#  
DE..#######...###.#  
  #.#########.###.#  
FG..#########.....#  
  ###########.#####  
             Z       
             Z       ")

    (def test-2 "                   A               
                   A               
  #################.#############  
  #.#...#...................#.#.#  
  #.#.#.###.###.###.#########.#.#  
  #.#.#.......#...#.....#.#.#...#  
  #.#########.###.#####.#.#.###.#  
  #.............#.#.....#.......#  
  ###.###########.###.#####.#.#.#  
  #.....#        A   C    #.#.#.#  
  #######        S   P    #####.#  
  #.#...#                 #......VT
  #.#.#.#                 #.#####  
  #...#.#               YN....#.#  
  #.###.#                 #####.#  
DI....#.#                 #.....#  
  #####.#                 #.###.#  
ZZ......#               QG....#..AS
  ###.###                 #######  
JO..#.#.#                 #.....#  
  #.#.#.#                 ###.#.#  
  #...#..DI             BU....#..LF
  #####.#                 #.#####  
YN......#               VT..#....QG
  #.###.#                 #.###.#  
  #.#...#                 #.....#  
  ###.###    J L     J    #.#.###  
  #.....#    O F     P    #.#...#  
  #.###.#####.#.#####.#####.###.#  
  #...#.#.#...#.....#.....#.#...#  
  #.#####.###.###.#.#.#########.#  
  #...#.#.....#...#.#.#.#.....#.#  
  #.###.#####.###.###.#.#.#######  
  #.#.........#...#.............#  
  #########.###.###.#############  
           B   J   C               
           U   P   P               ")

    (def test-3 "             Z L X W       C                 
             Z P Q B       K                 
  ###########.#.#.#.#######.###############  
  #...#.......#.#.......#.#.......#.#.#...#  
  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  
  #.#...#.#.#...#.#.#...#...#...#.#.......#  
  #.###.#######.###.###.#.###.###.#.#######  
  #...#.......#.#...#...#.............#...#  
  #.#########.#######.#.#######.#######.###  
  #...#.#    F       R I       Z    #.#.#.#  
  #.###.#    D       E C       H    #.#.#.#  
  #.#...#                           #...#.#  
  #.###.#                           #.###.#  
  #.#....OA                       WB..#.#..ZH
  #.###.#                           #.#.#.#  
CJ......#                           #.....#  
  #######                           #######  
  #.#....CK                         #......IC
  #.###.#                           #.###.#  
  #.....#                           #...#.#  
  ###.###                           #.#.#.#  
XF....#.#                         RF..#.#.#  
  #####.#                           #######  
  #......CJ                       NM..#...#  
  ###.#.#                           #.###.#  
RE....#.#                           #......RF
  ###.###        X   X       L      #.#.#.#  
  #.....#        F   Q       P      #.#.#.#  
  ###.###########.###.#######.#########.###  
  #.....#...#.....#.......#...#.....#.#...#  
  #####.#.###.#######.#######.###.###.#.#.#  
  #.......#.......#.#.#.#.#...#...#...#.#.#  
  #####.###.#####.#.#.#.#.###.###.#.###.###  
  #.......#.....#.#...#...............#...#  
  #############.#.#.###.###################  
               A O F   N                     
               A A D   M                     ")

    (def input (slurp "twenty.txt"))

    (defn ->maze [s]
      (maze/str->maze (string/replace s #" " "#"))))

  (let [maze (->maze test-2)]
    (bfs maze (maze->children-fn maze) [2 19]))
  )
