(ns aoc.fourteen)

(defn substitute
  [reactions [quantity element :as qe]]
  (if (= element "ORE")
    [qe]
    (let [reaction (get reactions element)
          min-quantity (first reaction)
          constituents (rest reaction)
          multiplier (inc (quot (dec quantity) min-quantity))]
      (mapv (fn [[n chem]] [(* n multiplier) chem]) constituents))))

(defn remaining-chem
  [reactions [quantity element]]
  (if-some [min-quantity (first (get reactions element))]
    (let [m (mod quantity min-quantity)]
      (if (zero? m)
        0
        (- min-quantity m)))
    0))

(defn simplify
  [reaction]
  (->> (group-by second reaction)
       (mapv (fn [[chem quantities]]
               [(reduce + (map first quantities)) chem]))))

(defn consume-leftovers
  [reaction leftovers]
  (loop [reaction reaction
         new-reaction []
         leftovers leftovers]
    (if-some [[quantity chem :as term] (first reaction)]
      (if-some [rem (get leftovers chem)]
        (cond
          (> quantity rem)
          (recur (rest reaction) (conj new-reaction [(- quantity rem) chem])
                 (dissoc leftovers chem))
          (< quantity rem)
          (recur (rest reaction) new-reaction (update leftovers chem - quantity))
          :else (recur (rest reaction) new-reaction (dissoc leftovers chem)))
        (recur (rest reaction) (conj new-reaction term) leftovers))
      [new-reaction leftovers])))

(defn substutite-once
  [reactions reaction]
  (loop [reaction reaction
         new-reaction []]
    (if-some [term (first reaction)]
      (let [new-reaction (into new-reaction (substitute reactions term))
            remaining (remaining-chem reactions term)]
        (recur reaction (conj new-reaction [(- remaining) (second term)])))
      new-reaction)))

(defn multiply-reaction
  [reaction x]
  (loop [old-reaction (rest reaction)
         new-reaction []]
    (if-some [[n chem] (first old-reaction)]
      (recur (rest old-reaction) (conj new-reaction [(* n x) chem]))
      (into [(first reaction)] new-reaction))))

(defn ore-per-fuel
  [reactions]
  (loop [reaction (rest (get reactions "FUEL"))]
    (if (= (count reaction) 1)
      (first (first reaction))
      (let [reaction (substutite-once reactions reaction)]
        (recur (simplify reaction))))))

(defn binary-search
  [reactions lo hi]
  (let [mid (quot (+ hi lo) 2)
        required-ore (ore-per-fuel
                      (update reactions "FUEL" multiply-reaction mid))]
    (if (< 999999000000 required-ore 1000001000000)
      mid
      (if (< required-ore 1e12)
        (recur reactions mid hi)
        (recur reactions lo mid)))))

(comment

  (require '[clojure.string :as string])

  (def test-input
"9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL")

  (def test-input-2
"157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT")

  (def input (slurp "fourteen.txt"))

  (def reactions
    (->> (string/split input #"\n")
         (map (fn [s]
                (re-seq #"([0-9]+) ([A-Z]+)" s)))
         (map (fn [vs]
                (let [chems (->> (map #(rest %) vs)
                                 (map (fn [[n chem]] [(read-string n) chem]))
                                 reverse)
                      quantity (first (first chems))
                      key (second (first chems))]
                  [key (vec (conj (rest chems) quantity))])))
         (into {})))

  (def guess (binary-search reactions 0 1e12))

  (ore-per-fuel (update reactions "FUEL" multiply-reaction (+ guess 5)))

  )
