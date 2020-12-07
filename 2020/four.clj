(require '[clojure.string :as string])

(defn passports
  [input]
  (map
   (fn [lines]
     (apply merge (map #(apply hash-map (string/split % #"[ :]")) lines)))
   (filter #(not= [""] %) (partition-by #(= "" %) input))))

(defn part-1-valid?
  [[{:strs [byr iyr eyr hgt hcl ecl pid]}]]
  (and byr iyr eyr hgt hcl ecl pid))

(defn part-2-valid?
  [{:strs [byr iyr eyr hgt hcl ecl pid]}]
  (and byr iyr eyr hgt hcl ecl pid
       (<= 1920 (read-string byr) 2002)
       (<= 2010 (read-string iyr) 2020)
       (<= 2020 (read-string eyr) 2030)
       (let [len (count hgt)
             units (subs hgt (- len 2))
             mag (subs hgt 0 (- len 2))]
         (and (re-matches #"[1-9][0-9]*" mag)
              (case units
                "cm" (<= 150 (read-string mag) 193)
                "in" (<= 59 (read-string mag) 76)
                false)))
       (re-matches #"#[0-9a-f]{6}" hcl)
       (#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)
       (re-matches #"[0-9]{9}" pid)))

#_(count (filter part-1-valid? (passports *input*)))
(count (filter part-2-valid? (passports *input*)))
