(ns aoc.seven
  (:require [clojure.math.combinatorics :as combo]
            [aoc.intcode :as ic]))

(defn run-until-output
  [interp]
  (let [new-interp (ic/execute-instruction interp)]
    (if (:out new-interp)
      new-interp
      (recur new-interp))))

(defn run-circuit
  [program phase-settings]
  (loop [input 0 phase-settings phase-settings]
    (if-some [phase-setting (first phase-settings)]
      (recur (first (:out (run-until-output {:program program
                                             :pc 0
                                             :in [phase-setting input]})))
             (rest phase-settings))
      input)))

(defn new-interp
  [program phase-setting]
  {:program program
   :pc 0
   :in [phase-setting]})

(defn feedback-loop
  [program phase-settings]
  (loop [amps (mapv new-interp (repeat program) phase-settings)
         input 0
         i 0]
    (let [interp (nth amps i)
          with-input (update interp :in conj input)
          new-interp (ic/run-until-input-or-halt with-input)]
      (if (and (:halted? new-interp) (= i 4))
        (first (:out new-interp))
        (recur (assoc amps i (dissoc new-interp :out))
               (first (:out new-interp))
               (mod (inc i) 5))))))

(comment

  (def program
    [3 8 1001 8 10 8 105 1 0 0 21 38 63 76 93 118 199 280 361 442 99999 3 9 101
     3 9 9 102 3 9 9 101 4 9 9 4 9 99 3 9 1002 9 2 9 101 5 9 9 1002 9 5 9 101 5
     9 9 1002 9 4 9 4 9 99 3 9 101 2 9 9 102 3 9 9 4 9 99 3 9 101 2 9 9 102 5 9
     9 1001 9 5 9 4 9 99 3 9 102 4 9 9 1001 9 3 9 1002 9 5 9 101 2 9 9 1002 9 2
     9 4 9 99 3 9 1002 9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001 9 1 9 4 9 3 9 1001
     9 1 9 4 9 3 9 1001 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 101 2 9 9 4 9 3 9 1002
     9 2 9 4 9 3 9 1001 9 1 9 4 9 3 9 101 2 9 9 4 9 99 3 9 102 2 9 9 4 9 3 9
     1002 9 2 9 4 9 3 9 1001 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 101 1 9 9 4 9 3 9
     102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 102 2 9 9 4 9 3 9
     1001 9 1 9 4 9 99 3 9 101 1 9 9 4 9 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3
     9 101 2 9 9 4 9 3 9 1001 9 2 9 4 9 3 9 1002 9 2 9 4 9 3 9 1002 9 2 9 4 9 3
     9 102 2 9 9 4 9 3 9 1001 9 1 9 4 9 3 9 1002 9 2 9 4 9 99 3 9 1001 9 1 9 4 9
     3 9 102 2 9 9 4 9 3 9 102 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 2 9 4 9 3
     9 102 2 9 9 4 9 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 101 1 9 9 4 9 3 9
     1001 9 2 9 4 9 99 3 9 1002 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9 101 2 9 9 4 9 3
     9 101 1 9 9 4 9 3 9 1002 9 2 9 4 9 3 9 1001 9 2 9 4 9 3 9 102 2 9 9 4 9 3 9
     101 1 9 9 4 9 3 9 101 2 9 9 4 9 3 9 1002 9 2 9 4 9 99])

  (def test-program-1
    [3 26 1001 26 -4 26 3 27 1002 27 2 27 1 27 26  27 4 27 1001 28 -1 28 1005
     28 6 99 0 0 5])

  (def test-program-2
    [3 52 1001 52 -5 52 3 53 1 52 56 54 1007 54 5 55 1005 55 26 1001 54 
     -5 54 1105 1 12 1 53 54 53 1008 54 0 55 1001 55 1 55 2 53 55 53 4 
     53 1001 56 -1 56 1005 56 6 99 0 0 0 0 10])

  (apply max (for [permutation (combo/permutations [0 1 2 3 4])]
               (run-circuit program permutation)))

  (apply max (for [permutation (combo/permutations [5 6 7 8 9])]
               (feedback-loop program permutation)))

  )
