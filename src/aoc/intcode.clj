(ns aoc.intcode)

(def ^:dynamic *input*)

(defn op-code
  [x]
  (mod x 100))

(defn param-modes
  [x n]
  (loop [n n
         modes (quot x 100)
         params []]
    (if (zero? n)
      params
      (recur (dec n)
             (quot modes 10)
             (conj params (mod modes 10))))))

(def op-code->param-pattern
  {1 [:value :value :pointer]
   2 [:value :value :pointer]
   3 [:pointer]
   4 [:value]
   5 [:value :value]
   6 [:value :value]
   7 [:value :value :pointer]
   8 [:value :value :pointer]})

(defn derefenced-params
  [x memory pc]
  (let [op-code (op-code x)
        pattern (op-code->param-pattern op-code)
        modes (param-modes x (count pattern))]
    (loop [pc (inc pc)
           type-modes (map vector pattern modes)
           params []]
      (if-some [[type mode] (first type-modes)]
        (recur (inc pc)
               (rest type-modes)
               (if (= type :pointer)
                 (conj params (nth memory pc))
                 (if (zero? mode)
                   (conj params (nth memory (nth memory pc)))
                   (conj params (nth memory pc)))))
        params))))

(defn add
  [memory a b location]
  (assoc memory location (+ a b)))

(defn mult
  [memory a b location]
  (assoc memory location (* a b)))

(defn input
  [memory location]
  (assoc memory location *input*))

(defn output
  [memory location]
  (println location)
  memory)

(defn jump-if-true
  [pc x jump]
  (if (not (zero? x))
    jump
    (+ pc 3)))

(defn jump-if-false
  [pc x jump]
  (if (zero? x)
    jump
    (+ pc 3)))

(defn less-than
  [memory a b location]
  (assoc memory location (if (< a b) 1 0)))

(defn equals
  [memory a b location]
  (assoc memory location (if (= a b) 1 0)))

(defn intcode-interpreter
  [memory pc]
  (let [instruction (nth memory pc)
        params (derefenced-params instruction memory pc)]
    (case (op-code instruction)
      1 (recur (apply add memory params) (+ pc 4))
      2 (recur (apply mult memory params) (+ pc 4))
      3 (recur (apply input memory params) (+ pc 2))
      4 (recur (apply output memory params) (+ pc 2))
      5 (recur memory (apply jump-if-true pc params))
      6 (recur memory (apply jump-if-false pc params))
      7 (recur (apply less-than memory params) (+ pc 4))
      8 (recur (apply equals memory params) (+ pc 4))
      99 memory)))

(comment

  (do 
    (def diagnostic-test
      [3 225 1 225 6 6 1100 1 238 225 104 0 1102 68 5 225 1101 71 12 225 1 117
       166 224 1001 224 -100 224 4 224 102 8 223 223 101 2 224 224 1 223 224 223
       1001 66 36 224 101 -87 224 224 4 224 102 8 223 223 101 2 224 224 1 223 224
       223 1101 26 51 225 1102 11 61 224 1001 224 -671 224 4 224 1002 223 8 223
       1001 224 5 224 1 223 224 223 1101 59 77 224 101 -136 224 224 4 224 1002
       223 8 223 1001 224 1 224 1 223 224 223 1101 11 36 225 1102 31 16 225 102
       24 217 224 1001 224 -1656 224 4 224 102 8 223 223 1001 224 1 224 1 224 223
       223 101 60 169 224 1001 224 -147 224 4 224 102 8 223 223 101 2 224 224 1
       223 224 223 1102 38 69 225 1101 87 42 225 2 17 14 224 101 -355 224 224 4
       224 102 8 223 223 1001 224 2 224 1 224 223 223 1002 113 89 224 101 -979
       224 224 4 224 1002 223 8 223 1001 224 7 224 1 224 223 223 1102 69 59 225 4
       223 99 0 0 0 677 0 0 0 0 0 0 0 0 0 0 0 1105 0 99999 1105 227 247 1105 1
       99999 1005 227 99999 1005 0 256 1105 1 99999 1106 227 99999 1106 0 265
       1105 1 99999 1006 0 99999 1006 227 274 1105 1 99999 1105 1 280 1105 1
       99999 1 225 225 225 1101 294 0 0 105 1 0 1105 1 99999 1106 0 300 1105 1
       99999 1 225 225 225 1101 314 0 0 106 0 0 1105 1 99999 7 677 677 224 1002
       223 2 223 1006 224 329 1001 223 1 223 1007 226 226 224 1002 223 2 223 1006
       224 344 1001 223 1 223 1108 226 677 224 102 2 223 223 1005 224 359 1001
       223 1 223 1107 226 677 224 1002 223 2 223 1006 224 374 101 1 223 223 1107
       677 226 224 1002 223 2 223 1006 224 389 101 1 223 223 7 226 677 224 1002
       223 2 223 1005 224 404 101 1 223 223 1008 677 226 224 102 2 223 223 1005
       224 419 101 1 223 223 1008 226 226 224 102 2 223 223 1006 224 434 101 1
       223 223 107 226 226 224 1002 223 2 223 1005 224 449 1001 223 1 223 108 226
       677 224 102 2 223 223 1005 224 464 101 1 223 223 1108 677 226 224 102 2
       223 223 1005 224 479 101 1 223 223 1007 226 677 224 102 2 223 223 1006 224
       494 101 1 223 223 107 677 677 224 102 2 223 223 1005 224 509 101 1 223 223
       108 677 677 224 102 2 223 223 1006 224 524 1001 223 1 223 8 226 677 224
       102 2 223 223 1005 224 539 101 1 223 223 107 677 226 224 102 2 223 223
       1005 224 554 1001 223 1 223 8 226 226 224 102 2 223 223 1006 224 569 1001
       223 1 223 7 677 226 224 1002 223 2 223 1005 224 584 1001 223 1 223 1108
       226 226 224 102 2 223 223 1005 224 599 1001 223 1 223 1107 677 677 224
       1002 223 2 223 1006 224 614 1001 223 1 223 1007 677 677 224 1002 223 2 223
       1006 224 629 1001 223 1 223 108 226 226 224 102 2 223 223 1005 224 644
       1001 223 1 223 8 677 226 224 1002 223 2 223 1005 224 659 1001 223 1 223
       1008 677 677 224 1002 223 2 223 1006 224 674 1001 223 1 223 4 223 99 226])
    (def multiply [1002 4 3 4 33])
    (def negative-numbers [1101 100 -1 4 0]))

  (binding [*input* 5]
    (intcode-interpreter diagnostic-test 0))

  )
