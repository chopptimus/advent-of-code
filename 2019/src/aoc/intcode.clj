(ns aoc.intcode)

(defn op-code
  [x]
  (mod x 100))

(def op-code->param-pattern
  {1 [:value :value :pointer]
   2 [:value :value :pointer]
   3 [:pointer]
   4 [:value]
   5 [:value :value]
   6 [:value :value]
   7 [:value :value :pointer]
   8 [:value :value :pointer]
   9 [:value]})

(defn param-modes
  [x]
  (lazy-seq (cons (mod x 10) (param-modes (quot x 10)))))

(defn param-values
  [program pc base instruction pattern]
  (loop [pc (inc pc)
         type-modes (map vector pattern (param-modes (quot instruction 100)))
         params []]
    (if-some [[type mode] (first type-modes)]
      (recur (inc pc)
             (rest type-modes)
             (if (= type :pointer)
               (if (= mode 0)
                 (conj params (nth program pc))
                 (conj params (+ (nth program pc) base)))
               (case mode
                 0 (let [position (nth program pc)]
                     (conj params (if (contains? program position)
                                    (nth program (nth program pc))
                                    0)))
                 1 (conj params (nth program pc))
                 2 (let [position (+ (nth program pc) base)]
                     (conj params (if (contains? program position)
                                    (nth program position)
                                    0))))))
      params)))

(defn extend-assoc
  [v key val]
  (if (contains? v key)
    (assoc v key val)
    (let [fill (inc (- key (count v)))]
      (assoc (into v (take fill (repeat 0))) key val))))

(defn add
  [program a b location]
  (extend-assoc program location (+ a b)))

(defn mult
  [program a b location]
  (extend-assoc program location (* a b)))

(defn input
  [program in location]
  (extend-assoc program location in))

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
  [program a b location]
  (extend-assoc program location (if (< a b) 1 0)))

(defn equals
  [program a b location]
  (extend-assoc program location (if (= a b) 1 0)))

(defn next-instruction
  [interp]
  (nth (:program interp) (:pc interp)))

(defn available-input?
  [interp]
  (boolean (seq (:in interp))))

(defn execute-instruction
  [{:keys [program pc in base] :as interp}]
  (let [instruction (next-instruction interp)
        op-code (op-code instruction)
        params (param-values program pc base instruction
                             (op-code->param-pattern op-code))]
    (case op-code
      1 (assoc interp :program (apply add program params) :pc (+ pc 4))
      2 (assoc interp :program (apply mult program params) :pc (+ pc 4))
      3 (assoc interp
               :program (apply input program (first in) params)
               :pc (+ pc 2)
               :in (rest in))
      4 (assoc (update interp :out (fnil conj []) (first params)) :pc (+ pc 2))
      5 (assoc interp :pc (apply jump-if-true pc params))
      6 (assoc interp :pc (apply jump-if-false pc params))
      7 (assoc interp
               :program (apply less-than program params)
               :pc (+ pc 4))
      8 (assoc interp
               :program (apply equals program params)
               :pc (+ pc 4))
      9 (assoc (update interp :base (fnil + 0) (first params)) :pc (+ pc 2))
      99 (assoc interp :halted? true))))

(defn run-until-input-or-halt
  [interp]
  (if (:halted? interp)
    interp
    (let [instruction (next-instruction interp)]
      (if (and (= (op-code instruction) 3)
               (not (available-input? interp)))
        interp
        (recur (execute-instruction interp))))))
