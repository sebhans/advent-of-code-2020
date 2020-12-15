(ns advent-of-code-2020.day15)

(defn- parse-input
  "Parses a comma-separated list of numbers into a vector of numbers."
  [s]
  (->> s
       (re-seq #"\d+")
       (map #(Long/parseLong %))
       vec))

(defn- speak-next-number
  "Speaks the next number given the vector of previously spoken numbers."
  [numbers]
  (let [last-turn (dec (.size numbers))
        most-recently-spoken-number (get numbers last-turn)
        turn-most-recently-spoken-number-was-spoken-before (.lastIndexOf (subvec numbers 0 last-turn) most-recently-spoken-number)
        was-first-time? (= turn-most-recently-spoken-number-was-spoken-before -1)]
    (conj numbers (if was-first-time?
                    0
                    (- last-turn turn-most-recently-spoken-number-was-spoken-before)))))

(defn- number-stream
  "Generates a lazy sequence of number vectors, representing the game state."
  [start-numbers]
  (iterate speak-next-number start-numbers))

(defn- nth-number
  "Returns the nth number spoken, starting with the start-numbers."
  [n start-numbers]
  (if (<= n (.size start-numbers))
    (get start-numbers (dec n))
    (->> start-numbers
         number-stream
         (take (inc (- n (.size start-numbers))))
         last
         last)))

(defn solve-1
  "Returns the 2020th number spoken in the Elves' memory game given the starting
   numbers in s."
  [s]
  (nth-number 2020 (parse-input s)))

(def trial-input "0,3,6")

(def trial-input-2 "1,3,2")

(def trial-input-3 "2,1,3")

(def trial-input-4 "1,2,3")

(def trial-input-5 "2,3,1")

(def trial-input-6 "3,2,1")

(def trial-input-7 "3,1,2")

(def real-input "1,12,0,20,8,16")
