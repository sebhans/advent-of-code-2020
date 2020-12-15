(ns advent-of-code-2020.day15)

(defn- parse-input
  "Parses a comma-separated list of numbers into a vector of numbers."
  [s]
  (->> s
       (re-seq #"\d+")
       (map #(Long/parseLong %))))

(defn- game-state-from
  "Creates a game state from the given starting numbers."
  [starting-numbers]
  (let [with-turns (map-indexed #(vector %2 (inc %1)) starting-numbers)]
    {:last-number (first (last with-turns))
     :last-turn (second (last with-turns))
     :spoken-at (apply hash-map (flatten (butlast with-turns)))}))

(defn- speak-next-number
  "Speaks the next number given the current game state."
  [state]
  (let [last-turn (state :last-turn)
        most-recently-spoken-number (state :last-number)
        turn-most-recently-spoken-number-was-spoken-before ((state :spoken-at) most-recently-spoken-number)]
    (-> state
        (assoc :last-number (if turn-most-recently-spoken-number-was-spoken-before
                              (- last-turn turn-most-recently-spoken-number-was-spoken-before)
                              0))
        (update :last-turn inc)
        (assoc-in [:spoken-at most-recently-spoken-number] last-turn))))

(defn- number-stream
  "Generates a lazy sequence of number vectors, representing the game state."
  [start-numbers]
  (iterate speak-next-number (game-state-from start-numbers)))

(defn- nth-number
  "Returns the nth number spoken, starting with the start-numbers."
  [n start-numbers]
  (if (<= n (.size start-numbers))
    (get start-numbers (dec n))
    (->> start-numbers
         number-stream
         (take (inc (- n (.size start-numbers))))
         last
         :last-number)))

(defn solve-1
  "Returns the 2020th number spoken in the Elves' memory game given the starting
   numbers in s."
  [s]
  (nth-number 2020 (parse-input s)))

(defn solve-2
  "Returns the 30000000th number spoken in the Elves' memory game given the
   starting numbers in s."
  [s]
  (nth-number 30000000 (parse-input s)))

(def trial-input "0,3,6")

(def trial-input-2 "1,3,2")

(def trial-input-3 "2,1,3")

(def trial-input-4 "1,2,3")

(def trial-input-5 "2,3,1")

(def trial-input-6 "3,2,1")

(def trial-input-7 "3,1,2")

(def real-input "1,12,0,20,8,16")
