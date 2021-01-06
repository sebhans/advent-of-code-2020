(ns advent-of-code-2020.day15
  (:import [com.koloboke.collect.map.hash HashLongLongMaps]
           [com.koloboke.collect.map LongLongMap]))

(defn- parse-input
  "Parses a comma-separated list of numbers into a vector of numbers."
  [s]
  (->> s
       (re-seq #"\d+")
       (map #(Long/parseLong %))))

(defrecord GameState [^long last-turn ^long last-number ^LongLongMap spoken-at])

(defn- game-state-from
  "Creates a game state from the given starting numbers."
  [starting-numbers]
  (let [with-turns (map-indexed #(vector %2 (inc %1)) starting-numbers)
        numbers (butlast starting-numbers)
        turns (map inc (range (dec (.size starting-numbers))))]
    (GameState. (.size starting-numbers)
                (last starting-numbers)
                (HashLongLongMaps/newUpdatableMap numbers turns))))

(defn- speak-next-number
  "Speaks the next number given the current game state."
  [^GameState state]
  (let [last-turn (:last-turn state)
        most-recently-spoken-number (:last-number state)
        spoken-at (:spoken-at state)
        turn-most-recently-spoken-number-was-spoken-before (.getOrDefault ^LongLongMap spoken-at ^long most-recently-spoken-number (long -1))]
    (.put ^LongLongMap spoken-at ^long most-recently-spoken-number ^long last-turn)
    (GameState. (inc last-turn)
                (if (>= turn-most-recently-spoken-number-was-spoken-before (long 0))
                  (- last-turn turn-most-recently-spoken-number-was-spoken-before)
                  0)
                spoken-at)))

(defn- nth-number
  "Returns the nth number spoken, starting with the start-numbers."
  [n start-numbers]
  (if (<= n (.size start-numbers))
    (get start-numbers (dec n))
    (loop [n (long (- n (.size start-numbers)))
           game-state (game-state-from start-numbers)]
      (if (zero? n)
        (:last-number game-state)
        (recur (dec n) (speak-next-number game-state))))))

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
