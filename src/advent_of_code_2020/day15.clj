(ns advent-of-code-2020.day15
  (:import [com.koloboke.collect.map.hash HashLongLongMaps]
           [com.koloboke.collect.map LongLongMap]))

(defn- parse-input
  "Parses a comma-separated list of numbers into a vector of numbers."
  [s]
  (->> s
       (re-seq #"\d+")
       (map #(Long/parseLong %))))

(defn- nth-number
  "Returns the nth number spoken, starting with the start-numbers."
  [n start-numbers]
  (let [l (.size start-numbers)]
    (if (<= n l)
      (get start-numbers (dec n))
      (let [spoken-at (HashLongLongMaps/newUpdatableMap
                       (butlast start-numbers)
                       (map inc (range (dec l))))]
        (loop [n (long (- n l))
               last-turn (long (.size start-numbers))
               last-number (long (last start-numbers))]
          (if (zero? n)
            last-number
            (let [turn-last-spoken-number-was-spoken-before (.getOrDefault ^LongLongMap spoken-at last-number (long -1))]
              (.put ^LongLongMap spoken-at last-number last-turn)
              (recur (dec n)
                     (inc last-turn)
                     (if (>= turn-last-spoken-number-was-spoken-before (long 0))
                       (- last-turn turn-last-spoken-number-was-spoken-before)
                       0)))))))))

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
