(ns advent-of-code-2020.day13
  (:require [clojure.string :as s]))

(defn- active-buses
  "Extracts the active buses from a comma-separated list of bus lines."
  [s]
  (->> s
       (#(s/split % #","))
       (filter (partial not= "x"))
       (map #(Integer/parseInt %))))

(defn- time-to-wait
  "Returns the time you would have to wait from t until the next bus with ID bus
   arrives."
  [t bus]
  (- bus (mod t bus)))

(defn solve-1
  "Returns the ID of the earliest bus you can take to the airport multiplied by
   the number of minutes you'll need to wait for that bus."
  [s]
  (let [[earliest-departure-time bus-list] (s/split-lines s)
        earliest-departure-time (Integer/parseInt earliest-departure-time)]
    (->> bus-list
         active-buses
         (map #(vector % (time-to-wait earliest-departure-time %)))
         (apply min-key second)
         (apply *))))

(defn- parse-offsets-and-buses
  "Parses the bus list into a sequence of [offset bus-id]."
  [s]
  (->> s
       (#(s/split % #","))
       (map-indexed vector)
       (filter #(not= (second %) "x"))
       (map (fn [x] (update x 1 #(BigInteger. %))))))

(defn- find-constellation-timestamp
  "Returns the smallest n for which n equals bus-offset modulo bus
   for each [offset bus] pair."
  ([offsets-and-buses]
   (let [negated-offsets-and-buses (->> offsets-and-buses
                                        (map (fn [[offset bus]]
                                               [(mod (- bus offset) bus) bus]))
                                        vec)]
     (find-constellation-timestamp negated-offsets-and-buses 0 0 1)))
  ([negated-offsets-and-buses i start-n step]
   (if (>= i (count negated-offsets-and-buses))
     start-n
     (let [[no bus] (get negated-offsets-and-buses i)
           max-n (+ start-n (* step bus))
           n (->> (range start-n max-n step)
                  (filter #(= (mod % bus) no))
                  first)]
       (find-constellation-timestamp negated-offsets-and-buses
                                     (inc i)
                                     n
                                     (* step bus))))))

(defn solve-2
  "Returns the earliest timestamp such that all of the listed bus IDs depart at
   offsets matching their positions in the list."
  [s]
  (->> s
       (s/split-lines)
       second
       parse-offsets-and-buses
       find-constellation-timestamp))

(def trial-input "939
7,13,x,x,59,x,31,19")

(def real-input "1000495
19,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,521,x,x,x,x,x,x,x,23,x,x,x,x,x,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,29,x,523,x,x,x,x,x,37,x,x,x,x,x,x,13")
