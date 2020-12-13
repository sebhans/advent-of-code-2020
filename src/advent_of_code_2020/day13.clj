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

(def trial-input "939
7,13,x,x,59,x,31,19")

(def real-input "1000495
19,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,521,x,x,x,x,x,x,x,23,x,x,x,x,x,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,29,x,523,x,x,x,x,x,37,x,x,x,x,x,x,13")
