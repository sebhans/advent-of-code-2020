(ns advent-of-code-2020.day23
  (:require [clojure.string :as s]))

(defn- parse-cups
  "Parses the cups into a vector of numbers."
  [s]
  (vec (map #(- (int %) (int \0)) s)))

(defn- cups-from
  "Returns a sequence of cups starting with cup-1."
  [cup]
  (concat (reverse (range 1 cup)) (reverse (range cup 10))))

(defn- perform-move
  "Performs one move with the cups.
  The current cup is always the first one in the vector."
  [cups]
  (let [current-cup (get cups 0)
        picked-cups (subvec cups 1 4)
        rest-of-cups (subvec cups 4)
        destination-cup (->> (cups-from current-cup)
                             (filter (complement (set picked-cups)))
                             first)
        index-of-destination-cup (.indexOf rest-of-cups destination-cup)]
    (-> (subvec rest-of-cups 0 (inc index-of-destination-cup))
        (into picked-cups)
        (into (subvec rest-of-cups (inc index-of-destination-cup)))
        (conj current-cup))))

(defn- play-cup-game
  "Returns a lazy sequence of moves."
  [cups]
  (iterate perform-move cups))

(defn- cups-after-cup-1
  "Returns the labels on the cups after cup 1 as a string."
  [cups]
  (let [index-of-cup-1 (.indexOf cups 1)]
    (str (s/join (subvec cups (inc index-of-cup-1)))
         (s/join (subvec cups 0 index-of-cup-1)))))

(defn solve-1
  "Returns the labels on the cups after cup 1 after 100 moves."
  [s]
  (->> s
       parse-cups
       play-cup-game
       (take 101) ; because the sequence includes the initial cup configuration
       last
       cups-after-cup-1))

(def trial-input "389125467")

(def real-input "586439172")
