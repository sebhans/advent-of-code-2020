(ns advent-of-code-2020.day17
  (:require [clojure.string :as s]))

(defn- parse-initial-state
  "Parses the initial state of the pocket dimension into the set of
  coordinates (2-dimensional vectors) of active cubes."
  [s]
  (->> s
       s/split-lines
       (map-indexed (fn [y line]
                      (map-indexed (fn [x cube] [x y cube]) line)))
       (mapcat identity)
       (filter #(= (get % 2) \#))
       (map #(subvec % 0 2))
       set))

(defn- in-dimension
  "Appends dimensions to the coordinates in state to bring them to n dimensions."
  [n state]
  (let [d (- n (.size (first state)))
         additional-dimensions (vec (repeat d 0))]
    (set (map #(into % additional-dimensions) state))))

(defn- neighbours
  "Returns all neighbour coordinates for a given coordinate."
  ([coordinate]
   (filter #(not= % coordinate) (neighbours [coordinate] 0)))
  ([coordinates dimension]
   (if (>= dimension (.size (first coordinates)))
     coordinates
     (recur (->> coordinates
                 (mapcat #(vector (update % dimension dec)
                                  %
                                  (update % dimension inc))))
            (inc dimension)))))

(defn- print-state
  "Prints a representation of state."
  ([state]
   (->> state
        (map #(subvec % 2 (.size (first state))))
        distinct
        sort
        (run! #(print-state state %))))
  ([state layer]
   (let [min-x (apply min (map #(get % 0) state))
         max-x (apply max (map #(get % 0) state))
         min-y (apply min (map #(get % 1) state))
         max-y (apply max (map #(get % 1) state))]
     (println "\nLayer:" layer)
     (doseq [y (range min-y (inc max-y))
             x (range min-x (inc max-x))
             :let [c (into [x y] layer)
                   cube (if (state c) \# \.)]]
       (print cube)
       (when (= x max-x)
         (print "\n"))))))

(defn- apply-conway
  "Applies the Conway rules to all cubes and returns the resulting state."
  [state]
  (let [neighbour-counts (frequencies (mapcat neighbours state))
        three-neighbours? (fn [[_ n]] (= n 3))
        two-or-three-neighbours? (fn [[_ n]] (or (= n 2) (= n 3)))
        coordinate (fn [[c _]] c)
        newly-activated-cubes (->> neighbour-counts
                                   (filter three-neighbours?)
                                   (map coordinate)
                                   (filter (complement state)))
        still-active-cubes (->> neighbour-counts
                                (filter two-or-three-neighbours?)
                                (map coordinate)
                                (filter state))]
    (into (set still-active-cubes) newly-activated-cubes)))

(defn- boot-up-and-count
  "Returns the number of active cubes after running the six-cycle boot process in
  n dimensions with the initial state described by s."
  [n s]
  (->> s
       parse-initial-state
       (in-dimension n)
       (iterate apply-conway)
       (take 7) ; because the first one is the initial state
       last
       count))

(def solve-1 (partial boot-up-and-count 3))

(def solve-2 (partial boot-up-and-count 4))

(def trial-input ".#.
..#
###")

(def real-input "...#.#.#
..#..#..
#.#.##.#
###.##..
#####.##
#.......
#..#..##
...##.##")
