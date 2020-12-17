(ns advent-of-code-2020.day17
  (:require [clojure.string :as s]))

(defn- parse-initial-state
  "Parses the initial state of the pocket dimension into a vector of vectors of
  vectors of cubes."
  [s]
  (->> s
       s/split-lines
       (map vec)
       vec
       vector))

(defn- active?
  "Returns true if the cube at the given coordinates is active."
  [state coordinates]
  (= (get-in state coordinates) \#))

(defn- active-neighbours
  "Returns the number of active neighbours of the given coordinate in 3 dimensions."
  [state [z y x]]
  (apply + (for [dz [-1 0 1]
                 dy [-1 0 1]
                 dx [-1 0 1]
                 :when (not (= 0 dz dy dx))
                 :let [neighbour-active? (active? state [(+ z dz) (+ y dy) (+ x dx)])
                       count-neighbour (if neighbour-active? 1 0)]]
             count-neighbour)))

(defn- expand-state
  "Adds one layer of inactive cubes on all sides of the state in 3 dimensions."
  [state]
  (let [z (.size state)
        y (.size (get state 0))
        x (.size (get (get state 0) 0))
        inactive-x (vec (repeat (+ x 2) \.))
        inactive-y (vec (repeat (+ y 2) inactive-x))]
    (vec (concat [inactive-y]
                 (map (fn [y-layer] (vec (concat [inactive-x]
                                                 (map #(vec (concat [\.] % [\.])) y-layer)
                                                 [inactive-x]))) state)
                 [inactive-y]))))

(defn- apply-conway
  "Applies the Conway rules to all cubes in 3 dimensions and returns the resulting state."
  [state]
  (->>
   (for [z (range (.size state))
         y (range (.size (get state 0)))
         x (range (.size (get (get state 0) 0)))
         :let [coordinates [z y x]
               active? (active? state coordinates)
               active-neighbours (active-neighbours state coordinates)
               will-be-active? (or (and active? (or (= active-neighbours 2)
                                                    (= active-neighbours 3)))
                                   (and (not active?) (= active-neighbours 3)))]
         :when (not= will-be-active? active?)]
     [coordinates will-be-active?])
   (filter identity)
   (reduce (fn [state [coordinates active?]]
             (assoc-in state coordinates (if active? \# \.)))
           state)))

(defn- run-cycle
  "Runs one cycle in the 3-dimensional pocket dimension."
  [state]
  (->> state
       expand-state
       apply-conway))

(defn solve-1
  "Returns the number of active cubes after running the six-cycle boot process."
  [s]
  (->> s
       parse-initial-state
       (iterate run-cycle)
       (take 7)
       last
       flatten
       (filter #(= % \#))
       count))

(defn- active-neighbours-4dim
  "Returns the number of active neighbours of the given coordinate in 4 dimensions."
  [state [w z y x]]
  (apply + (for [dw [-1 0 1]
                 dz [-1 0 1]
                 dy [-1 0 1]
                 dx [-1 0 1]
                 :when (not (= 0 dw dz dy dx))
                 :let [neighbour-active? (active? state [(+ w dw) (+ z dz) (+ y dy) (+ x dx)])
                       count-neighbour (if neighbour-active? 1 0)]]
             count-neighbour)))

(defn- expand-state-4dim
  "Adds one layer of inactive cubes on all sides of the state in 4 dimensions."
  [state]
  (let [w (.size state)
        z (.size (get state 0))
        y (.size (get (get state 0) 0))
        x (.size (get (get (get state 0) 0) 0))
        inactive-x (vec (repeat (+ x 2) \.))
        inactive-y (vec (repeat (+ y 2) inactive-x))
        inactive-z (vec (repeat (+ z 2) inactive-y))]
    (vec (concat [inactive-z]
                 (map expand-state state)
                 [inactive-z]))))

(defn- apply-conway-4dim
  "Applies the Conway rules to all cubes in 4 dimensions and returns the resulting state."
  [state]
  (->>
   (for [w (range (.size state))
         z (range (.size (get state 0)))
         y (range (.size (get (get state 0) 0)))
         x (range (.size (get (get (get state 0) 0) 0)))
         :let [coordinates [w z y x]
               active? (active? state coordinates)
               active-neighbours (active-neighbours-4dim state coordinates)
               will-be-active? (or (and active? (or (= active-neighbours 2)
                                                    (= active-neighbours 3)))
                                   (and (not active?) (= active-neighbours 3)))]
         :when (not= will-be-active? active?)]
     [coordinates will-be-active?])
   (filter identity)
   (reduce (fn [state [coordinates active?]]
             (assoc-in state coordinates (if active? \# \.)))
           state)))

(defn- run-cycle-4dim
  "Runs one cycle in the 4-dimensional pocket dimension."
  [state]
  (->> state
       expand-state-4dim
       apply-conway-4dim))

(defn solve-2
  "Returns the number of active cubes after running the six-cycle boot process in
  4 dimensions."
  [s]
  (->> s
       parse-initial-state
       vector
       (iterate run-cycle-4dim)
       (take 7)
       last
       flatten
       (filter #(= % \#))
       count))

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
