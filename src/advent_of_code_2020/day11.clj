(ns advent-of-code-2020.day11
  (:require [clojure.string :as s]))

(def tile-of
  "Maps characters of the seat layout to tiles."
  {\. :floor
   \L :empty-seat
   \# :occupied-seat})

(defn- parse-seat-layout
  "Parses a seat layout."
  [s]
  (let [lines (s/split-lines s)
        height (count lines)
        width (count (first lines))]
    {:height height
     :width width
     :entries (vec (map tile-of (apply concat lines)))}))

(defn- map-tiles-in
  "Maps f over each entry in a matrix.
   f is passed the matrix, the entry, and its index."
  [matrix f]
  (update matrix :entries
          (fn [entries]
            (vec (map-indexed #(f matrix %2 %1) entries)))))

(defn- neighbour-matrix
  "Generates a matrix of neighbours (sequence of neighbour coordinates).

   The neighbours are determined by the function find-neighbours, which takes
   the seat-layout and [row column] and returns a sequence of [row column] pairs
   for the neighbours."
  [seat-layout find-neighbours]
  (map-tiles-in seat-layout #(find-neighbours %1 %3)))

(defn- direct-neighbours
  "Returns the indices of the direct neighbours of the given index in
   seat-layout."
  [seat-layout i]
  (let [num-rows (:height seat-layout)
        num-cols (:width seat-layout)
        max-col (dec num-cols)
        first-row? (< i num-cols)
        last-row? (>= i (* num-cols (dec num-rows)))
        first-col? (zero? (mod i num-cols))
        last-col? (= (mod i num-cols) max-col)]
    (->> [(when (not first-row?) [(when (not first-col?) (- i num-cols 1))
                                  (- i num-cols)
                                  (when (not last-col?) (- i max-col))])
          (when (not first-col?) (dec i))
          (when (not last-col?) (inc i))
          (when (not last-row?) [(when (not first-col?) (+ i max-col))
                                 (+ i num-cols)
                                 (when (not last-col?) (+ i num-cols 1))])]
         flatten
         (filter some?))))

(defn- count-if-occupied
  "Returns 1 if there is a seat at the given index and it is occupied,
   0 otherwise."
  [seat-layout-entries index]
  (if (= (get seat-layout-entries index) :occupied-seat) 1 0))

(defn- count-occupied-neighbours
  "Returns the number of occupied neighbours for the given tile in the given
  seat layout."
  [neighbours-entries seat-layout _ index]
  (->> (get neighbours-entries index)
       (map (partial count-if-occupied (:entries seat-layout)))
       (apply +)))

(defn- neighbour-count-matrix
  "Returns a matrix of numbers corresponding to the seat layout.
   Each position contains the number of its occupied neighbours."
  [seat-layout neighbours]
  (map-tiles-in seat-layout (partial count-occupied-neighbours (:entries neighbours))))

(defn- apply-rules
  "Applies the given rules given by the function change-seat to the seat-layout
   and returns the resulting layout.

   change-seat is expected to take a layout character and the number of occupied
   neighbours and return a layout character."
  [seat-layout neighbours change-seat]
  (let [neighbour-count-matrix (:entries (neighbour-count-matrix seat-layout neighbours))]
    (map-tiles-in seat-layout #(change-seat %2 (get neighbour-count-matrix %3)))))

(defn- step-rules-with-neighbour-threshold
  "Returns a step function that takes a layout character and the number of
   occupied neighbours and returns the layout character after applying the puzzle
   rules. How many neighbours are too many, causing people to leave, is given by
   the neighbour-threshold."
  [neighbour-threshold]
  (fn [tile number-of-occupied-neighbours]
    (case tile
      :empty-seat (if (zero? number-of-occupied-neighbours)
                    :occupied-seat
                    :empty-seat)
      :occupied-seat (if (>= number-of-occupied-neighbours neighbour-threshold)
                       :empty-seat
                       :occupied-seat)
      tile)))

(defn- until-it-no-longer-changes
  "Reducer function that stops when the value no longer changes."
  [prev next]
  (if (= next prev)
    (reduced prev)
    next))

(defn- print-it
  "Prints a seat layout and returns it."
  [seat-layout]
  (let [w (seat-layout :width)]
    (println "-------------------")
    (doall (map-indexed (fn [i tile]
                          (print (case tile
                                   :empty-seat \L
                                   :occupied-seat \#
                                   \.))
                          (when (= (mod i w) (dec w))
                            (println "")))
                        (seat-layout :entries))))
  seat-layout)

(defn- watch
  "Watch as people arrive at the given seat layout and choose their seats
   according to the given rules until the chaos has stabilized."
  [seat-layout neighbours rules]
  (->> seat-layout
       (iterate #(apply-rules % neighbours rules))
       ;(map print-it)
       (reduce until-it-no-longer-changes)))

(defn- count-occupied-seats
  "Returns the number of occupied seats in the given seat layout."
  [seat-layout]
  (->> seat-layout
       :entries
       (filter #(= % :occupied-seat))
       count))

(defn solve-1
  "Returns the number of seats that are occupied after everyone has settled down
   according to the rules of part 1."
  [s]
  (let [seat-layout (parse-seat-layout s)
        neighbours (neighbour-matrix seat-layout direct-neighbours)]
    (->
     (watch seat-layout neighbours (step-rules-with-neighbour-threshold 4))
     count-occupied-seats)))

(defn- step-once
  "Add the delta coordinates onto the row/column coordinates."
  [[row column] [row-delta column-delta]]
  [(+ row row-delta) (+ column column-delta)])

(defn- is-in-layout
  "Returns true if the given coordinates are inside the layout, false if they fall
   outside."
  [seat-layout [row column]]
  (and (>= row 0)
       (< row (seat-layout :height))
       (>= column 0)
       (< column (seat-layout :width))))

(defn- seat-in-direction
  "Return the coordinates of the first seat encountered when starting at the start
   coordinates and looking in the given direction."
  [seat-layout start direction]
  (->> (step-once start direction)
       (iterate #(step-once % direction))
       (take-while #(is-in-layout seat-layout %))
       (map (fn [[row column]] (+ (* row (seat-layout :width)) column)))
       (drop-while #(= (get-in seat-layout [:entries %]) :floor))
       first))

(defn- line-of-sight-neighbours
  "Returns the coordinates of the seats visible from the given coordinates in
   seat-layout."
  [seat-layout start]
  (->> [[-1 -1] [-1 0] [-1 1]
        [ 0 -1]        [ 0 1]
        [ 1 -1] [ 1 0] [ 1 1]]
       (map #(seat-in-direction seat-layout
                                [(quot start (seat-layout :width))
                                 (mod start (seat-layout :width))]
                                %))
       (filter some?)))

(defn solve-2
  "Returns the number of seats that are occupied after everyone has settled down
   according to the rules of part 2."
  [s]
  (let [seat-layout (parse-seat-layout s)
        neighbours (neighbour-matrix seat-layout line-of-sight-neighbours)]
    (->
     (watch seat-layout neighbours (step-rules-with-neighbour-threshold 5))
     count-occupied-seats)))

(def trial-input "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(def real-input "LLLLLLLLL.L.LLL.L.LLLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLLLLLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLL.LLLLL.LLLLLLLLLLLLLLLL
LLLLLLLLLLLLL.LLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLL.LLLLLL.LLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLL.LLL.LLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLL
L..LL.L.L....L..L.L....L.....L.L.L......L..L..L....L.LL.......L.....L....L..L................L....
LLLLLLLLLLLLLLL.LLL.LLL.LLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLL..LLLLLLLL.LLLLLLL.LLLLL.LLLLLLL.LLLL.LLLLLL.LLLLLLLLL.LLL.LLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLL.LLL.LLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLL
LL.LLLLLLLLLLLL.LLL.LLL.LLLLLLLL.LLLLLLLL.LLLLLLL..LLLL.LLLLLLL.LLLL.LLLLLL.LLLLL.LLLL.LL.LLLLLLLL
LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLLLL.L.LLLLLLL.LLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLLLLLLLLLL.LLLL.LLL.LLLLLLLL.LLLLLLLLL.LLLLLLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLLL.LLLL.LLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLL.LLLL.LLLLLLLLL.LL.LLLLLLLLLLLLLLLL
.LLLL..L.L..L....L.LLL...LLL..L.L....L.L.L...L.....L...L.....L...L..L...LL...L..LLL..L...L....L..L
LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLL
LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLL.LLLLLLL.LLLL.LLLLLL.LLLL..LLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLL..LLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLL.LLLLLLL.LLLL.LLLLLL.LLLLLLLLLLLLL.LLLLLLLL
.....LL...LL..L...L..L..LL.L..LLL...L..L.LLL...LLL..L.....L...L.....L........LLLL..LLL.....L....L.
LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLLLLLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLL.LLLLLLLLLLLLLLLLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLL.LLLL.LLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLL.LLLLL.LLLLLLLLLLLLL.LL
LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLL.LL.LLLLLLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLL.L
LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLL.LLLLLLLL.LLL.LLLLLLLLL.LLLLLLL.LLLL.LLL.LL.LLLLL.LLLLLLLLLLLLLLLL
LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.L.LLLLL.LLLLLLLL
....L..LL.L.L......L..LL.......LL......L..L...L..L.........LL....LL.LLL..LLLL..L..L.......L....L..
LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLL.LLLLLL.LLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLLLLLLLLLLL
LLLLLLLLL.LLL.L.LLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLL.LLL.LLLLLLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLL
.LL..LLLLL.LL.L...L...L...L....LLLL...LLLL.LL...LL...L.LL...LL..LL.L..L......L....L.L.L.L...LL...L
LLLLLLLLL.LLLLLLLLLLLLL..LLLLLLL.LLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLL.LLLLLLL.LLLL.LLLLLLLLL.LL.LLLLLLLLLLLLLLLL
LLLLLLLLL.LLLLL.L.LLLLL.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLL.LLLL..LLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLL.LLLLLLL.LLLL.LLLLLL.LLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLL.LL.LL.LLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLL.LLLL.LLLLLL.LLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLLLLL.LLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLL.LLLLLLL.LLLLLL.LLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLL.LLLLL.L.LLLLLLLL
....L.........LL...L.L..LL.L.L..L......L.L.L...L....L.......LL.LL....L.L...L....L...L....L...L....
L.LLLLLLL.LLLLL.LLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLL.LL..LLLL
LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLL..LLLLLLL.LLLLLLLLLLLLL.LLLL.LLLLLL.LLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLL.L.LLLLLLL.LLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLL
LLLLLL.LL.LLLLLLLLLLLLL.LLLL.LLLLLLLLLLLL.LLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLL
LLL..L.L........L...L..L.LLLLLLL..LLLL..L...L..L.L..LLL....L..L.LLL..L...L..L.......L.LLLLLL.LL.L.
LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LL.LLLL.LLLL.LLLLLL.LLLLL.LLLLLLLLLLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLLLLLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLL.LL.LL.LLLLLLL.LLLL.LLLLLL.LLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLL.LLL.LLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLL
L...L.....LL....L.L.LLLLL.....L....LL..L.....L...L.L.LL.......LL...LL............L.L..L..LLL..LL..
LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLLLLL.LLLL.LLLLLLLLLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLL.LLLLLLL.LLLL.LLLLLL.LLLLL.LLLLLLL.LLLLL.LL
LL.LLLLLLLLLLLL.L.LLL.LLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLL.LL.LLLL.LLLLLL.LLLLL.LLLLLLL.L.LLLLLL
LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLL.LLLL
LLLLLLLLL.LLLLL.L.LLLLL.LLLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLL.LLLLLLLLLLLLLL
LLLL.LLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLLL..LLLL.LLLLLL.LLLLL.LLLLLLL.LLLLLLLL
LL...L.........L..LLL........L.L..L....L.L.L....LLLL.LL.L....LL....L.LL.LLL..L..L....LL.L......LLL
LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLL.L.LLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLL
...L........L....L.L.LL.LL.L.L.....L.L..L.....LL....L..L.L.L..L..L.L...L.........L.LL.LLLLL..L....
LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLL..LLLL.LLLLLLL.LLLLLLLLLLL.LLL.L.LLLLLLLLLLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLL.LLLLL.L.LLLLLLLL
LLLLLLLLL.LLLLLLLLLLLLL.LL.LLLLLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLL.LLLLL.LLLLLLL..LLLLLLL
LLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLL
..LLLL.L..LL.L......LL.L...L.L..L....LLL.........L....L.......LL.LLL.L.L.....LL.L...L.LLL....L....
LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.L.LLL.LLLLLLL.LLLL.LLLLLL.LLLL..LLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLL.LLLLLLLLLLLL.LLLLLLLLL.L.LLLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLLLLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLLLLLLLLLLLLL.LL.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLL.LLLLLL.LLLLL.LLLLLLLLLLLLLLLL
LLLL.LLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLL.LLLLLLL.LLLL.LLLLLLLLLLLL.LLLLLLL.LLLLLLLL
LL...LLLLL.....LL..L.....L....L......L.L........LL......LL.LL..L..LLL.L...L..L..L.L.....LLLL...LLL
LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLL.LL.LLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLLLLLLLLLLL.LLLL.LLLLLL.LLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLL.LLLLL.LLLLLLLLLLLL.LLLLLL.LLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLL.LLLLL.LLL.LLLLLLL.LLLLLLL.LLLLLLLLLLLLL.LLLL.LLLLLL.LLLLL.LLLLLLLLLLLLLLLL
.LL..L.....LLLL.LL..L.LL....L.LL...L..LL......L..LL...LL..L.L.LLLL...LL.L..L..LLL.L.L.....L.LL.LL.
LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLLLLLLLLLLL.LLLLL.LLLLLLLLL.LL.LLLLLLLLLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLL.LLLL.LLLLLLLLLLLL.LLLLLLL.LLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLL.LLLLLLLL.LLLLLLLLLLL.LLLL.LLLLL.LLLLLLL.LLLL.LLLLLLLLLLLLLLLLLLLL.LLLLL.LL
LLLLLLLLL.LLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLLLLLL.LLLLLLLLLLLLLLLL
LLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLL.LLLLLL.LLLLL.LLLLLLLLLLLLLLLL
LLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLLL.LLLLLLL.LLLL..LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL.LLLLLLLL
LLLLLLLLL.LLLLL.LLLLLLLLLLLLLLLL.LLLLLLLL.LLLLLLL.LLL.L.LLLLLLLLLLLL.LLLLLLLLLLLLLLLLLLLL.LLLLLLLL")
