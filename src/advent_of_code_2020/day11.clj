(ns advent-of-code-2020.day11
  (:require [clojure.string :as s]))

(def tile-of
  "Maps characters of the seat layout to tiles."
  {\. :floor
   \L :empty-seat
   \# :occupied-seat})

(defn- parse-seat-layout
  "Parses a seat layout into a vector of vectors of tiles.
   Each tile is a map with the state in the :tile key."
  [s]
  (->> s
       s/split-lines
       (map (fn [row] (->> row (map #(hash-map :tile (tile-of %))) vec)))
       vec))

(defn- for-each-position-in
  "List comprehension over each position in a matrix.
   f is called with matrix, row, and column as arguments."
  [matrix f]
  (vec (for [row (range (count matrix))]
         (vec (for [column (range (count (first matrix)))]
                (f matrix row column))))))

(defn- link-neighbours
  "Adds a :neighbours key to each tile, which contains a sequence of coordinates
   of the tile's neighbours.

   The neighbours are determined by the function find-neighbours, which takes
   the seat-layout, row, and column and returns a sequence of [row column] pairs
   for the neighbours."
  [seat-layout find-neighbours]
  (for-each-position-in seat-layout #(assoc (get-in %1 [%2 %3])
                                            :neighbours
                                            (find-neighbours %1 %2 %3))))

(defn- direct-neighbours
  "Returns the coordinates of the direct neighbours of the given coordinates in
   seat-layout."
  [seat-layout row column]
  (let [num-rows (count seat-layout)
        num-columns (count (first seat-layout))]
    (->> [(when (> row 0) [(when (> column 0) [(dec row) (dec column)])
                           [(dec row) column]
                           (when (< column (dec num-columns)) [(dec row) (inc column)])])
          (when (> column 0) [[row (dec column)]])
          (when (< column (dec num-columns)) [[row (inc column)]])
          (when (< row (dec num-rows)) [(when (> column 0) [(inc row) (dec column)])
                                        [(inc row) column]
                                        (when (< column (dec num-columns)) [(inc row) (inc column)])])]
         (apply concat)
         (filter some?))))

(defn- count-if-occupied
  "Returns 1 if there is a seat at the given coordinates and it is occupied,
   0 otherwise."
  [seat-layout coordinates]
  (if (= (get-in seat-layout (conj coordinates :tile)) :occupied-seat) 1 0))

(defn- count-occupied-neighbours
  "Returns the number of occupied neighbours for the given tile in the given
  seat layout."
  [seat-layout tile]
  (->> tile
       :neighbours
       (map (partial count-if-occupied seat-layout))
       (apply +)))

(defn- for-each-tile-of
  "Invokes f for each tile and replaces the tile with the returned value.

   f is passed the seat layout and the tile."
  [seat-layout f]
  (vec (for [row seat-layout]
         (vec (for [tile row]
                (f seat-layout tile))))))

(defn- neighbours
  "Returns a matrix of numbers corresponding to the seat layout.
   Each position contains the number of its occupied neighbours."
  [seat-layout]
  (for-each-tile-of seat-layout count-occupied-neighbours))

(defn- apply-rules
  "Applies the given rules given by the function change-seat to the seat-layout
   and returns the resulting layout.

   change-seat is expected to take a layout character and the number of occupied
   neighbours and return a layout character."
  [seat-layout change-seat]
  (let [neighbours (neighbours seat-layout)]
    (for-each-position-in seat-layout #(change-seat (get-in seat-layout [%2 %3])
                                                    (get-in neighbours [%2 %3])))))

(defn- step-rules-1
  "Takes a layout character and the number of occupied neighbours and returns the
   layout character after applying the puzzle rules from part 1."
  [tile number-of-occupied-neighbours]
  (assoc tile :tile
         (case (:tile tile)
           :floor :floor
           :empty-seat (if (zero? number-of-occupied-neighbours)
                         :occupied-seat
                         :empty-seat)
           :occupied-seat (if (>= number-of-occupied-neighbours 4)
                            :empty-seat
                            :occupied-seat))))

(defn- fixed-point
  "Reducer function that stops when the value no longer changes."
  [prev next]
  (if (= next prev)
    (reduced prev)
    next))

(defn- watch
  "Watch as people arrive at the given seat layout and choose their seats
   according to the given rules until the chaos has stabilized."
  [seat-layout rules]
  (->> seat-layout
       (iterate #(apply-rules % rules))
       (reduce fixed-point)))

(defn- count-occupied-seats
  "Returns the number of occupied seats in the given seat layout."
  [seat-layout]
  (->> seat-layout
       flatten
       (filter #(= (:tile %) :occupied-seat))
       count))

(defn solve-1
  "Returns the number of seats that are occupied after everyone has settled down
   according to the rules of part 1."
  [s]
  (-> s
      parse-seat-layout
      (link-neighbours direct-neighbours)
      (watch step-rules-1)
      count-occupied-seats))

(defn- step-once
  "Add the delta coordinates onto the row/column coordinates."
  [[row column] [row-delta column-delta]]
  [(+ row row-delta) (+ column column-delta)])

(defn- seat-in-direction
  "Return the coordinates of the first seat encountered when starting at the start
   coordinates and looking in the given direction."
  [seat-layout start direction]
  (->> (step-once start direction)
       (iterate #(step-once % direction))
       (drop-while #(= (:tile (get-in seat-layout %)) :floor))
       first
       (#(when (:tile (get-in seat-layout %)) %))))

(defn- line-of-sight-neighbours
  "Returns the coordinates of the seats visible from the given coordinates in
   seat-layout."
  [seat-layout row column]
  (let [num-rows (count seat-layout)
        num-columns (count (first seat-layout))
        directions [[-1 -1] [-1 0] [-1 1]
                    [ 0 -1]        [ 0 1]
                    [ 1 -1] [ 1 0] [ 1 1]]]
    (->> directions
         (map #(seat-in-direction seat-layout [row column] %))
         (filter some?))))

(defn- step-rules-2
  "Takes a layout character and the number of occupied neighbours and returns the
   layout character after applying the puzzle rules from part 2."
  [tile number-of-occupied-neighbours]
  (assoc tile :tile
         (case (:tile tile)
           :floor :floor
           :empty-seat (if (zero? number-of-occupied-neighbours)
                         :occupied-seat
                         :empty-seat)
           :occupied-seat (if (>= number-of-occupied-neighbours 5)
                            :empty-seat
                            :occupied-seat))))

(defn solve-2
  "Returns the number of seats that are occupied after everyone has settled down
   according to the rules of part 2."
  [s]
  (-> s
      parse-seat-layout
      (link-neighbours line-of-sight-neighbours)
      (watch step-rules-2)
      count-occupied-seats))

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
