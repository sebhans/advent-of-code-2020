  (ns advent-of-code-2020.day20
  (:require [clojure.string :as s]
            [clojure.set :refer [intersection]]))

(defn- encode-border
  "Encodes a tile border as a number."
  [border]
  (->> border
       (map-indexed (fn [i pixel]
                      (if (= pixel \#) (bit-shift-left 1 i) 0)))
       (apply +)))

(defn- parse-single-tile-borders
  "Parses a single tile into a map with a binary encoding of each border."
  [s]
  (let [[header & tile-lines] (s/split-lines s)
        [_ tile-id] (re-matches #"Tile (\d+):" header)]
    {:id (Long/parseLong tile-id)
     :image (map #(apply str (butlast (rest %))) (butlast (rest tile-lines)))
     :n (encode-border (first tile-lines))
     :s (encode-border (last tile-lines))
     :w (encode-border (map first tile-lines))
     :e (encode-border (map last tile-lines))
     :nr (encode-border (reverse (first tile-lines)))
     :sr (encode-border (reverse (last tile-lines)))
     :wr (encode-border (reverse (map first tile-lines)))
     :er (encode-border (reverse (map last tile-lines)))}))

(defn- parse-all-tile-borders
  "Parses a set of tiles into a sequence of tile maps."
  [s]
  (map parse-single-tile-borders (s/split s #"\n\n")))

(defn- border-encodings
  "Returns the set of border encodings of the given tile."
  [tile]
  (hash-set (:n tile)
            (:s tile)
            (:w tile)
            (:e tile)
            (:nr tile)
            (:sr tile)
            (:wr tile)
            (:er tile)))

(defn- unique-border-encodings
  "Returns a set of border encodings which are unique among all tiles."
  [tiles]
  (->> tiles
       (mapcat border-encodings)
       frequencies
       (filter (fn [[encoding frequency]] (= frequency 1)))
       (map first)
       set))

(defn- with-4
  "Returns all tiles with four of their border encodings from the given set."
  [tiles search-set]
  (filter #(= 4 (count (intersection (border-encodings %) search-set))) tiles))

(defn solve-1
  "Returns the product of the tile IDs of the corner tiles in s."
  [s]
  (let [tiles (parse-all-tile-borders s)
        unique-border-encodings (unique-border-encodings tiles)
        corner-tiles (with-4 tiles unique-border-encodings)]
    (->> corner-tiles
         (map :id)
         (apply *))))

(defn- borders
  "Returns a map of border encoding to direction of the given tile."
  [tile]
  (hash-map (:n tile) :n
            (:s tile) :s
            (:w tile) :w
            (:e tile) :e
            (:nr tile) :nr
            (:sr tile) :sr
            (:wr tile) :wr
            (:er tile) :er))

(defn- transpose-image
  "Transposes an image."
  [image]
  (apply mapv str image))

(defn- transpose-tile
  "Transposes a tile."
  [tile]
  (update (assoc tile
                 :n (tile :w)
                 :nr (tile :wr)
                 :w (tile :n)
                 :wr (tile :nr)
                 :s (tile :e)
                 :sr (tile :er)
                 :e (tile :s)
                 :er (tile :sr))
          :image transpose-image))

(defn- flipv-image
  "Flips an image vertically."
  [image]
  (vec (reverse image)))

(defn- flipv-tile
  "Flips a tile vertically."
  [tile]
  (update (assoc tile
                 :n (tile :s)
                 :nr (tile :sr)
                 :w (tile :wr)
                 :wr (tile :w)
                 :s (tile :n)
                 :sr (tile :nr)
                 :e (tile :er)
                 :er (tile :e))
          :image flipv-image))

(defn- fliph-image
  "Flips an image horizontally."
  [image]
  (mapv s/reverse image))

(defn- fliph-tile
  "Flips a tile horizontally."
  [tile]
  (update (assoc tile
                 :n (tile :nr)
                 :nr (tile :n)
                 :w (tile :e)
                 :wr (tile :er)
                 :s (tile :sr)
                 :sr (tile :s)
                 :e (tile :w)
                 :er (tile :wr))
          :image fliph-image))

(defn- rotate-to-west
  "Rotates the tile so that the given direction matches west."
  [tile direction]
  (cond
    (= direction :w) tile
    (= direction :wr) (-> tile flipv-tile)
    (= direction :e) (-> tile fliph-tile)
    (= direction :er) (-> tile fliph-tile flipv-tile)
    (= direction :s) (-> tile transpose-tile fliph-tile)
    (= direction :sr) (-> tile transpose-tile flipv-tile fliph-tile)
    (= direction :n) (-> tile transpose-tile)
    (= direction :nr) (-> tile transpose-tile flipv-tile)
    :else (throw (UnsupportedOperationException.
                  (str "Direction " direction " to west")))))

(defn- find-and-align-next-tile
  "Finds the tile in tiles that fits to the right of the given tile and returns
  it propertly oriented."
  [tiles tile]
  (let [border (:e tile)
        [direction next-tile] (->> tiles
                                   vals
                                   (map #(vector ((borders %) border) %))
                                   (filter first)
                                   first)]
    (rotate-to-west next-tile direction)))

(defn- assemble-row
  "Returns an assembled line of the given width starting with the given
  start tile."
  ([tiles width start-tile]
   (loop [tiles (dissoc tiles (:id start-tile))
          row [start-tile]]
     (if (= (.size row) width)
       row
       (let [next-tile (find-and-align-next-tile tiles (last row))]
         (recur (dissoc tiles (:id next-tile)) (conj row next-tile)))))))

(defn- rotate-to-north
  "Rotates the tile so that the given direction matches north."
  [tile direction]
  (cond
    (= direction :n) tile
    (= direction :nr) (-> tile fliph-tile)
    (= direction :s) (-> tile flipv-tile)
    (= direction :sr) (-> tile flipv-tile fliph-tile)
    (= direction :e) (-> tile transpose-tile flipv-tile)
    (= direction :w) (-> tile transpose-tile)
    :else (throw (UnsupportedOperationException.
                  (str "Direction " direction " to north")))))

(defn- find-and-align-first-tile
  "Finds the tile in tiles that fits to the bottom of the given tile and returns
  it propertly oriented."
  [tiles tile]
  (let [border (:s tile)
        [direction next-tile] (->> tiles
                                   vals
                                   (map #(vector ((borders %) border) %))
                                   (filter first)
                                   first)]
    (rotate-to-north next-tile direction)))

(defn- which-of
  "Returns the direction which matches one of border-encodings."
  [tile directions border-encodings]
  (->> directions
       (map #(vector % (% tile)))
       (filter #(border-encodings (second %)))
       (map first)
       first))

(defn- assemble-tiles
  "Returns a vector of vectors of tiles in the correct alignment."
  [tiles]
  (let [unique-border-encodings (unique-border-encodings tiles)
        a-corner (first (with-4 tiles unique-border-encodings))
        corner-west (which-of a-corner [:w :e] unique-border-encodings)
        corner-north (which-of a-corner [:n :s] unique-border-encodings)
        oriented-corner (rotate-to-north (rotate-to-west a-corner corner-west)
                                         corner-north)
        width (long (Math/sqrt (count tiles)))]
    (loop [tiles (into {} (map #(vector (:id %) %) tiles))
           rows []
           first-tile-in-row oriented-corner]
      (if (= (.size rows) width)
        rows
        (let [row (assemble-row tiles width first-tile-in-row)
              tiles (reduce #(dissoc %1 (:id %2)) tiles row)]
          (if (empty? tiles)
            (conj rows row)
            (recur tiles
                   (conj rows row)
                   (find-and-align-first-tile tiles first-tile-in-row))))))))

(defn- assemble-image
  "Returns the assembled image."
  [tiles]
  (->> tiles
       assemble-tiles
       (mapcat
        (fn [rows]
          (reduce (fn [image tile]
                    (map str image (:image tile)))
                  (repeat (count tiles) "")
                  rows)))
       vec))

(defn- match-positions
  "Returns a sequence of match positions for re in s."
  [re s]
  (let [matcher (re-matcher re s)]
    (->> (repeatedly #(when (re-find matcher)
                        (.start matcher)))
         (take-while identity))))

(defn- find-sea-monsters
  "Returns a sequence of possible locations of sea monsters."
  [image]
  (->> image
       (map-indexed (fn [i row]
                      (->> (match-positions #"#....##....##....###" row)
                           (map #(vector i %)))))
       (mapcat identity)
       (filter (fn [[i j]] (and (pos? i) (< i (dec (.size image))))))
       (filter (fn [[i j]]
                 (re-matches #"..................#."
                             (subs (get image (dec i))
                                   j
                                   (+ j 20)))))
       (filter (fn [[i j]]
                 (re-matches #".#..#..#..#..#..#..."
                             (subs (get image (inc i))
                                   j
                                   (+ j 20)))))))

(defn- find-sea-monsters-in-all-orientations
  "Returns a sequence of possible locations of sea monsters."
  [image]
  (->> [(find-sea-monsters image)
        (find-sea-monsters (-> image transpose-image))
        (find-sea-monsters (-> image fliph-image))
        (find-sea-monsters (-> image flipv-image))
        (find-sea-monsters (-> image transpose-image fliph-image))
        (find-sea-monsters (-> image transpose-image flipv-image))
        (find-sea-monsters (-> image transpose-image flipv-image fliph-image))
        (find-sea-monsters (-> image flipv-image fliph-image))]
       (mapcat identity)))

(defn- solve-2
  "Returns the number of '#' that are not part of a sea monster."
  [s]
  (let [image (->> s
                   parse-all-tile-borders
                   assemble-image)]
    (->> image
         find-sea-monsters-in-all-orientations
         count
         (* 15)                            ; There are 15 '#'s in a sea monster.
         (- (count (filter #(= \# %) (apply str image)))))))

(def trial-input "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...")

(def real-input "Tile 1237:
######..#.
...##.#..#
....#..#..
.#.#......
#..##.....
#.#..#..#.
......#..#
#.....#...
..........
..#.#.....

Tile 2113:
.##.####.#
.##.#.#.##
#....#...#
........#.
##.##.#.##
..#.#.....
#.##....#.
.##.......
.......#.#
##..##.#..

Tile 3089:
....###..#
##...#..#.
.....#.#..
...#.....#
..........
#........#
##.##..#.#
....###...
#.#....#..
#####..#..

Tile 1217:
#..#...###
#.#.......
.......###
#.........
....#....#
#..#....##
#.......##
#........#
#.#..#..#.
##....##.#

Tile 2129:
.###.##.##
##.......#
#....#....
#......#.#
#..#.#..##
#.......#.
##....####
..#.....##
#........#
...##.####

Tile 1861:
##.##..#..
...#.#...#
#....#....
......##.#
...#.....#
.........#
##....#...
##...#....
.#...#.##.
#.#.###...

Tile 3907:
.#.#.#...#
........##
##.......#
#......#..
#......##.
..#......#
#.#..#....
...#....#.
.......#..
.#.####.##

Tile 1549:
##.#......
....#.#.##
...#.....#
..........
..#......#
...##....#
..........
#.......#.
##...#...#
.###.##..#

Tile 2699:
..####...#
.##.#..###
..#..##.##
#.#.#...##
#.....#...
#.......##
###...#..#
.....#....
#........#
.#.###....

Tile 2593:
.#....#...
....#.#...
....##....
##...#...#
...#....##
#...#..#.#
..##.#..#.
..####....
.#..#..#..
...###.##.

Tile 3209:
.#..#....#
.....#...#
#.........
..........
#.........
.....#..#.
#.........
##........
..#.......
#.#.....##

Tile 1789:
###..#..#.
#.#..##.##
#...##...#
#.........
..#.#.....
..####..##
#.#..###..
......####
#....####.
..##.#..##

Tile 2111:
#.##.#.###
.#..#....#
.#.#..#...
#.......#.
.......#..
#......#.#
#.........
.....#...#
##.....#..
..##.#..##

Tile 3137:
.####..##.
##.##....#
#....#....
#.........
..#...#.#.
#.#..##.#.
..#...#..#
..#..#.#.#
.......###
#.##...###

Tile 1871:
#.####....
#....#....
......#..#
#.......##
##.#....##
..#..#....
#..#...#..
#.#...#.##
#.##.....#
.##.....##

Tile 3119:
######.##.
..#..##..#
.......##.
#...#.#..#
..#...#...
#..#....#.
#....#....
#......#.#
#.........
......#.##

Tile 1579:
.#.#.#....
#..#.....#
###.....#.
..#.#....#
####......
#.#......#
.......#..
...#.....#
.#####...#
#..#....##

Tile 1249:
#...####..
#.........
...#.....#
#......#.#
....##....
#..#..##.#
#..#.....#
....#.#...
#.#....#..
.##...#.#.

Tile 1319:
....#.####
#.#..#.#.#
#......#..
..#.#.#..#
..#....#.#
##....#.#.
###...#...
###......#
..#..#..##
..#.#..##.

Tile 1987:
.#...####.
...#...###
...##...##
..#..#..#.
##.....#..
#..#....##
.#.....#.#
#......#..
##......#.
..###...#.

Tile 2963:
.##.#####.
#.##...##.
#..##.....
..#......#
..##.....#
.#..#..#.#
..........
#...#..##.
...#......
##..#####.

Tile 1609:
..##..###.
.....#..#.
.....#....
##....#...
###..#...#
##....#..#
......##..
..#..###..
#....#.#..
...##....#

Tile 2267:
...#.#..#.
..##....##
####..#.##
....#.##.#
##..#.....
..##.#....
#.##...###
#..##.###.
..........
..#..##...

Tile 1531:
#.#..##..#
#........#
#.....####
....#...##
#.....##.#
#.........
......##.#
.#.#..#..#
.........#
#..#.#.#.#

Tile 2357:
.......#..
...#..#...
.......#..
.#...#.###
#.####...#
.#.#.....#
#.#.....#.
.###..##.#
#.#.#...#.
##.#.#####

Tile 1373:
.#.....###
..#...##..
..#......#
...#...#.#
.#..#.#..#
......#...
#...##..#.
#...#..##.
...#......
##.#.#...#

Tile 3373:
...##.#.##
#.........
....#....#
#..###...#
#...#...##
......#.##
........#.
.......##.
#..#...#..
.##..#.#..

Tile 1831:
...#####.#
#........#
#.....#..#
#.#...###.
...#.##..#
..##.....#
..#....#..
......##.#
##...#....
##.###.#.#

Tile 3607:
#...#.#...
.#...##.##
.#.......#
..####....
####....#.
.#.#...#.#
###...##..
#......#..
###.#..#.#
##.##..###

Tile 3251:
##.#..##..
.#....##.#
#.......#.
..#..#..#.
#..##....#
#...##..##
.....#..##
...#...##.
....##....
#####...##

Tile 3917:
.##.......
.#..#...#.
....##...#
#.#..##.##
.#.#...#..
..#.......
.....#..#.
.#..#.#..#
...##..#.#
##..#...##

Tile 1489:
.##..##.#.
##...#.#.#
#.......#.
##.#.....#
.#...##..#
..#.......
.#..#....#
###..#..##
#..##.....
#######...

Tile 3253:
#..#..####
....#..#..
#.##..#...
.##....###
........##
..........
#...#.....
##...#...#
...##.....
.##..##.#.

Tile 2081:
.......##.
#...#....#
#..#....##
##....##..
#.#.###...
.....#....
#...###.##
##..##....
#........#
.#...#..#.

Tile 1481:
######..#.
###.#....#
.....#...#
#.........
#..#....##
#........#
..........
#...#....#
#..#.##...
#####..##.

Tile 2953:
..##.#####
#.#..#..##
..#....#.#
#...#.#...
#..##....#
#.#....#.#
.......#.#
#..##.....
#.#.....##
...##..#.#

Tile 1783:
.#..#.#.##
#...#....#
...#......
#.#.#..#.#
.........#
......#...
.#...#..##
..#...#.#.
#..#..#..#
...#..#.##

Tile 2269:
#.#.##..#.
#.....##..
.#.......#
..#..#...#
....#.....
..........
##.##....#
.....#....
##.....###
.#.#####.#

Tile 1277:
..##..#...
#...#.....
.......#..
...#......
....#....#
..#.......
#......#..
.#........
#.........
###.###...

Tile 2633:
####.#.#..
......#.#.
........#.
#.........
...#...###
...#.....#
.##.#..#.#
.#........
#.#......#
.##.##.#.#

Tile 3919:
###..###..
...#......
...##.#..#
#...#..#.#
.........#
...#.#.#..
...#.#....
#....#...#
.#.###..##
.#.#.##...

Tile 1753:
#...##....
#.....#...
..........
.#..#.....
#.......##
..#.......
......#..#
#..####...
....#..#.#
..#.####..

Tile 3739:
.##.#.#..#
###.##.##.
##.###.#..
.#....#..#
#...##....
#..#......
#....###.#
..#.......
###....#.#
...###..#.

Tile 3701:
.####..#.#
#.....#...
.#........
.........#
...#.#....
#.#.......
#...#....#
.###.#.##.
#.#.......
##.##..##.

Tile 3631:
#..#..###.
.........#
#...##....
.......#.#
.....#....
...#..#..#
........#.
..#....#.#
......#...
##.#..#.#.

Tile 2389:
..#.##...#
###.......
#..##...##
#.######..
..#..##...
.#...#...#
.....#...#
#..#....#.
..#...##..
#...#..##.

Tile 1123:
.##.#.###.
#........#
#.........
#.....##.#
....#.....
....##.#.#
....#....#
#.#.#..#.#
#..#.....#
#.#..#..#.

Tile 2659:
#.##.#.##.
.#........
#....#..##
###..##.##
#.#.###..#
#...##...#
...#...#..
...#..#...
..#.#.#..#
####...#..

Tile 3709:
.##...#.##
..#..##..#
......#.##
......#..#
....#.....
..........
#......#.#
#....##..#
.....##..#
####.##...

Tile 2887:
######..##
#.........
.......#.#
...#.#....
#....##...
#...#....#
...#.#...#
#....#...#
##....#..#
#########.

Tile 1367:
#.####.###
#......#.#
..........
#....#....
.#......#.
#.........
...#.#.#..
......#...
##....#...
.....##.##

Tile 2221:
##..#.....
#.....#...
.........#
#..#......
#.#...#...
###......#
...#.....#
#........#
#.....#...
..##.###..

Tile 2621:
##..#.###.
.......###
.........#
..#.#...#.
..........
....#.....
#.##..##..
......#..#
....#...#.
##.#.....#

Tile 2503:
..#.##.##.
......#...
#.#..#....
....#....#
#.....#...
..#......#
##...#..#.
#....#..#.
...#......
#....####.

Tile 3491:
#..#.###..
#.........
#..#..#..#
###....###
#.#.##....
#........#
......#...
##...#...#
##........
.#.#.##.##

Tile 2333:
..##.....#
.....#..#.
#.#.#..#.#
##.##...#.
..#.#.....
...#.....#
..###..#..
###.....##
#........#
##.####.#.

Tile 2399:
.....#.##.
##.....#.#
##.......#
.#.#..##.#
#...##...#
###.#....#
#......#..
##......#.
#.#....#..
.....#.#..

Tile 2683:
#.##..##..
.#....#..#
.........#
.......#.#
.##.#..##.
.....##..#
#....##..#
....##.#.#
...###.###
.#.##.#.##

Tile 1741:
.##..#.#.#
.##....#..
##..#.....
##...###..
......#..#
#.....#...
.......#..
#.....#...
...##..#.#
#..###.##.

Tile 3539:
.###....##
..........
.##.#...#.
.....#...#
.........#
.........#
.......#..
..#.....#.
.#.#.###.#
#.#.####.#

Tile 3929:
##..######
.#..##.###
#.##....#.
...##..#.#
#...#.....
........#.
.....#...#
#...#.#..#
.#.#.....#
..##.#..#.

Tile 3221:
..####..#.
##......#.
..#..#.###
##........
....#....#
.#...#...#
#.#...#..#
##....#..#
##......#.
.#..#..##.

Tile 1117:
.##.##.#.#
##.....###
#.#.#.##..
..#....#..
##....#...
...#..##.#
..#.#.#..#
...#...#.#
....##...#
.###.#..##

Tile 3163:
.####.#.#.
#.........
..........
......#..#
....##.#..
.#....#..#
.....####.
#....#.#..
...#.#..##
##.#.#....

Tile 1129:
.....##...
##......##
#..#..#..#
..#....#..
#.#...#...
....#.##..
...#...#..
#.........
#..#..#.#.
#.#..###.#

Tile 3079:
###..#.##.
....###...
##..#....#
....##...#
......##..
....#....#
#.#.#.....
#...#....#
.#........
.##.#.###.

Tile 2039:
.#.#.##..#
..##......
.##.......
#.........
....#...##
#..#.#...#
..........
#.....#..#
####..#..#
.#..#..##.

Tile 1901:
####.#.##.
#.#...##..
...#....##
..........
#.........
#...##...#
..........
##......##
.#.....#..
.###.##...

Tile 2029:
####..##..
##...#....
.#....##.#
..#....##.
#........#
..........
..........
#..#..#...
#..#....##
....####..

Tile 1879:
##..#.##.#
#...#...##
.##...#.##
.#.#.....#
.##.#..#.#
#..##.....
#..##...#.
......#.##
...#..#..#
#....#..#.

Tile 3347:
###..#..##
.#..#....#
.#...#...#
....#....#
.#......##
......##.#
.....#..##
.........#
#......#..
.####.#..#

Tile 2417:
####.#..##
...#.#....
.#....#..#
.#........
#.........
.....#....
#..##.....
#..#...#.#
#..#.##..#
..##..####

Tile 2897:
...#..#.##
#.#.......
.........#
#.#.#..#..
..#.......
.........#
.##...#..#
....#..###
.###...##.
.#.#.#.#..

Tile 2837:
.#....###.
.......#..
......#..#
#..#..#.##
#.#.##...#
#...#....#
...##.#.#.
#.....#..#
#.#.....#.
#####.####

Tile 3001:
##....#..#
#....#.#..
.....##..#
.....##..#
...#......
#.....#..#
#.#...##.#
.#...#...#
........#.
###.####.#

Tile 2153:
.####.##..
#..#.....#
..#....#..
#......#.#
.........#
.###.....#
#.##.....#
#..#......
#.#.......
.######..#

Tile 2749:
..##.###..
#..#...#..
..##.....#
###.......
#..##.....
......#..#
..#..##...
.###.....#
#.#.......
...#..#..#

Tile 2791:
..##.##.#.
.##.###.#.
.#..#....#
.##..#.#.#
#..#....#.
......##..
#.##.#.#.#
##...#.##.
.#...#.#..
...#.#...#

Tile 2707:
..###...##
.#....#...
..#...##..
#.#.......
....#....#
#.....#...
.........#
##.....###
#........#
#.##.#####

Tile 2969:
..#####..#
..........
#.....#..#
....#.#..#
...#......
..##.#...#
...#.#....
...#.....#
..#.......
####..##.#

Tile 3329:
#.###..#..
...#....#.
........#.
.........#
###.#..#.#
.#.......#
.........#
.......#..
#....##..#
##.###.#..

Tile 3989:
.#####.###
#....#.#..
.........#
.........#
##.#...#.#
......#..#
...#.....#
###......#
.#.......#
.###.##.#.

Tile 3583:
..#..#.#..
.#........
###...##..
##.....#..
##.......#
#.........
#.........
#...#..#.#
##.#......
#.........

Tile 3931:
....#..#..
#.#.#.....
..#..#..#.
.##...##.#
.......#.#
.........#
....#...##
......#..#
#.#....#..
#.#.#.##.#

Tile 1493:
####..##.#
..........
....#..#..
##...####.
#.........
#.........
.#.#.....#
....##...#
......##..
.#########

Tile 1013:
#.###....#
....####.#
...#.####.
.......##.
.#..#....#
#.........
#......#..
...#...#..
#.....#...
#.#.#####.

Tile 2089:
.#...#.##.
..#...#..#
........#.
...#.#..#.
.....##..#
#...###...
#....#.#..
#....#..#.
#...###..#
........##

Tile 1613:
#.#...####
#.....#...
#....##..#
#....#....
.#....#...
#........#
......#.#.
#.....#...
##...#...#
....#..##.

Tile 3331:
.#.#.#####
.....#...#
.......#..
##..#...#.
#........#
......#..#
..#.#....#
.......#..
..#.......
###.###..#

Tile 3217:
..#.###...
#.##......
..#...##..
...#.##..#
###...###.
#...#.#.#.
##.#...#..
.........#
.....##...
##.###....

Tile 3011:
#.##.#....
....#...##
..........
#........#
#....#...#
#..#......
..........
.........#
....##....
##.####...

Tile 1171:
###..###.#
..#.....##
##.#...#..
.#.......#
..........
##.....##.
.##.....##
..#...####
##.##..#.#
#.#.#.#...

Tile 1933:
#####..#..
#.#.#.#...
#.#.......
#.#.#.....
.....#...#
#.#.......
..........
...#..#...
.##....#..
......#...

Tile 2141:
#.###...#.
#.....#...
.........#
.....#..#.
#.###.....
#..###.#..
.....#..#.
....#....#
.#.......#
...##..##.

Tile 1031:
###...#..#
#.......#.
##.#..#..#
##..#.....
#...#.....
##....#...
#......##.
#.........
......#..#
....#..##.

Tile 2857:
#.#..#...#
#........#
##.#..#.##
....#.##..
....#...##
#...#.....
#..#......
.......###
#...####.#
.#...####.

Tile 3313:
.#......##
...##.....
.#.##.....
#.#.#.....
#.#..#....
..#.......
#..#.#...#
#.#......#
##...#....
..###.##.#

Tile 1303:
#.##.#....
#.......##
...#...##.
....##...#
.....#...#
.........#
#..#...#.#
##...##...
...#.#.#..
....##..##

Tile 3457:
#...#.#..#
.........#
#.#..#...#
.....#.#..
........##
...#......
..##.##...
..........
....#....#
#...#..#.#

Tile 3643:
##.##..###
..#.#....#
..##.....#
..#.......
#.........
#.........
#.......#.
......#..#
.#......#.
.###..##.#

Tile 2803:
#...#..##.
##....#.##
.#.......#
#...###...
#...###..#
#..#......
.......#.#
#.......#.
...##.....
..#..##.##

Tile 2549:
.#....####
.#........
.....###..
.....#..##
..#.###...
......#...
.........#
.......##.
#......#.#
..###.####

Tile 3571:
#..#.##...
...#....#.
..##..##..
....#.##.#
.#.....#..
.##...#...
.##......#
#.#......#
......#...
.#..#..#..

Tile 1327:
#..###...#
#....#...#
#.##...#.#
#.......#.
#.#...#..#
.........#
##........
....#..#..
.....#..#.
.#.###..##

Tile 2833:
##...#..#.
##....#..#
##.#.....#
.......#..
###...#..#
.........#
#.#....###
....##.#..
..#...#...
..#.#.#.##

Tile 1997:
##..#...##
#...#.....
#.....#...
.......#..
.......#.#
#.........
#......#..
.........#
..#.#.#..#
.###.##..#

Tile 3673:
..#..###..
.#..##....
#.##.....#
.####..###
##..#..#..
#.#.#...##
.#....#.#.
....#.##..
#...#....#
..#.##....

Tile 3323:
.#...#####
#..#...#.#
..........
#....#...#
.#........
#........#
##......##
...##...#.
...#.#..##
#.##.###.#

Tile 1619:
#..###.###
#...##...#
.#........
#.#.....#.
#.##.....#
#......#..
.......#..
#####.#..#
..........
####...#..

Tile 2477:
.##.##..#.
#.#.#.....
......#..#
......#.#.
##.##.....
.###.#..#.
.........#
...#.#....
#.....#...
...##.#...

Tile 3389:
#...#.#.##
##.#.....#
......#..#
#..#.#...#
#....#..#.
##......##
....#...##
#.##..##..
#........#
...##.#.##

Tile 1193:
.##...###.
#...##..#.
###.......
#.#..#....
.#..#.#...
#..##....#
#.#..##...
####.....#
#...#..#..
..##.....#

Tile 3767:
#...#.....
...#.#..#.
#######...
...#.#....
#.........
.#....##..
#.#.......
.#.#.#..#.
..#.###..#
....##.#..

Tile 3191:
#.##...#..
..........
......####
#..#..#...
....#.#.##
......#.#.
.....#....
.#....##.#
..#..#.#.#
#...##.#.#

Tile 3049:
#...#...#.
.#..#....#
##.#.....#
...#...##.
#.##.....#
.....#.#..
..##..##..
..........
#....#.#.#
#.##.#..#.

Tile 1747:
###....#..
....#..#.#
.......#..
........#.
#..#......
.......###
...#.##...
.....#..##
.#....#...
.##..####.

Tile 1429:
...#.#.#..
.#...#....
..........
##........
#.........
#..#.#....
.##.......
.#........
.##.#.#..#
#####.##.#

Tile 2909:
#.##.###..
#..#.....#
#...#.....
.#..#.##..
#.#.....#.
#...#...##
...#...#..
.##.....##
##..#..#.#
#.###..###

Tile 1973:
#####.#.#.
...#.....#
#.........
#........#
.....###..
..........
.#.....#.#
.##..#.##.
##.#..#...
#.##...#.#

Tile 3449:
.#.#.#..#.
#...#.#...
..#.##....
#...###...
.....#...#
#.......#.
##....#.##
......#..#
#.......#.
..####....

Tile 3371:
##.#..#.#.
#...##...#
##...##...
.#....#...
#...##...#
..#......#
......#..#
##.###...#
#....#...#
.##....#..

Tile 1103:
####..#..#
.......#..
#......#.#
..........
.#....##..
.#.##...##
##....##.#
..#..#.###
..........
.#.#...#.#

Tile 1723:
.#....###.
...#.#...#
..#.#.....
#####....#
.#.#......
....#..#..
..#.......
..##...#.#
....###...
...##.#.#.

Tile 1093:
..##.#.###
.##.......
#....#....
.#.....#..
#.......##
.........#
.#......##
#......#.#
#..#.#...#
#.#####.#.

Tile 1321:
#.#.###.#.
.#..##..#.
#.###..#..
#..##....#
#.......#.
....#....#
#....#...#
.....#....
#.###.....
.###...##.

Tile 1979:
#####.###.
#.#..#..#.
#.####....
###.....##
....#.....
#.........
###.#..#.#
..##...###
#.......##
..#.......

Tile 2677:
##..#.##.#
#.##.....#
#..#...#.#
..#.#.#..#
##.#..#..#
.....#..#.
#...#...##
...#...###
##.#...#..
.###..##..

Tile 1553:
.#...###..
.........#
###.......
##..#...#.
..#...##.#
..##......
.##.......
...#..#...
#....##...
#####.#...

Tile 3187:
###..#..#.
#.....#...
.....#..##
#.....#..#
..#......#
##...#....
......##.#
......#...
#.#..#....
########.#

Tile 2243:
##.####...
..#.#.#..#
#..#......
.........#
....#...##
#.........
#......#.#
....#....#
......#.##
##.#....#.

Tile 1423:
..#..##..#
..#....#.#
#.#.....##
##.......#
.#..#..#..
......##..
##...##...
.....##..#
...#.#.#.#
##...###..

Tile 3319:
##.##.#.#.
#.......#.
....#.....
###....###
........#.
###.......
.#.....##.
#...#....#
..#.......
.##...####

Tile 3343:
.#...#####
..........
.###...#.#
.##.......
..........
##....####
#......#.#
#.#.###..#
..#.......
.#####..#.

Tile 2789:
##....####
.##.......
#......#.#
.#.##.....
#......#.#
#........#
##......#.
.#...##...
#.....##.#
...###.#..

Tile 3623:
##..#..#.#
#.....#.#.
##.....#.#
.#...##..#
.#.......#
..#...#..#
.#..#..##.
###..#.###
.#.##.#..#
.#.##.#.#.

Tile 1439:
.#..#.#.#.
....#.....
.........#
#..#......
##..#....#
#.#.#.##..
##.#.#...#
#.........
#.......#.
####...#.#

Tile 1361:
#.#...###.
#.........
....#.....
..#.......
......#..#
##....##.#
.##..#..##
..#......#
.....##..#
#..#....#.

Tile 1823:
.##.#.#...
#.........
#..#.##.#.
#...##....
##........
#...#.#.##
#........#
.........#
........#.
#.###...#.

Tile 2971:
.#.......#
#.#....#..
#..#.#....
...#..#...
##....#...
#..#......
.........#
....##....
#...###..#
#...#.#.#.

Tile 3229:
..###.####
#........#
.#.......#
..........
##.......#
#..#......
.#.#......
#.....##.#
.#......#.
.#####.#.#

Tile 1907:
####.#.#..
....#....#
#....#....
#.........
#...#..###
...#.#.#.#
....##....
#.###..###
..#..##.##
..##.##...

Tile 1699:
..#..##...
.#..#.#...
.....#...#
.#.#.#...#
........#.
##.....#..
#.#......#
.##......#
....###.#.
...##..###

Tile 2131:
#..#####..
#....#...#
#..####..#
......#..#
#.#..#...#
#.#...#..#
..##......
#.#####..#
......#..#
#...####.#

Tile 1657:
..#..##.#.
#....##...
##..#....#
#........#
#.........
#.......##
#......###
#.#.#...##
........##
#.##.#.##.")
