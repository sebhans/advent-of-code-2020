(ns advent-of-code-2020.day24
  (:require [clojure.string :as s]))

(defn- parse-tile-list
  "Parses a tile list into a sequence of directions."
  [s]
  (:directions (reduce (fn [state c] (case c
                                       \n (assoc state :prefix :n)
                                       \s (assoc state :prefix :s)
                                       \e (update (dissoc state :prefix)
                                                  :directions
                                                  #(conj % (case (:prefix state)
                                                             :n :ne
                                                             :s :se
                                                             nil :e)))
                                       \w (update (dissoc state :prefix)
                                                  :directions
                                                  #(conj % (case (:prefix state)
                                                             :n :nw
                                                             :s :sw
                                                             nil :w)))))
                       {:directions []} s)))

(defn- parse-tile-lists
  "Parses a sequence of tile lists (one per line) into a sequence of direction
  sequences."
  [s]
  (map parse-tile-list (s/split-lines s)))

(def interpretation-of
  "Maps directions to position deltas."
  {:e  [ 1  0]
   :se [ 1 -1]
   :sw [ 0 -1]
   :w  [-1  0]
   :nw [-1  1]
   :ne [ 0  1]})

(defn- add-positions
  "Adds two positions, returning the sum."
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn- flip-tile-at
  "Flips the tile at the given position."
  [floor position]
  ((if (floor position) disj conj) floor position))

(defn- flip-tile
  "Flips a tile identified by the given tile list."
  [floor tile-list]
  (let [position (reduce add-positions [0 0] (map interpretation-of tile-list))]
    (flip-tile-at floor position)))

(defn- flip-tiles
  "Flips the tiles identified by the given sequence of tile lists."
  [tile-lists]
  (reduce flip-tile #{} tile-lists))

(defn solve-1
  "Returns the number of tiles left with the black side up after all of the
  instructions in s have been followed."
  [s]
  (count (flip-tiles (parse-tile-lists s))))

(def neighbour-deltas (vals interpretation-of))

(defn- neighbours-of
  "Returns the six positions neighbouring the given position."
  [position]
  (map (partial add-positions position) neighbour-deltas))

(defn- neighbours-of-black-tiles
  "Returns a sequence of positions neighbouring black tiles. If a position has N
  black neighbours, it will appear in the sequence N times."
  [floor]
  (mapcat neighbours-of floor))

(defn- daily-flip
  "Flips all tiles according to the daily flipping rules."
  [floor]
  (let [n (frequencies (neighbours-of-black-tiles floor))
        black-tiles-to-flip (filter #(let [n (n %)] (or (nil? n) (> n 2))) floor)
        white-tiles-to-flip (filter #(and (= (n %) 2) ((complement floor) %)) (keys n))]
    (reduce flip-tile-at floor (concat black-tiles-to-flip white-tiles-to-flip))))

(defn solve-2
  "Returns the number of black tiles after 100 days."
  [s]
  (->> s
       parse-tile-lists
       flip-tiles
       (iterate daily-flip)
       (take 101)
       last
       count))

(def trial-input "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew")

(def real-input "neswsewswseswseenwseneswseswswswseswse
nwnenwwnwnwneswnenweswnwnwnwnenwswnese
wnewwwwwwwwswwwsewnwwswnee
wseeeeeeesesenesenweseseeseesw
nenenenwneenwnenenwnwswnenwseswsenwenw
sesenwsesesenweseeseseesesesesenwsesesw
sesesewseswsewesenwneseswswwsesesesenese
ewnenenenenewwneeneneneneseneenew
eswswswwseswswsesese
ewnwnwnwnwnwnwwswwwnwnwew
swswswswswesewswswsweeswswswneswwsww
nwwwesesewnwwwnewwwwwwenwsew
wnesenwneeneneswnesenewnenwnesenwnenw
swseswweseswswswswswseewse
nenewnwsenwnwnenwnwnwnwnwnwnwnw
neneneswnwnwnenenwwsenenenenenenenwsenenw
eeseeeeneneseenwseeweseeeeswe
seneswesesesesewsw
swswwsweswseneswswswseswnweswsesenwwswsw
swnwnwsenwwnwnwnwwenwnwwnwwwnwnenw
sesewseseseseseesesenwse
neneneneenenenwneswneenene
swswswewswnwsewswswseswseswswseswswswne
nenwnwnewneeenenwswneneneseneswneesw
neswwseswnwswswsesewesenwnwswsenewe
neeneneenesenwneswnenenwnwwnwnenesenesw
seseswswswneswswswswnwswneswsewseseesw
wnwwseswnwenewenwnweewswswswsenw
eswwenweseewesenw
swsewneenewnwseseweeswneeseneew
eeeeeseenweeee
eseswenwwseseseese
sesesesenesesesesewseesewseneeweesese
wwnwnwnwnwesenwnwwnwwsw
eeswnwnweesweeenwsewswswenewnesw
swswswswswswnweswswswsweswnwswswswneswsw
wwnwwnwnwwnwnwenwsenwsewwnwswnwnenw
seseeseseseseswneswsenwsenwsesesesesw
enwnwwnwnwsweneseswsenwwenwnwnwenw
seeenenwewweseseseswseewenesesee
weswswwswwswwswwswwnwswseneswwwnw
eeeeneseeneeeswneenweneneeewnesw
nwwswswnweswswwwesewswswswwsw
seeseneswneswswnwenwenwnwwneseswneee
neneneseneneswneenenenenwnenene
enenenewneswenesenwneneseneenenewne
senwseseseesweseesenwnesesesesesenesesew
seesesesewsewnweseeeeneeseseeese
wneseswenwswnenwnwwswseseswneesenew
nesenwswwnwwseewnewwwsewwnwwsw
seseeswswneswneesewnwnwenesewse
wswnwswswwwweswswswwsw
eeeeweeseeneee
swnwswnwseeeswenwnweswnwswswsewswswswne
nenesenwnweenwneeneswneneeneeneeswnesw
senenenwwnweneswnwnwwnwnwnwenwnwnenenwne
swewseeneswneweneeneeeswnwneneeee
nenwwwswwnesweewwseswswswswewwew
sesesewnwseneseswsenesesesenesewesesesese
nwnenenwwnwwswsewswewswnwseeeew
nwwweeseneeesweeeeeewew
wwnwwwewwwwnwsewwwsew
neewnwnwnwswswnwnwnwswnwnwnwnwnwnenwnw
eeeeenweeseeswseeeseesweneew
wwwseswswswnewwwswwseneswswswsww
nenesweneweeswswwnewnwnenweenee
nwwwswwwwsenwwwnwwwwwenwsenww
nwnwnwewnwnwwseenwnwseneenwwnwnwnwse
wnwnwsewnwnwsenenw
wwesewnwswenwswseenwneswswseswnwwww
nwnwsewwwwwswwenwnwwnewsewwewnw
wnewsenwsewseneseseeseseseseseneesee
neneneneneeneswneswswnenenenenenenenwnw
enwnwnwsenewnenenwnwnwnwewnwnwwnenw
wwwseneseswseeneesenesenewseswenwsw
newneeneneneswnenwneneeseneneenenee
neeswnwswswnewwnwewwswnwseewseswe
seseesesewsesenwweweswesese
wsewnwswswswswswswswnwseswswseeneneswwsw
senewneseswseseswswenesenwswswswswswswsesw
wswneswwwswswwwwswswneswseewwe
newwwwswwwwwwnwsewewwww
sesewsenewseneswwnewswswswseswnenene
nwneneneewnwswnenwnenwneseneenwneneswsenw
eweeesweseseneneseeeeeenwsee
wswenwenwswswseswsw
sweenweeseneenweeeene
nenwseneeeeseswnweeeeeswwneenee
wewnweswwwwswwsewswnewwwswwswsw
swseneeswwswwswwwswswswseswswswswne
wneneneneneswnwwneeenwneneneenwnenw
swnwnenwnwnwenwnenwnenwnwswnenwswnwenwnenw
nenwnenwwnwnwweswneseneseneneneswnwnwsw
weenwswnwnwwwnwnenwsenwnenw
seseseeneeswseseeseesewswswsewsenwnwne
nwwswwswwswwseswnewweseseswswwne
newwenwewnwnwnwswnwswwenwnwsewnwnw
seswneswswswwswswswnwswswesweeswswswnwsw
swswswseswswseneewsewwwneswwswneseswne
eenwseeeeneeeeeseewswseewe
wwwwwwwwwnesww
senwewenwseseeeeseswseseseeesee
neswnwnwnwwenwnwwwsewnenwnesesewswne
ewesweseneneeeseesewsewewsenwnenw
newwswwweswsweswwwswwwwwwwe
nweeenesweseneweenesewene
enenweseseneneswwneswewseswseeew
seneswsenwnenwseseswsenwseseseswsesesew
seswnenwwnwswnweeeweseswnwnweesw
wwnwwwwenwenwwnwwewnwnwnwnwnwsw
swswnwswnewswswswswwwswsweeswswnwswswsw
nenenenenenewenenenenenenesene
swseeseswneseswneswswseswswswwswnwneswsw
sewseneseseeeeseneew
nwenenwsweeswwnwnweenwswswswenwnww
eeeseseswsenwwweeswnesenwswswneww
nwnenenenenenenenenwneseneneswneneenwsene
seneseesweeeesee
eneswseeeseseswseeeewnweesewenee
wwsewswswswneswswswnew
neseeswneneswneneneenwnenwnew
eseseewsewewneseeenwenwesesese
nenenenenenewneneeneneenee
sewewsesenwwwseeneneswnenesesesenw
nenenwneswneswwneseswneenwnwneeswnwnwe
seeeenwseeseeseeeswneeneweewsee
nwenwswnwnwenwswnenwnwnwswnenwnewneenw
eswwnwneswswsenwwenenwseswseseesene
eesewswseseeenwwnweseewwnese
neneseneneswneneenewnwneneneswnwneneene
esesenwswesesewwnesenwewswswsenwswse
sesewnwnwwnenewnwwwswsewne
wnenwnwnwneenwnweswwnwswsenweswsesene
wnwneneseeesewsesenwnesw
esesenwnwswsewswnwseneeseseseesesenwse
swnwnweseeeeeeeeesenwsewnwsese
nwswnwswsweswseseseseswswswswswwneseeswsw
wneenwseewsenwsesesewneswsesweesee
wnewswwsenenwnwsewnwnwnwnenwnwsww
swewswnewwneswwnweeenweenenesene
sewneswseneseseswswswseseseseswneseswse
eeeenweeeneeseseewneeesweese
wnwneswswnwnwnenwsenenwsenwnenwnwnenesw
weneswnweneeneeesesweeneenenww
seswneseseseswsesesesese
neeeneneswneneweenenwnenwswneesene
swswswsewnwnwseseeswswswnwswswsweswnene
seseswseswwnenesese
nwneswwseneneneeewwseneneenwesww
wnwnenenenwsenwnwe
nwwenenwenwswsenwwwsewswwnenwnwesw
nwnesenwnwswsenwnwwnwwnenwwnewnwww
sweeenweeeeenwsweeeeeeswneee
eeseseswnwnweeeeneeseeesewnwee
seswsesewweneswseswnweswneswsewneswsw
wwwwsenewnwwwwwnesewswwneww
swwnwnwseseswewsenwswswewnw
wenwwnenwsweswnewsewswwsewnwweesw
seeseeneeeeswwenwseeeswseeeee
nenwneswnewseswnenenwsenewnenenenwsene
neneneneswenenenwwneeswneenenenenenene
enwnwnwnenwwsenenewnwnene
nenenwneswnwseeswnenwneseeneneeswnene
nenenesenwnewnwnwnesenwnenwnwnwwnwnene
wwwseseeneswseswnwsewswswseneeene
wwwseesesenwwwwwnwwnewwswsw
wwswswwnwnwesewseswwnweswnwseeww
neswnwswnwswenesewnew
wewsewnwnwnwnwnwnenwnwwnwnwnw
swsesweswneswnwneneeswswwwswswswsesenw
wwweswwwswwwswswwswnw
seseswseswsweswnwseswswsweseswseseswnw
eswwneswswswneeswseswwwswswewene
swsewnwneswnwswswseeswsewnwseswnwseese
neweswswswswswsweseswnwnwnesenwswneswsew
nenenenenewneneneneneeneswnwwnenenee
newnwwsesenwseeseseseeeesweneseee
eneeeeeseeeeeswee
wswweneneseseswswseseneswswseswswwsw
ewnenwneeseneeneneneseeneneweneee
swwwswwenwnewswseneswswswswwwwsw
eeneesweenweeeeeneweeeesew
nwwsenwnwnenweseeweewsewsenewswnesw
nwwnwnwnwnwnwnwwenwnwnwnwnwnewswsenw
nwnenwnenwnenenweswnwnenwnwne
sesenenwenwnenweseneneeneneseenenew
seseeeeseeswenwse
seseseeenwseseeeewsweseesenwee
seseseseeseswnwsesweneseswsenwsesesesese
swwseswswseswswwwneswswwwswnesw
nwnwswwneeswnwnwwnwnwwsewnwwnwew
newneswseswwwswwwwnewwsenwwswswswse
nwwnwnwswenwnwwwnwnweswnwwewnwswnw
wwsewwwwwneewwwwswswnwweww
nwnwnwswnwnwnwnwnenwnwnwnwnwnw
nwnwnwnwwnwswswwewwnewwne
swwneeswwnwnenenenenenenenenenenenwnese
eeneeneeeweeeewnesenweeeesw
eseewwswwwnewwwnewwsewwwew
eswwsenenwsenewesesesewesesesenenwse
swswswswswswwwnwswseswsw
eeseenewswwnesewweneeeenweseese
wneseswswneswnwnewnewsenewnesewwsww
swsenwseneswneswswswswnwswseswwwswswwsw
nwnwnwsenwnwnwnwnwnwnwnwnwnwnwseswswnene
nwsweseeseseswsenwenwseenwsw
seswseneseswseswnwnwsewswe
swsweeenwnweneswe
enwwnwwnenwnwwewnwwwswsewnwnww
nwewwsweewswsweswnwewneswnw
wnenenwnenwnwwseneneneeneswnenesenene
enwwswesewesene
nenenwneneswnewenwnenenwswseenwwnwwsesw
senwnwseswswswswswseseswswsenwwswswswseee
eeeseeseeeewee
neeswnweenweeswnwswwenenwneswneeene
neseenenwseeeenwesweswwwswenenene
nwnwwnwnwswseneneswwnenwsenwnwnwewnwswne
sesesenwseseseseesenwseseswsesenwsesese
nwesewseswsesesese
wwswseswnwswwswnwsenesewsenwwwnene
swnwwnenenwnwnwnenwnwnenwnwsenwnwne
nenesenesenenenwneenenenwsesenwnesewnw
senwwwswwnwwnwwnwwnwwnewsewsew
eswnwswswwswswswwswsww
wneneseneewnenene
sewesweeeeeeseneweseseseneese
eeeseenwesweweewseenweee
sesesenesenwsesewseswsesweseswsenenwsw
eenwnwwswswneewswnwnwswwswenewne
senwnwenenewwnwnwwwesenwnwnwnwnwew
seewwseneswenweeeseseseseeeseenw
swwswswsenwswswwswneswwswwenwswnesesw
esenenwseeneenenenwenenewneeewnene
wwswseswsenwwwswnwswenewnewswsw
neeeeeeswenwseeneeneseeeenew
nwswswnwnwswsesewswswswneswnweseseseese
swseseewseseneeseseseeseneswsesenwe
swwswwnwneseneeswswseswswwnwswnenwew
seswsewseneneseseswwsesewseswseesenesw
swseswswseswswnweswswswesewwswswenw
wswnwnwsweneswenwnwsenewsenwwswnew
swewwwnwnwwwwwnwww
seeseseenesewseeeeeenesewneswesw
sewseseswseswswseswsesesesweswwne
nwnwwnwesewseswneswnwnwswnewwwneww
nenenewneenenenenenenewenenene
eeeesewswneeeseeeeeswwenwnene
esweenweeeeenee
swesesewswsenewsesese
seseswseswseswswseswswwnenweswseswswsw
swswswseswwswswneswswnweswswewwwww
sweeeeswneeeeeenweee
swnenwnwenwnenwnwneswnwswnwneneenenwe
sweseswswnwswseswwswswswswswswswswnww
wwswswwswwneneswwwwwwnewseww
seseesenweseswnwseseseswswseenwesese
nenenenenenenwnewneseenenenenenene
neeswnenwnwnewnwsweneeswnwnenwnwnenwnw
eewneeeeeeeneeenenwneneeswswne
wseswswesesesenwsenewseseeseswswsesese
nwewnwwwseswnwnenewswneseseneenwswsww
newwwnwnwwnenwwwwwsenenwwnwsesese
newwwwenwnwnwwwwwwewwwwsw
wneseneswenwswneswneswwnesewswswswew
esenwswsewsesesewsenesewseeneesese
nwswwnwnwwnwewnwwwwnwwewwse
eeswnwnwnwnenwneneseswseswewsewseseswsw
eseenenwseeswswseseweneswswnewenene
nenewsenwnenenenwneneswnweeeseneneswsw
nweswswnwesewnewsewwwwwnwwnenwnww
neeseenweneneneneeneesenewswwnee
swesenwseneeesesesesesewsewsenwese
wnwnewwwneewwwwwwwswwwwse
nwwnwnwnwnwswnwnwnwnwenwnwnwseesewnwnw
eneswswenwneneseswwseneeenwewnenw
neswnenwneneenenwnenenenwswneswnesesew
sesesesesesenwswnwseseswsewseseswsesene
wseswnwswseseenwnwswe
wsenwsenwnwnwneewwwswenewnwwnwsw
nwnwneswnenewnweswnwsenewneenwnwsene
newneewewnewneswwseeenewnwsesesew
eeseeswneseseswnwseswesenwswwenenwne
esenwswswsweswswseneseseswsewsenwsese
eswneneneswnenenenwswnesweeeneenenw
sweseseenwsewswseseseeseenesenwseesee
ewnwwwwwwswsewwwswswnewwnesw
neeweenwnesweeswneeswneneenenee
wsewwnwswwnwwseesewwswnenewwwe
sweswenwswnwwnenwnwnwneswnwenwnw
swnenewenwswsesewswnwwswwswwwww
senenwewsesesenwsesewnewnesw
wnenwseneswwnenenwswnwnenenesesenewneene
swnenwewswswswwwswseswneswswswwswswsw
wneeswnewwsewnwwwnwsenwwwwsenwnw
seseneenewwnenwenwnwsewwswsenewsene
eeeswseseseewneseesenwewswsenenesw
nesenenenewnwnwnenwnenw
neswewnwsewwewswsewwwwnwwwwne
nwnenenenenenesewneneneneneswne
swseswsesenwnenewnwwwwswswwwnenesesesw
swseseesesewsenwswseswsenwesenewwnew
seseneenweenwseeneenweneeeewnene
wneenwseseeenewnweneneesewsenewee
eewesenweeewneeeeeseeeeee
seswneseenwsesesenwsesesesesesesewswswse
seswnweswnwseswswswseswswseeenwswswswsese
eneswenenenewnenenenenwneneswneneswnwse
wsenewwwsewnwwwwwswneseswwww
nenenwswenwsenwswneweweswnewsenewe
esweeneeeswneneenenwneseneenwe
neseseswesewswsenwswseneswseswswswseswsesw
nwnenwseneneeneseenee
nenewneeeneeswewswwsenenenweenesw
nwnwseswnenwnwenwwseeneswwwnwnwwswse
seswswswseseseswnenwsesesesesenwswsweswsw
ewneswwwswnwwnwnwswwwesewenww
nwsenwnwnwnwwnenenenenenwnwne
wnwsenwnwwnwenwsesenenewnewwnwwnw
wsenwnwswsenwnwsewsenwnwwnenwenwsenw
esewsewenweswenesesenweseeseseswese
nwenwseenenwneneneewneneswnwnewnwwne
neenenwswneswneenenenenwnenwswwnenenenw
esenesweneesweswwenwneswnweswnene
ewwwwewswwnewwwwnwwwsw
esenesenwnewneseneneneenenenenwweene
swneeswseswwsewwwwewenenwswwnwe
sesesenweeseenwseswsesesewene
wswsenwseseswseeseseswnwwswsesenesesese
enweeeeeeeeeewsweenweswe
swswseswswneswnewswswswswneswswseswsesw
eseswenweneeseswswnwwweneeeee
sweswswswwswneswswswswswnesewswwswsw
swnenwnwnwnenwneenenenwnwnweswnenwnwnesw
eseweneneenenwnewswnwnenwswswswenw
newnenesenwneenenenew
ewswswswneenwnenenenenwnwnenenenenwsw
neneneswswenwenenenenenesenewnenesenenene
wnwwwsenwwwwwwwwnesewnwwsewnw
neeneeenwewneenweeswswesenene
seseneswswsenesesesenwswseswsesesesesewse
nwnenwswseeeswnenwnwnwnwwnwnwnesenwne
seswswswsweneswseswswswswswseswnwsw
swwnwnwwnwnwnwnwnwnwsesenwnwnwnwenwnww
enwnwnwneswnenwnwnwswnwnwnwenenwnwnwnwsesw
nwswswswswswseswneswswswswseswswswswsenw
wnweswneeeneeswswneeneewseneenee
seneesenwnwnesewnwwnwnwnenw
nwenwnenenwnwswnwnwnwnenenwswsenwnwenw
nenwnwnwnenwnwnesweseneneswnenenwnwswswne
wnwwwwnwwnwsenwnese
nwwswwwnwwwnwwswenenwwwsenwww
eswswnenenenwswenewneeesweeenenene
sweseneesesewsewweneeeeseewsew
eeneeeneeeeeeweseeeneeswsw
senwnwswnwenwnwwnwswnwenwnwnwnwnwnwwnw
eeswnenenweswneswwnwenwneeenwswsw
eeswswweeenweeeenweeneeee
nenwnwnwnwnwnwnwneswnenenenenw
seeseeenenweseeewsweewesesee
sweseswseseswneswswswnwsenwnwswsweswesese
nwwesewwwwwnwwnwwwwnenwswwnw
wseswwswwwwneswswwweewwswswwe
sewnwnwweseswnwswewswwswwnewsww
swwwwswwswnwswswwwswe
nwewneneswnwswswneswsewseeseewswse
swswnwswswseswswswsenwnwsweneneneseswwsw
eenenenwseeewseneenwseneneneenee
swswswseswswwneswnwswwswsweswswswswnew
sewnesweewnenewneneneseswneweneese
senwsewnwsenwnenwenwsenwnwnwnwnwwnwnwnw
senwneswnenwnwnwwnwnwsewnwnwsewwnwnw
swneneeneenwneeneeee
swwswwswnwswswswswwswswswsesw
nenwswnwnenwnwnwnenwenwnesweeswnenenene
sweeenwnewenwsenwneeeneswneneeseee
swenwenwsweswnweeeee
nwwnwnwnwwenwnwnwnwnwnwseswnwnwnwwneenw
nwnewnenwnwnwnwnwnwnwsenenwsenwnwnwnwnwse
swseweswswswesewnwwwnwwwseewnew
senwswnewnwnesenwwsewesesewnwseneswnw
eneeneeneneeenwneesweenwesewsee
sweenwswwwwwwwswswswswnw
eeseeseneweesewseseeneseswnwnew
wswnwseewnwwsenwwnewwwwsweseswne
nenwnwswnwnwneneneneenenenewnwnwsenene
wwswseswseswseswsewneswseseeswneswseswse
nwnwswwnwenwnwsweenwewswnwswnwnwne
senenenwwnenenenenenwsenewneneneswenwne
eeeseeneweneweenweneeeeesee
swneswswseswswwswswswswsenewswswswswwne
swneswswnwnwnwnenwenesweesewneenwswnenw
wwnenwnewsenwsesesenwwnwneswwewne
newenwwnwsewseneewseneeeneeneesw
enesewwnwnwnwswneenwnwswnwwwwnwwe
senwwwwwswwnewsweswww
nwswswwnewwswwwwwswwwsesew
nesweneewenwsweneseeneeenwnesww
seseswwseseswseseseswsesewneseswswnese
wseswnenwnewnwswneewnwwseenwenwene
nwnwwwwweseswwwnwwwwsenwnwneew
nwnwswnwnwnwswenenwnenwnenwnwnwne
seeeeeswwsewewnwwwenwwsenwne
neswnenwnwwwnenwenwnwnwneseneenwnenwnene
swseswseseswenwneenwswswswsewewswsw
nenwnwnenenwswnwnwnwnwnenwnw
seneneseswseswsesesesesesesenwwseseswsese
swwewewsewwswnwwneswswwwswwwsw
neeewseseeswesesenwnweewnwseswsesesw
ewsweneesenwnesewneseeneeneenee
nwnwnwnwnenwnwnenewnwenenwnw")
