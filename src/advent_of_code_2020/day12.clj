(ns advent-of-code-2020.day12
  (:require [clojure.string :as s]))

(defn- update-ship-position
  "Returns a function that updates the value of the position in the given direction."
  [direction]
  (fn [value position]
    (update position direction #(+ % value))))

(defn- update-ship-direction
  "Returns a function that updates the direction in position with the given operator."
  [operator]
  (fn [value position]
    (update position 4 #(mod (+ 4 (operator % (quot value 90))) 4))))

(defn- forward-position-in-ship-direction
  "Updates the value of the position in the direction of position."
  [value position]
  (update position (get position 4) #(+ % value)))

(def guessed-interpretation
  "Interpretation of the instructions as first guessed."
  {:start [0 0 0 0 0]
   :actions {"N" (update-ship-position 3)
             "S" (update-ship-position 1)
             "E" (update-ship-position 0)
             "W" (update-ship-position 2)
             "L" (update-ship-direction -)
             "R" (update-ship-direction +)
             "F" forward-position-in-ship-direction}})

(defn- parse-instruction
  "Parses a single instruction according to the given interpretation."
  [interpretation s]
  (let [[_ action value] (re-matches #"(.)(\d+)" s)]
    (partial ((interpretation :actions) action) (Integer/parseInt value))))

(defn- parse-instructions
  "Parses s into a sequence of navigation instructions according to the given interpretation."
  [interpretation s]
  (->> s
       s/split-lines
       (map (partial parse-instruction interpretation))))

(defn- navigate-from
  "Navigates according to the given instructions, beginning at the given start position."
  [start-position instructions]
  (reduce #(%2 %1)
          start-position
          instructions))

(defn- manhattan-distance-to-0
  "Calculates the Manhattan distance of the given position to 0."
  [position]
  (+ (Math/abs (- (get position 0)
                  (get position 2)))
     (Math/abs (- (get position 1)
                  (get position 3)))))

(defn- solve-with-interpretation
  "Returns the Manhattan distance between the starting position and the location
   the instructions in s lead to according to the given interpretation."
  [interpretation s]
  (->> s
       (parse-instructions interpretation)
       (navigate-from (interpretation :start))
       manhattan-distance-to-0))

(def solve-1
  "Returns the Manhattan distance between the starting position and the location
   the instructions in s lead to according to the guessed interpretation."
  (partial solve-with-interpretation guessed-interpretation))

(defn- move
  "Returns a function that updates the value of the waypoint position in the given direction."
  [direction]
  (fn [waypoint value]
    (update waypoint direction #(+ % value))))

(defn- rotate
  "Returns a function that rotates a waypoint vector clockwise if sign is +1 or
   counter-clockwise if sign is -1."
  [sign]
  (fn [waypoint value]
    (loop [i (mod (* (quot value 90) sign) 4)
           [e s w n] waypoint]
      (if (zero? i)
        [e s w n]
        (recur (dec i) [n e s w])))))

(defn- update-waypoint
  "Returns a function that updates the waypoint in position with the given operator."
  [operator]
  (fn [value position]
    (update position 4 #(operator % value))))

(defn- forward-position-in-waypoint-direction
  "Updates the value of the position in the direction of position."
  [value [se ss sw sn [we ws ww wn :as waypoint]]]
  [(+ se (* we value))
   (+ ss (* ws value))
   (+ sw (* ww value))
   (+ sn (* wn value))
   waypoint])

(def printed-interpretation
  "Interpretation of the instructions as printed on the back."
  {:start [0 0 0 0 [10 0 0 1]]
   :actions {"N" (update-waypoint (move 3))
             "S" (update-waypoint (move 1))
             "E" (update-waypoint (move 0))
             "W" (update-waypoint (move 2))
             "L" (update-waypoint (rotate -1))
             "R" (update-waypoint (rotate +1))
             "F" forward-position-in-waypoint-direction}})

(def solve-2
  "Returns the Manhattan distance between the starting position and the location
   the instructions in s lead to according to the printed interpretation."
  (partial solve-with-interpretation printed-interpretation))

(def trial-input "F10
N3
F7
R90
F11")

(def real-input "F12
W1
N3
E3
W3
F93
N2
R90
N4
L180
F13
E2
R270
F16
S4
L90
E1
S4
R180
L90
S2
F89
S3
R90
W1
F2
S2
W3
R180
N2
F41
E3
F74
E1
F20
W3
F32
E1
R90
F19
S4
E1
F41
L180
S1
L90
E5
L90
W2
F45
R180
F17
S3
E2
S1
F27
N5
R180
S1
E5
N1
L270
F60
L180
F18
N2
R180
E4
S4
L180
S5
L270
S1
F32
S5
F83
N2
R180
W1
L180
E2
S3
F94
N3
F82
L90
N2
W5
R180
S4
W3
E1
F81
S3
F45
R90
F51
R180
N1
W5
L90
E3
N5
E3
R180
N3
R180
F87
E4
R90
E4
L180
E4
R90
N1
L270
N3
E3
E3
L180
N1
W5
N3
F85
N1
E2
S3
L90
E5
N3
N1
F74
W2
F79
S1
L90
W5
F88
W4
N1
R180
F94
L90
W5
F79
R180
R270
N3
F83
W3
N3
L90
F33
S5
F93
L90
S1
W3
F24
L90
F80
S2
F86
F7
R90
E1
S3
W3
F23
R90
S1
W4
R90
N4
R270
S1
E1
F20
L270
F31
S2
L180
F41
E3
L90
N3
E3
N1
E5
F27
S3
W2
L90
E5
S1
F9
S3
E2
R90
S5
E2
R180
E3
F32
E3
S1
W1
F39
N4
F5
E5
F26
W4
S1
F54
S5
R90
F50
W4
S1
L90
S5
L90
F2
E5
L90
N1
W3
F75
L90
F85
S2
W4
F93
S4
E3
S2
E4
R90
S3
W1
F94
R90
N3
R270
S2
W3
N5
F1
S1
F32
E2
F17
N4
W2
S4
L90
W2
N2
L180
F94
R90
F78
N3
E3
N3
E4
F37
E5
L180
W2
L180
N2
R270
S2
F28
R90
S1
F15
E1
F50
N3
R90
F14
N4
F80
L90
S5
W1
L90
W5
F100
N1
L90
N5
W2
N1
F76
L180
S3
L180
E1
E5
F1
R90
W4
F95
S2
R90
W2
F29
W2
F47
W5
S5
W4
F58
L90
F22
N1
E4
N4
E2
S5
L180
W4
N2
W3
R90
F76
N1
F41
W5
F48
W1
N4
E2
R180
F79
N1
L90
S4
E2
F86
N2
E1
F5
W4
S2
L90
F3
R270
F43
L180
S5
R90
W5
R180
E4
S3
R90
S2
F41
F70
L270
E4
R90
F18
S4
F88
E2
L90
L270
F56
L90
N1
L90
F7
S3
F36
L90
F2
L90
F2
W4
S4
L90
E5
N2
L90
S4
R90
W1
R90
N2
E3
W2
S4
E4
R90
N5
L90
N4
F55
N3
E1
F80
W2
R90
S2
L90
W5
R90
R90
F87
L270
S5
R270
N5
E5
F10
E1
R180
N1
L90
S3
W1
F18
R90
S3
F89
L90
F42
S3
R180
F92
L90
F48
F11
W2
L180
F27
L180
N3
W1
S2
L90
W1
N1
W4
N1
R90
S2
W5
E1
F74
L180
F48
W3
F3
E3
F74
L270
F38
R180
N2
F83
S1
F78
L90
N4
F43
W3
W2
N1
L90
N2
W4
L90
S5
E5
W1
F83
E5
F70
N2
L90
N2
F66
S2
E2
R90
N1
F2
L90
F15
W3
F10
R90
N3
E5
N5
F57
R90
W2
F83
F91
W5
N2
R90
E4
S2
L90
N2
R90
S1
F49
L90
E3
L180
E2
F98
S1
R180
F23
N2
E2
F8
N2
L90
E2
N4
R90
F29
L90
E5
S3
F92
S5
F68
N1
L90
S5
F73
W4
L180
S1
R180
F74
R90
F27
R180
R90
F88
R270
S1
W1
F73
W1
S5
L90
N5
R90
F50
E2
R90
W3
F23
L90
W2
N1
E2
N2
F56
L90
F22
E5
F46
S2
L180
E4
R90
F10
W2
R90
E5
L270
S4
F55
R90
F17
R90
F8
L180
F25
E4
S2
E1
S1
F35
N5
W5
N3
F79
N1
R90
E1
F32
R90
F87
N2
R180
L180
F38
L90
S1
L270
S5
F63
L180
F50
W1
F23
S2
E2
F30
E3
L90
F79
R180
F90
L180
F81
R90
F72
R180
E4
N3
W3
R90
E5
R90
F81
S3
F66
S5
F83
L90
F15
N5
F16
L270
S3
L270
S3
L90
N5
E2
F60
W2
S4
E1
R180
W2
F28
W4
R90
N3
W3
F48
W2
S5
F2
R90
F47
S5
F11
S5
E2
N2
F93
S2
E4
F23
E4
S1
L90
F87
L180
N4
W5
N5
R180
R90
S5
F65
E5
F51
E5
F65
L90
S4
W3
L180
W1
W2
E4
R270
N4
F86
R90
L90
F9
F76
N4
E4
N4
R90
F40
E5
S1
W3
F22
W2
L90
F51
W1
E1
S2
F27
S5
R90
R270
R90
F71
R90
W4
R90
W1
N4
L90
E2
E1
F61
W1
S5
E2
E4
F10
L90
N2
F3
R90
F7
L90
F56
N1
R270
F70
F71
N3
F96
R90
F56
S4
F96
S5
W5
N4
E2
F90
S5
R180
F81
E1
N4
L90
S2
L180
E5
R90
S4
F60
S4
W3
F3
E5
F54")
