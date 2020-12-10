(ns advent-of-code-2020.day10
  (:require [clojure.string :as s]))

(defn- parse-joltages
  "Parses a list of joltages, one per line."
  [s]
  (->> s
       (s/split-lines)
       (map #(Integer. %))))

(defn- add-built-in
  "Adds the joltage of the built-in adapter to the given sequence of joltages."
  [joltages]
  (conj joltages (+ 3 (apply max joltages))))

(defn- count-differences
  "Returns a histogram map over the joltage differences when using all adapters
   with the given ratings at once on the charging outlet (rated at 0 jolts).
   Expects joltages to be sorted (ascending)."
  [joltages]
  (second (reduce (fn [[previous distributions] current]
                    (let [diff (- current previous)]
                      [current (update distributions diff inc)]))
                  [0 {1 0 2 0 3 0}]
                  joltages)))

(defn- num-ones-times-num-threes
  "Returns the number of 1-jolt differences multiplied by the number of 3-jolt
  differences."
  [difference-frequencies]
  (* (difference-frequencies 1) (difference-frequencies 3)))

(defn solve-1
  "Returns the number of 1-jolt differences multiplied by the number of 3-jolt
   differences when using all adapters with the given ratings at once on the
   charging outlet."
  [s]
  (-> s
      parse-joltages
      add-built-in
      sort
      count-differences
      num-ones-times-num-threes))

(defn- add-charging-outlet
  "Adds the joltage of the charging outlet to the given sequence of joltages."
  [joltages]
  (conj joltages 0))

(defn- up-to
  "Returns a vector of joltages up to index n."
  [joltages n]
  (subvec joltages 0 (inc n)))

(defn- from
  "Returns a vector of joltages starting at index n."
  [joltages n]
  (subvec joltages n))

(defn- skipping
  "Returns a vector of joltages with index n removed."
  [joltages n]
  (into (subvec joltages 0 n) (subvec joltages (inc n))))

(defn- can-leave-out?
  "Returns true if the adapter at the given index in joltages can be left out."
  [joltages n]
  (<= (- (nth joltages (inc n)) (nth joltages (dec n))) 3))

(defn- number-of-arrangements
  "Counts the number of valid adapter arrangements.
   Expects joltages to be a sorted vector (ascending)."
  [joltages]
  (let [n (count joltages)
        middle (quot n 2)]
    (if (< n 3)
      1N
      (let [num-left (number-of-arrangements (-> joltages (up-to middle)))
            num-right (number-of-arrangements (-> joltages (from middle)))]
        (if (-> joltages (can-leave-out? middle))
          (+ (* num-left num-right)
             (number-of-arrangements (-> joltages (skipping middle))))
          (* num-left num-right))))))

(defn solve-2
  "Counts the number of valid adapter arrangements."
  [s]
  (-> s
      parse-joltages
      add-charging-outlet
      add-built-in
      sort
      vec
      number-of-arrangements))

(def trial-input "16
10
15
5
1
11
7
19
6
12
4")

(def trial-input-2 "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3")

(def real-input "103
131
121
151
118
12
7
2
90
74
160
58
15
83
153
140
166
1
148
33
165
39
100
135
68
77
25
9
54
94
101
55
141
22
97
35
57
117
102
64
109
114
56
51
125
82
154
142
155
45
75
158
120
5
19
61
34
128
106
88
84
137
96
136
27
6
21
89
69
162
112
127
119
161
38
42
134
20
81
48
73
87
26
95
146
113
76
32
70
8
18
67
124
80
93
29
126
147
28
152
145
159")
