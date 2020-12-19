(ns advent-of-code-2020.day19
  (:require [clojure.string :as s]))

(defn- parse-rule
  "Parses a single rule into a pair [number text]."
  [s]
  (let [[_ number text] (re-matches #"(\d+):\s*(.*)" s)]
    [number text]))

(defn- parse-rules-and-messages
  "Parses rules and messages."
  [s]
  (let [[rule-text message-text] (s/split s #"\n\n")]
    {:rules (->> rule-text
                 s/split-lines
                 (map parse-rule)
                 (into {}))
     :messages (s/split-lines message-text)}))

(defn- replace-rule-numbers
  "Replaces numbers in rule with their corresponding texts in rules."
  [rule rules]
  (let [matcher (re-matcher #"\d+" rule)
        buffer (StringBuffer.)]
    (loop [number (re-find matcher)]
      (if number
        (do
          (.appendReplacement matcher buffer (str "(?:" (rules number) ")"))
          (recur (re-find matcher)))
        (do
          (.appendTail matcher buffer)
          (.toString buffer))))))

(defn- to-regex
  "Converts a rule to a regular expression, using the given rule set to expand it
  recursively."
  [rule rules]
  (if (re-find #"\d" rule)
    (recur (replace-rule-numbers rule rules) rules)
    (s/replace rule #"[\" ]" "")))

(defn- count-matches
  "Returns the number of messages that completely match the regex."
  [messages regex]
  (->> messages
       (map #(re-matches regex %))
       (filter identity)
       count))

(defn solve-1
  "Returns the number of messages that completely match rule 0."
  [s]
  (let [{rules :rules messages :messages} (parse-rules-and-messages s)]
    (->> rules
         (to-regex "0")
         re-pattern
         (count-matches messages))))

(defn number-of-matches
  "Returns the number of times the regex matches in s."
  [regex-as-string s]
  (let [regex (re-pattern (str regex-as-string "(" regex-as-string ".*|$)"))]
    (loop [s s
           n 0]
      (if-let [match (re-matches regex s)]
        (recur (get match 1) (inc n))
        n))))

(defn solve-2
  "Returns the number of messages that completely match rule 0
  with updated rules 8 and 11.

  Uses the fact that rules 8 and 11 occur only in rule 0 and together
  effectively say that the message must consist of a sequence matching rule 42
  repeatedly, followed by a sequence matching rule 31 repeatedly, and that the
  matches of rule 42 must outnumber the matches of rule 31."
  [s]
  (let [{rules :rules messages :messages} (parse-rules-and-messages s)
        rule-42 (to-regex "42" rules)
        rule-31 (to-regex "31" rules)
        rule-0 (str "(" rule-42 "+)" "(" rule-31 "+)")
        rule-0-regex (re-pattern rule-0)]
    (->> messages
         (map #(re-matches rule-0-regex %))
         (filter identity)
         (map (fn [[_ fortytwos thirtyones]]
                (> (number-of-matches rule-42 fortytwos) (number-of-matches rule-31 thirtyones))))
         (filter identity)
         count)))

(def trial-input "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb")

(def trial-input-2 "42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: \"a\"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: \"b\"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba")

(def real-input "2: 12 16 | 41 26
55: 92 16 | 84 26
107: 48 26 | 29 16
91: 16 86 | 26 120
56: 19 16 | 30 26
33: 69 16 | 127 26
65: 112 16 | 76 26
23: 16 16 | 44 26
102: 16 116 | 26 132
39: 16 26 | 26 26
40: 23 26 | 76 16
108: 16 53 | 26 51
22: 110 26 | 55 16
42: 1 16 | 47 26
14: 112 26 | 46 16
117: 115 26 | 76 16
120: 26 6 | 16 59
72: 26 130 | 16 66
131: 102 26 | 20 16
93: 16 16 | 26 16
58: 97 26 | 104 16
69: 26 88 | 16 46
54: 76 16 | 116 26
1: 26 64 | 16 28
48: 13 26 | 61 16
92: 85 26 | 117 16
49: 124 26 | 98 16
6: 44 44
24: 112 26
17: 112 16 | 116 26
115: 44 16 | 16 26
113: 16 128 | 26 89
106: 26 132 | 16 6
16: \"b\"
67: 44 16 | 26 26
104: 44 88
41: 26 132 | 16 76
38: 16 59 | 26 46
89: 16 24 | 26 62
80: 18 26 | 35 16
98: 46 26
101: 16 132 | 26 46
85: 16 59
126: 16 67 | 26 59
9: 26 49 | 16 80
10: 26 67 | 16 59
34: 26 93 | 16 23
4: 70 26 | 107 16
100: 123 26 | 63 16
109: 118 16 | 54 26
77: 16 50 | 26 99
88: 26 26 | 16 16
81: 67 26
18: 16 115 | 26 88
123: 57 16 | 103 26
60: 26 18 | 16 43
94: 26 23 | 16 59
0: 8 11
57: 46 16 | 125 26
110: 26 58 | 16 60
20: 44 76
15: 56 26 | 33 16
114: 26 132 | 16 23
7: 16 6 | 26 115
28: 16 3 | 26 25
51: 112 16 | 23 26
43: 88 16 | 116 26
111: 26 6 | 16 93
62: 26 132 | 16 112
76: 16 26 | 26 16
27: 96 26 | 45 16
50: 26 93 | 16 76
132: 16 26
35: 16 115 | 26 132
53: 16 46 | 26 116
75: 104 26 | 81 16
82: 26 9 | 16 100
78: 26 116 | 16 125
19: 16 39 | 26 132
37: 26 41 | 16 127
45: 91 16 | 108 26
59: 26 26 | 26 16
116: 26 16
84: 7 16 | 94 26
86: 16 6 | 26 88
63: 94 26 | 17 16
103: 115 26 | 59 16
130: 16 34 | 26 20
99: 39 26 | 6 16
26: \"a\"
64: 16 15 | 26 21
97: 67 16 | 23 26
83: 105 26 | 27 16
21: 75 16 | 52 26
30: 26 125 | 16 112
3: 16 121 | 26 74
105: 26 73 | 16 113
125: 16 16
13: 16 88 | 26 39
32: 72 26 | 122 16
122: 16 77 | 26 2
90: 4 26 | 32 16
12: 26 6 | 16 39
29: 68 26 | 101 16
79: 106 26 | 111 16
61: 132 16 | 46 26
31: 83 26 | 90 16
96: 16 87 | 26 5
118: 26 93 | 16 116
44: 26 | 16
25: 37 16 | 71 26
52: 10 26 | 7 16
124: 125 16 | 6 26
66: 26 36 | 16 14
127: 16 116 | 26 39
68: 26 46 | 16 6
70: 79 26 | 109 16
128: 126 16 | 40 26
8: 42
71: 26 114 | 16 78
73: 129 16 | 131 26
5: 16 65 | 26 118
11: 42 31
119: 16 112 | 26 6
95: 16 88 | 26 59
87: 119 16 | 14 26
121: 35 26 | 95 16
47: 22 26 | 82 16
46: 16 16 | 16 26
129: 12 16 | 17 26
112: 26 26
36: 67 26 | 125 16
74: 16 10 | 26 38

aaaaaababbaaababaaaabbbb
aabbabbabbbbbaaaababbbbbaaabbaabababbaaabababbab
baabaabaaabbaaaaaaabaabb
aabaaababababbbaabababaaaaaaabba
babbaabaaaabbabbabbaabba
aabbabbbaaaaaaabaabaabaa
aaabaabbaaabaabbaaaaababaabaabbabbabaaababaabaabbaabaabbaabaaaabbabababa
abbabaabbbaabbbbaabaababaabbaabbbabbbabbaabbabbaabaabbbbbaaabaababbabbba
abaaabaababaaabbbaaabbbb
baabbabbaaababbbabaabaab
abababbabbabbbaabaaaaaab
bbabbaababbbbbbbbaababba
abaaaaaaabbaaaaabbaabbba
abbbbaabbabbbaabbabbbbbbaaababbaabaaaababaaabbbabaaaaabbbababaabbbaababb
aabaaabbaaabbbaababbbaaaaabbaaababbbababbbabaabbabbaabbbbbabbbbbaaaababbabbabaaa
aaabbbababbbabababbaabba
babbbbabbbaabaabbbbaabba
ababbbbbbbbaaabababbaaaabbaabaaabbbabbab
bbabaaaaabababaababaabaaabababbb
bbabbabbabbaaaabbbaababbbaaaabaabbaaaaaa
baaaabaabbabbabbabbbbbbaabbbbbbbbbaababbbbabbaabaabababb
babbaaaabaababaabaababab
bbbababaababaabbbabaabbb
baaababbbbbbbababbabbabaaaaabbaa
baababaababbabbbabbababaabbaaabababbaaab
bbbaaabbbabbbaaaaaabaabaabbbbaabbaaabbba
aabbabaaaaaabaabababaabbbbbaabbaaababbbb
aaababbbaabaabbababaababbbaaabbabaaaabab
bbbbbbbbabbbaaaabbbbaaaa
abbbbaaaaabaababbbbaabba
abbbbaaaaabbbabbbbbaabbbaaabbabbabbababaababbaaa
bababbabaaabbaaaaabaabaababababa
aabaabbabbabaabaaaabbaaabbbbbbbabbbbbabbabbbbabb
abbaaabaaaaaaaaabaaabbbaabbbaaabbaaaaabaaaabbbbaaaabbaab
aabaaababbaaabbbaabbbaaabaabbbbabbbaaabaabbaababababaaba
aaaaaabaabbaaaababbaabba
aaaaaaaabaaabbaabaabaaabbbababaaabbabbab
bbabbabaaabbababaabababb
aabbbbabbaaabbbabaababaaaaabbbba
baaaaaaabbbababbabbaaabaaabababbaababaaaababaaabaababaaa
bbbaabbbabaaaabaaaabbbaa
baaaabaabbababaabaabaaaa
bbbbbaabbaaaaaaabbaaabaa
aabbbbabbbbaaabbabbbbaab
babaababbabbabbabbababbbaaabbbbb
babbbbabbbaabaaabbbaabaa
abbbbaaaabbaaaaabbabbbaaabbbaababaababbbaaaababbaaabbbbbaabaaaaa
baabbbbabbbbbaabbbaabbba
aabbabbbaaabaaaabbaabbbb
babaabaabaaaabbaabababbb
aabbabbbbabbaababbaaaaaa
bbaababbababbbbbbbbbaaab
bbbbbbabaaabbabbabaababbbbbbbbaa
baabbaaabbabaabbbbbababb
abbbbbbaaabaaabbaababbbb
bababbbabbabaabbbabbaabaaabbbabaababbababbababab
aababbababbbbbaabaababab
babbbaaaaaaaaaabbbabaaba
abbbaaabbaaaabaaababaabbaabaaaaaabbaabbaabaabababbabaaaa
aababaabaabbaaaabababaab
babababbaaaaaabaabababababbbabbbbbababaabaababba
bbbbbabbbbaababaabaabbbbbababababaabbaba
baabbbbabbabaabbbaababbb
abbbbbabbababbbbbbaaabbabababbabaabbaabbabaaabbaaaaabababaabbaababbbabbaaaabaaaa
aababababaaaabbbaaaabbbaaaaabbbb
ababbaabbbaaaabaaaabaaab
abbbbbbaaababaabbabbabbababbbaaabbaababa
bbaabaabbbbaabbbbbaaabbbabbababbbbbbaabb
abbababbabababbbaabaaaaa
abbbaaaaaaaaabaabababbab
bbaababbabababbaababaabbabbbbbabbbabbbbb
babbabbabaabbaabaababbbb
bbbbbaaaaabbabbabbaaaaab
aaababababbbabaababbabab
baababbbaaabbaababaaaabbabbaabbb
aababaabaabaaaabbbbabaabbbbbbbabaabbbbaa
aaaaaaaabbbbaabbbbbababaabbaabbbaaababba
bbbababbbbaabbabbbababbaabababababbaaabaababaaab
baababaaaabbbabbabbbabbababbababbabbaaab
baabbbbaaabbbababbaaaabb
bbababbbabbbbaaaabbabbab
babbabbbabbbabaabaabbbbb
aabaabbabbababaabbbbaaaa
aabbbabbbbababaabaaaaaab
aaabaaaabbbabaabaabbabaabbbabbbb
aababaabbbbbabaaaaaaabba
babaabbabbababbbbaabbbaabbbbbaababbbbabbbbabbbab
abaaabbbbbababbbbabbbbba
abbbbaaaaabbbabaaababbaa
ababbbaaabbbabbbbabbaabb
aaaaaaabaaaaabaababbbbabbabbbbababaabbaa
aaaabbabbaaababbbbbabababbabaabaaabbbabbbbbbbabbaaabbbbbbabbabab
bbbaaabbbbbbaabbaaabbbba
bbbaaababbabbabbbaabaabb
abbbbbbbaababbbbbbbbabbaabaabbaababbbaba
aaaaaabbbbbbbababbaababbbaabbbba
baaaabaaabbababaabaabaaaababbbbbbababaaabbbbbabb
bbabbbaabbbaabbbbabaaabbbabaaabaabbbbbbbaaaaabba
aaaaaabaabababbabbbbabba
ababbaababababbaaaaabbba
aabaababbabbbbabbbbaabbbabaaaaaaaabaaababaabbbbb
baabbabbbbaaabbabababbaaaaaababb
aaabbaabababbabaababaaab
abbaaaabaaaaaabbaaabbbaa
ababbababbabbabababbaaaababbabbabbbabaabbababaabbbabbbbaabbbbabbabbabbbb
baaaaaaabbababbababaabbabaaaabbaaabababaabaaaaabaabbbbaa
aabbaaaabbaaababbbaaabbaabbbababababaaaa
aaaaabbbaaabababbaabbaabbaaaaaba
bbababaabaaabaabbbbbabbb
aababaabaaaaabbbabaaabaaabababbb
bbbabababbaaaabbabaaabbabbabaaba
bababbaaabaaaaaababbaabb
aaaaaabbaaabaaaabaababab
baaabaabbabaababaabbbbabaabaabbbbbbabbbabbaabbaa
aaaaabaaaaabbabbbbbbbabb
babbabaaaaabaaaaabbbbabb
bbbbbaababbbbbbabbbaaabbabbbabbbbabbaabbbababbbb
aaabababbbabbabbaaaabaababbaabbbaabababb
bbaaababaaaaabaabaabbbbabbabbabbbabbbbbabaaabbba
aabaaabbababbbbbbbbbaaaa
aababbabbbaabaaabbabbaabaabbababbabbbbbb
bababaaabbbbbaabbbbabaabbaabaaba
aaaabaaaaaaaaababbaaaaabbbbababbbabbbaab
aabaaaabbaaabaabaababbaa
aabaababbbbabaabbaaabbba
bbbbababbbabaaabbabbaabbababaaabbbbbabba
aabbaaaaabbaaaababbbaabb
aaabaaaaaaaaaaaabaaababaabbbabbbaababaabbaabaabbbbaabbabbbbabbaa
abaaaaaaabbbabbbababbbaabaabaabb
abbaaaaaaabbaaaababbbaab
bbabaabbaaaaaabbabaabbba
bbabaabbabbbabaababbbbba
baabbbbaaaabaabaaabaabaa
bbabbbbbbbaabbbaaabbaabbbbaabbabbabbbabbbbabbbab
aaabababbbabbaababababababbbbabaabababaaaaaaabba
aabbbaaaaababbabbbaaabbabbaaaabaabbbbaaaabbaabba
ababaabbaabbbabaabaaaabbbbabbbbbbaaabbbb
baababbbabbbabbbaababaaa
bbbbbaaaabababababbabaab
bbbbbaabbbabbaabbaaaaaba
abaaabbbbabaababbabaaababbaababa
aabbbbabbabaaabbbbababaaabaaaabbbabaabaa
babbabbbababbbbbbaabbbbaabbabbab
abababababbaaaaaaaabbbba
aabbbabbbbabaaabbabbbbabaabaaaabbbbabaaaaaabbbba
aabbbabbbbaababbbbbbbaab
bbaaaababbbababaaaabbbbaabbbaaabbababbbb
abbabbaabaaaabababaabababaaabbaaababbbbaababbabbaaabbabaababbaab
aaabababaaaabaabaaaaaaabbbababbababbabbababbaaab
babaabbabaabbabbbbbaabaa
babbabbbbbaabaaabbbbbabaabbbabbabbaaaabbabbbabaabbbaaaaa
bbbababaaabaaabbabababaabaaababbbabaaaaaabbaabba
baabbabbababbbaaaaaabbaa
aabaabababbbbbaabaababba
ababbbaaababbbaaaaabbbaa
abbabbbabbaababbaababbbb
aababaababaabaaabbabaaababaabababbabbbab
aaaabaaaaabbbbabaabbbabbbbaababbbbbbaaab
bbbbababbbbbababaaaabbaa
abbbaaaabbbaaabaabbbaaab
bbabaaabbbbbabaababbaaaabbbaaabbbaaaaabaabbbabbbbbabbbbbbaaaaabbaaabbbabbbbaabababaaabaa
aaabbaabaabbababaaaaabba
bbabaaaaabbbaabaaabbabaaaabbabbbaaabaabbbbaabbab
bbaabaaaaabbbaaababaabaaabbabaab
bbbbababababaabbbaaababa
baabaaaaabbababaaababaabaabbbabaabbabababbababbaabaaaabaabbbbaaaabaabbbbbaabbbbb
bbababaaababababbababaaabbbabbbaaaaababa
bbaabaaaabbbbbbbaaaabbbb
aabaabbaababbbbbbaaababb
abbbaaaaaabbabbababaaabaabbabaabababaaaabababbbb
abbbbbaaaaaabbabbbabaaabbaaaaaaabbbabbab
bbabbabbbbabbabbaabbaabb
aabbabbababbaababbbabbba
abababbabaababaababbbbaababbabab
bbababbabbaabbbbbbbaaaaabababaabaaaaabba
abaaaabbbaabbaabbaabaabb
ababbaababbabbbaabaababa
abaaaabaaabbbabbbaabbaaaababaaaabbbabbbabbaaaaaa
aabbababbbabbbaaabaabbba
abbabbbaaaaaabbbaabbabaabbaaabbbababbabbabaaabba
aabbbabaaaabbbabbabbbbbabbabaabbabaababa
babaabbaabaabaaababbabbaababababaabababa
abbaaaaaaaabbbabbbaaabbbbaaaaaba
bbaaabbbbbaaabbabababbab
aabbababbbbaabbbaababbba
abaaabbaaabaabbbbaaaabab
babbaabaaabbbbabbaaabbaabbaaaabb
bbabbabbababbbaabbaababa
babbbababbabbbbabbbaabbbbbaaaabaabbbababaabbabbabbabbaaababbbbaaaabaaaabbbaabaabaaabbabb
bbabbabbbbbbbbabbabbbbbb
bbbaaabbabaaabaaabbababbbabababbbbabaaabaaaababb
babaaababbaababbababbababbbabaabbbbbbabbabbaaababbabaabaaababbaa
aabbbbabbaaaabbaaabaabbabaabbabbabbbaaabaaaababa
abbaaaaaabbababbbbbbbabb
baabbbaaabbaababbbbabbaa
ababbabababbbaabbbaabbbb
abababbabaabbbabbaababaaababaabbbbbabbaabbaabbababbabbababbbbabbaabbbaaaaabbaababaabbaba
aabaababababbbaabbaabbab
babaabbabbbbaabbaababbaa
baabbaabbbbbbababaabbaaaabababaabaabbbabbbbbabaa
abbabababbaabaabbbaababa
baababbbaabaaabaabaaabbbaabababa
bbabaabbbbbbbaabbabaabbb
babaabbaabaabbbaaabaabbbbabbabaababbabbb
babaababbbaababbbbaabaaaabbbbabaabbabbbbaaaabbba
ababaabaababbbbaaaaabbba
aabbbabbaabbbbabaabbabbbabbbaababaaaaaaaabbabbbb
aaabaaaaaabaabbaaabaaaaa
abbababbabbbaababbbaabba
abababbababbbbabbaaaaaab
aaaabaabaaabbbabbbaabbab
abbbbbbbbaababbbbabbabab
ababaabbbbbbbbababbbbaab
aaaaaaabaabbabababaaaaaaaaabababbabbbbaa
bbaabaaaaabbbabbbaaabbba
baabbaaababbabaabbbbaaba
bbbaabbbabbabbbabaaaaaab
aaaaababbbbabbaababbabababaabbab
aaabbbababbbabbaabaabbbb
abababaaabababababbaabba
ababbbaaabbaabaaabbabbbb
bbabaabbbaabbbbabaaabaaa
ababbaabaaaabaabbbbbbbab
baabaaabbbaabaaaaabaaaabbaabbbabbbbabbbbaabababb
abbaabbbabaababaabaabaaaabbbaaaababbaabbbabaabaa
aababbabbababaaaababbbba
bbabbabaabaaaaaaabaaaaaabbaababbaaaabbaababbbbaabbabbbbaaabbabaa
aaaaaaabbbaababbababbbaabbabbabaaababbaabbabbbbbaabababa
aaabaaaabaaabaababaabbab
baaaaaaabaabaaababaaaabaaabbaabb
bbaabaabaabaaaabbbbbaaab
abaabbbaaaabaaaaabbbababaaabaaaaaaaabaaaababbbabbbababbaaabbabbb
abaaabbbabbbbbabbabaaaaa
ababbababbbabaababbbbabb
babaababaaaaaaaaaaabbbabaaaababa
aabaaabbaaaaabbbaaabbaba
baaabbaaaabaaababaababaaaabaaabaaaabaaaaabbbaaba
aaaaabbbbabbaabaaaabbaabbbbbbabb
aabbbbababbbaaaabbaaaaab
bbbabaabaabaaaabbabaaaaa
abbbbaaaabbbaabaabaabbbb
abbaaaaababbaaaaabbbabbabaaaaaba
abbbaabaabbbaababbababab
bbbaaabbbaabaababaaabbab
aabaabababbbaaaabbbbaaba
baababbbbaababbbbabbbbabbbbababbabbbbaab
bbbbababaababaabbbbbbaaabbaaaabaabbbbaaabbbabbba
baaabbaabaabbbabaaaaabaaaabbaabb
aabaaababbbabababbbabbbb
aabbaababaabbbbabaaaaaba
aaaabbababbaababbabbbabb
ababababaaaabbababaaabab
aaabaabababbbbababbabbbbaabbaaabaabaaabbbbbababbabbabbabaaabbbba
aabaaabbaabaaabbbbbaaabbbbbabaaa
bbbaabbbbabaaababbbbbbabababbbbbaaabbabbabbabbaa
abbaaaabbaabaababbbaaaab
baaaabaaabaaaabbbbabbbab
bbabbababbbbbbbbbabaabbb
babaaabaaabbabbabbababab
bbabbabaabbbababbbbabbba
abaaaabababbbaaabbabbabaaabaaabb
aaaabbabaaaaaaabbabbbaba
abaaaaaabaabbaaababbbbabaabbabbabbbaaaaaaaabbbba
ababbabaabbbbbaaaabaabaa
abbbaababaaababbaabaaaaa
bbbabaabbbaabbbbbaaabaaa
bbabbabbbaaabbaabaaabbba
babbabaabbabbaababaabbba
abbababaababababaabbbbaa
baabbbabaaaaabbbbabbbbbb
aabbabaaaabbabbaabbbaaab
abbbbbaaaabaaaabbbbabbbb
bbbaabbbbbbbabaaababaaaa
ababbaabbbaababbbbaaababbaaabaabbbaababbbbbbbbaa
bbbabbababbbbbaaaabbbabbbbbaabbabbaaaaabbaabbaaaabbbbabaaababaaa
aababaaababbbaababaabbbaaababbbabbbbbbaa
babbabbbaaaabbabaaabaaab
aabbbaaaaaabaabaababaaab
babaaabaaabbbaaaabaabbba
aabaaaababbbabbaabaababa
bbbbbbaaabbabbaaabaabaabaaabbabaaaababbbabbbabbb
aababaababbaaaabbabbaabb
abbbaaaabaaabbaabbabababbaabaabb
aaaaaabbabbaaaaababababa
bbbbabaabbbaaabaaababbbb
babaabababbbabbabbbabaaa
bbbaaabbbaabbbbaaaabbbaa
abbabbbababbbaaabaabbabbababbabbababbbbaaaabbabababbbbabaabbaaba
abababbabbbaaababaabbaba
aabbababaaabbbbbbbaaaabbbbbbbbababbbabaabbabbbbb
abaabaaabbababbbbaaaabababaabbaaaaaabbbbbbbbaaaabbbbbbaabbaababa
aabbabbaababbbbbbbbbaaaa
aaaaaabbaabbababbbaaaaaa
bbbaaababbbbbbabaaabbaabbbbaabba
bbabbabbabbbabbabbabbbab
bbaabaaabbaabbbbabbbaabb
babbbaaaaababaabbbbbbabb
abaaabaaabaaaabaaabbbbbb
bbababaaabbbabbaabbbabbbbabbbbbaababaaba
abababaabbabaabbbbaaabbabaabbbababbaabbb
babbaabaabbbabbaabaabbbb
baabbbbabbababbabaababba
bbaaabbbaabbaaaabaaababbbabbabaabaabbbbb
aaaabbabbbbbbbbbaaaabbabbaaaabbb
bbbbabaaabbaababbbaabbab
baaaabbaabaaaaaababbabaabaaababa
aabaabbabbaaaababababbaababbabab
abaaaaaabaaababbabbaaabb
abbbabbabbbbabaabbbbaaba
babababbbbabbbaaaabbbabbaaaaaabbbabbabbbaabaaaabababaaaa
aaaabaababbbbbbaabaabbaa
ababbbaaababababababbaaa
ababbbaabababaaabbbbbabb
abbababbaabaaabbbbbaabab
babaababbababbbabbbbabbb
abbbbabbabbbaabbabaaaaabbaaabbbbbabbaaababaababa
baabbbbaaaabbabbbbbababb
aaaaabaaabbabbbababbbbaabaaabbaabbbabbba
baabaaabaabbbaaaabaaabba
aabaabbabbbbbababaababbb
aabbbabaabaaaaaaabaabbaa
abaabababaabaababbbbbbaabbabaabbaabaabbb
aabaaabbbbabbabaabaaaababbaababaaabbbaab
abaaaaaabababbbaabbbaabb
bbabaabbaaabaaaababbbabaabbaabba
bbbbabababbbabababaababb
babbaaabababbabaabbabababaaababbbbabbbbbbbabbabbbaaaabaaabbbbbba
abbbbbbbbbbbabababbabbaaabbaaaababbbbabaaabbaaaaaababbbbbbaababbbbaabaabababaabbababaabbbbbbbbba
bbbaaababaaaabbabaaababbababbaabbbbbbbba
babbabbaaaabbbabababbabb
ababaabbabbabbbabbabbbba
aaabbaabababbbaaababaaab
ababbbabaababbaaababbaaa
baaaabaaaabbaaaaabaababa
baaababbababababbbabbaaa
ababbabababaabababaabaabaaaabbbbbbaaaaababaabbaabbaaaabb
abbbbbaaaaaaabbbabbabaab
aaaaaaabbbaaababbbbaaababaababaaababbaaa
aaabbbabbabaabaaaabbbaab
abbbabbabbababaaaaabbbbb
abbbbabaaaaabbbaabbabaab
bbabaabbaabbbaaabaabbbbb
ababbaababbabbbabbabbaabbbaaaaabbbbbbbba
aabaaabababbbbaabaabaabb
aabbbaaabaabaaabbbbabbaa
babbbaaababbaabaabbbabbbababbaabababaaba
aababaabbabbabbababbbbabaabbbbabbbbbaaaaaabaabaa
aabaaaabbbaabbbabaabaaababaaabababbbabbbabbaaababbaabaababbabbbbabbabaaa
abbbabbbbabbbbaaaabbbabbaabaaabaabaababbbbbabaaa
babababbaababbababbabaaa
aaaaabbbabbbabbabbbaabba
bbaabaaaabaabbabababaaab
babbabaaaaaaaababaaababbbaabbbaaabaabaab
abbbbbbbabbaabaabbbbbbaa
aabaaaaabaaaaaabaaaaabbaaabababa
abababbabbabbabbaabaabbb
aabbbaaaabababbaabbaaaabbbababab
baababaababbabaaaabaaabaaabbbababbbbabbabbabbaaababbbbba
bbababaabaaaabbaababbababbabaaaaabbabbab
abbbbbbbabbbbbaaaaaabbba
bababbbabbbaaabaaabbbabbabaaabaaaabbabbbababbbbbababbbab
abababaaaaaaaababbbaabaa
babbabbbaabbabbbaababaaa
abbaabaababaaaabbbbbabbaaaabbabbbbbababaabbbbbabbaaaaabbbbaaaabaaabbaaab
aaababababbaaaabbaaaabbb
aaaaaabaaaaabaabaaaabbaa
bbaaabbbabbbbbbaaabbaaaaababbabbbaaabbba
bbbaabbbbbabbbaaabbbbbab
aabbaabaabbbababbaaabbbb
bbabbbabbbaabbaaabbbbbbababaaabaabbababbaabbaabbbaabaaaabaababbb
aaaabaaaabbbabaababbbbaabaaaaaba
abbbabbaabaaabbbabbababaaaaabbabbaabbaaaababbbab
baaabaabaabbaaaabbababbbabbaaaababbbbbab
bbbbbbababaaabaabbaaaaaa
baaabbaaaabaaabaaaabaaab
bbbaaabaaabbabbaabbabaab
babaabababbbbbbaaaaabbba
aaabbbabaaaaaaaabababbab
bbbbababaabbabbababbbbbb
aaaaabaaaaaaaababaaaaabb
aaaaaaaababbbbababbbabbaabababbb
abbbbbbaaababaababbbbaab
aaaaaabababaababaaaaabba
aabbaaaabababbabaabaababbbaabbbbabbbaaaaaabbabaaabaabbbb
ababbbbaabbabbbabbbbbaaababababbaababaaabaaaaaab
bbabbabbbabaaabaaabbbbaa
baaaabbaaabbbabaabaabaaabbabbababbbbbbbbaaabaaaababbbbbabaaabbabbabbbbbaaaababaa
abbababaaaabababbbabbaaa
babaabbabbbaabbbaaaabbaa
abbbbbaaaabaabbaaabbabbaabbabaaaabbabaaa
abababbababaaabaabbababaaaaabbababaaaaaaabababbaabaabbbabbaaaabb
bbbaabbbababaabbabaaaaab
aabaabaaabbabaabbbaaabaa
abababaabaabbaababbabbbb
babaabbaaaaaaaabaaaaabba
bbaabbbaababaaaaaaaababa
bbabaaaaabaaaaaaaaabaaaabbbabababaabbbababaaabba
abaaabaaabaaaabbbaaaabab
aabbabaaaaaaabbbababbbba
abababaabbbbaabbaabbaaabbbbbaaaabababababababaaaabbbbaaaaaaaabbbbabbbaaa
abbbabbabababbaaabaabbaa
aaabaaaaaabbaaaabbaabbba
aabaabbababbabbaaabbabbbababbaab
bbbbbaabaaabaaabaabbaabbbbaababa")
