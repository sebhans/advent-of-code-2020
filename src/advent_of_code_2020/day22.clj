(ns advent-of-code-2020.day22
  (:require [clojure.string :as s]))

(defn- parse-decks
  "Parses the two card decks."
  [s]
  (->> s
       (#(s/split % #"\n\n"))
       (map (fn [deck] (->> deck s/split-lines (drop 1) (map #(Long/parseLong %)) vec)))))

(defn- play-round-of-combat
  "Plays a round of Combat with the given decks and returns the resulting decks."
  [[^clojure.lang.IPersistentVector deck1 ^clojure.lang.IPersistentVector deck2]]
  (let [card1 (get deck1 0)
        card2 (get deck2 0)
        rest1 (subvec deck1 1)
        rest2 (subvec deck2 1)]
    (if (> card1 card2)
      [(conj rest1 card1 card2) rest2]
      [rest1 (conj rest2 card2 card1)])))

(defn- play-combat
  "Plays a game of Combat with the given decks and returns the winning player's deck."
  [[deck1 deck2 :as decks]]
  (cond (zero? (.length deck1)) deck2
        (zero? (.length deck2)) deck1
        :else (recur (play-round-of-combat decks))))

(defn- score-deck
  "Returns the score of a combat deck."
  [deck]
  (->> deck
       rseq
       (map-indexed #(* (inc %1) %2))
       (apply +)))

(defn solve-1
  "Plays a game of Combat with the decks in s and returns the winning player's score."
  [s]
  (-> s
      parse-decks
      play-combat
      score-deck))

(declare play-recursive-combat)

(defn- play-round-of-recursive-combat
  "Plays a round of Recursive Combat with the given decks and returns the
  resulting decks."
  [[^clojure.lang.IPersistentVector deck1
    ^clojure.lang.IPersistentVector deck2 :as decks]]
  (let [^Long card1 (get deck1 0)
        ^Long card2 (get deck2 0)
        ^clojure.lang.IPersistentVector rest1 (subvec deck1 1)
        ^clojure.lang.IPersistentVector rest2 (subvec deck2 1)]
    (let [^Long winner (cond (and (>= (.length rest1) card1)
                                  (>= (.length rest2) card2))
                             (get (play-recursive-combat [(subvec rest1 0 card1)
                                                          (subvec rest2 0 card2)]) 0)
                             (> card1 card2)
                             1
                             :else
                             2)]
      (if (= winner 1)
        [(conj rest1 card1 card2) rest2]
        [rest1 (conj rest2 card2 card1)]))))

(defn- play-recursive-combat
  "Plays a game of Recursive Combat with the given decks and returns a pair
  [winner deck] where winner is 1 or 2 and indicates which player has won,
  and deck is their winning deck."
  ([^clojure.lang.IPersistentVector decks]
   (play-recursive-combat decks #{}))
  ([[^clojure.lang.IPersistentVector deck1
     ^clojure.lang.IPersistentVector deck2 :as decks]
    ^clojure.lang.IPersistentSet history]
   (cond (history decks) [1 deck1]
         (zero? (.length deck1)) [2 deck2]
         (zero? (.length deck2)) [1 deck1]
         :else (recur (play-round-of-recursive-combat decks) (conj history decks)))))

(defn solve-2
  "Plays a game of Recursive Combat with the decks in s and returns the winning
  player's score."
  [s]
  (-> s
      parse-decks
      play-recursive-combat
      second
      score-deck))

(def trial-input "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10")

(def real-input "Player 1:
26
16
33
8
5
46
12
47
39
27
50
10
34
20
23
11
43
14
18
1
48
28
31
38
41

Player 2:
45
7
9
4
15
19
49
3
36
25
24
2
21
37
35
44
29
13
32
22
17
30
42
40
6")
