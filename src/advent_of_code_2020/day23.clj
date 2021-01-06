(ns advent-of-code-2020.day23
  (:require [clojure.string :as s]))

(defn- parse-cups
  "Parses the cups into a vector of numbers."
  [s]
  (vec (map #(- (long %) (long \0)) s)))

(defmacro take-if-not-in
  "Returns the value of expr if it is not one of the following expressions or the
  alternative expression otherwise."
  [expr & exprs]
  (let [n (gensym "n")]
    `(let [~n ~expr]
       (if (and ~@(for [e (butlast exprs)]
                    `(not= ~n ~e)))
         ~n
         ~(last exprs)))))

(defmacro find-destination-cup
  "Returns the label of the destination cup (the one below current that is not one
  of the picked cups)."
  [current cups picked-1 picked-2 picked-3]
  `(let [n# (alength ~cups)]
     (take-if-not-in (mod (- ~current 1) n#) ~picked-1 ~picked-2 ~picked-3
                     (take-if-not-in (mod (- ~current 2) n#) ~picked-1 ~picked-2 ~picked-3
                                     (take-if-not-in (mod (- ~current 3) n#) ~picked-1 ~picked-2 ~picked-3
                                                     (mod (- ~current 4) n#))))))

(defn- perform-move-indexed
  "Performs one move on the given starting state and returns the resulting state."
  [^clojure.lang.PersistentVector state]
  (let [^longs cups (get state 0)
        current-cup (long (get state 1))
        picked-cup-1 (aget cups current-cup)
        picked-cup-2 (aget cups picked-cup-1)
        picked-cup-3 (aget cups picked-cup-2)
        next-cup (aget cups picked-cup-3)
        destination-cup (find-destination-cup current-cup cups picked-cup-1 picked-cup-2 picked-cup-3)
        after-destination-cup (aget cups destination-cup)]
    (aset-long cups current-cup next-cup)
    (aset-long cups destination-cup picked-cup-1)
    (aset-long cups picked-cup-3 after-destination-cup)
    [cups next-cup]))

(defn- play-cup-game-indexed
  "Performs the given number of moves on the given starting state and returns
  the resulting state as a sequence beginning with cup 1."
  [n number-of-cups cups]
  (letfn [(encode-cups [cups]
            (if (> number-of-cups (.length cups))
              (let [cup-array (first (reduce (fn [[cup-array prev] cur]
                                               (aset-long cup-array prev cur)
                                               [cup-array cur])
                                             [(long-array (conj (vec (range 1 number-of-cups)) 0)) (dec number-of-cups)]
                                             (map dec cups)))]
                (aset-long cup-array (dec (last cups)) (count cups))
                cup-array)
              (first (reduce (fn [[cup-array prev] cur]
                               (aset-long cup-array prev cur)
                               [cup-array cur])
                             [(long-array (range 0 number-of-cups)) (dec (last cups))]
                             (map dec cups)))))
          (decode-cups [^longs cup-array]
            (vec (take (alength cup-array) (iterate #(inc (aget cup-array (dec %))) 1))))]
    (loop [n (long n)
           state [(encode-cups cups) (long (dec (get cups 0)))]]
      (if (zero? n)
        (decode-cups (get state 0))
        (recur (dec n) (perform-move-indexed state))))))

(defn- cups-after-cup-1
  "Returns the labels on the cups after cup 1 as a string."
  [cups]
  (s/join (subvec cups 1)))

(defn solve-1
  "Returns the labels on the cups after cup 1 after 100 moves."
  [s]
  (->> s
       parse-cups
       (play-cup-game-indexed 100 9)
       cups-after-cup-1))

(defn solve-2
  "Returns the product of the two cups immediately clockwise of cup 1 after ten
  million moves with a million cups."
  [s]
  (->> s
       parse-cups
       (play-cup-game-indexed 10000000 1000000)
       (#(* (get % 1) (get % 2)))))

(def trial-input "389125467")

(def real-input "586439172")
