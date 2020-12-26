(ns advent-of-code-2020.day25
  (:require [clojure.string :as s]))

(defn- parse-public-keys
  "Parses the two public keys from s."
  [s]
  (->> s
       s/split-lines
       (map #(Long/parseLong %))))

(def HANDSHAKE-START-VALUE 1)

(def HANDSHAKE-SUBJECT-NUMBER 7)

(def MODULUS 20201227)

(defn- transform
  "Transforms the value with the subject number once."
  [subject-number value]
  (mod (* value subject-number) MODULUS))

(defn- transform-subject-number
  "Transforms the subject number with the given loop size."
  [subject-number loop-size]
  (loop [i loop-size
         value 1]
    (if (zero? i)
      value
      (recur (dec i) (transform subject-number value)))))

(defn- brute-force-public-key
  "Performs a brute force attack to determine the loop size of the given public
  key. Returns the loop size."
  [public-key]
  (->> HANDSHAKE-START-VALUE
       (iterate (partial transform HANDSHAKE-SUBJECT-NUMBER))
       (map-indexed vector)
       (drop-while #(not= public-key (get % 1)))
       first
       first))

(defn- encryption-key-from
  "Returns the encryption key, given a secret loop size and the other party's
  public key."
  [loop-size public-key]
  (transform-subject-number public-key loop-size))

(defn solve-1
  "Returns the encryption key the handshake between the two public keys in s is
  trying to establish."
  [s]
  (let [[public-key-1 public-key-2] (parse-public-keys s)]
    (encryption-key-from (brute-force-public-key public-key-1) public-key-2)))

(def trial-input "5764801
17807724")

(def real-input "6929599
2448427")
