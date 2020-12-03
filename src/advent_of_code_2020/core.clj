(ns advent-of-code-2020.core
  (:gen-class))

(import java.io.FileNotFoundException)

(defn- solve-day [n]
  "Solve both puzzles for day n and print the solutions.
   Do nothing if the day namespace does not exist (yet)."
  (let [ns-symbol (symbol (format "advent-of-code-2020.day%d" n))]
    (try (require [ns-symbol])
         (let [day-ns (find-ns ns-symbol)
               solve-1 (ns-resolve day-ns (symbol "solve-1"))
               solve-2 (ns-resolve day-ns (symbol "solve-2"))
               real-input (var-get (ns-resolve day-ns (symbol "real-input")))]
           (println (format "Day %d, 1st puzzle: %s" n (solve-1 real-input)))
           (println (format "Day %d, 2nd puzzle: %s" n (solve-2 real-input))))
         (catch FileNotFoundException e :ignore-day-if-not-found))))

(defn -main
  "Solve all puzzles."
  [& args]
  (->> 24
       range
       (map inc)
       (map solve-day)
       dorun))
