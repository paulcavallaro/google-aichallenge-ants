(ns ants.core
  (:import (java.io BufferedReader FileReader))
  (:use [clojure.string :only [split]])
  (:use [clojure.pprint :only [pprint]]))

(def 
  ^{:doc
  "The game-map is a vector of row vectors each
  containing one entry per a column"}
  game-map nil)

(def settings {:ready false})
(def food [])
(def water [])
(def ants [])
(def dead-ants [])
(def turn 0)

(defn parse-update
  [line]
  (let [split-line (split line #" ")
        str->int (fn [x] (Integer. x))
        lst->ints (fn [lst1 lst2] (conj lst1
                                  (vec
                                   (map str->int
                                        (rest lst2)))))]
    (case (count split-line)
          2 (def turn (Integer. (second split-line)))
          3 (case (first split-line)
                  "f" (def food
                        (lst->ints food split-line))
                  "w" (def water
                        (lst->ints water split-line)))
          4 (case (first split-line)
                  "a" (def ants
                        (lst->ints ants split-line))
                  "d" (def dead-ants
                        (lst->ints dead-ants split-line)))
          nil)))

(defn parse-settings
  "Parse a line that is either of the form:
    variable-name integer-value
  OR
    ready

  It will create a new entry in the dict passed in
  of the form:
    {:variable-name integer-value}
  OR
    {:ready true}"
  [dict line]
  (let [split-line (split line #" ")]
    (if (> (count split-line) 1)
      (assoc dict (keyword (first split-line))
             (Integer. (second split-line)))
      (assoc dict (keyword (first split-line))
             true))))

(defn make-map
  "Creates a map consisting of a vector of row vectors
  containing an entry for each col"
  [rows cols]
  (vec (repeat rows (vec (repeat cols :L)))))

(defn make-long-map
  "Creates a map consisting of land tiles"
  [rows cols]
  (vec (repeat (* rows cols) :L)))

(defn init-game
  "Initiliazes the game state based on the bot input
  read from standard input"
  []
  (def settings (parse-settings settings (read-line))))
  

(defn lookup-map
  "Look up what's at the given position in the map"
  [row col]
  (nth game-map (+ (* row (:rows settings)) col)))

(defn update-map
  [row col val]
  (assoc game-map (+ (* row (:rows settings)) col) val))

(defn print-map
  ([]
     (print-map game-map (:rows settings) (:cols settings)))
  ([map rows cols]
     (println
      (second (reduce
               (fn [[num repr] nxt]
                 (if (= num cols)
                   (vector 1 (str repr "\n" nxt " "))
                   (vector (+ num 1) (str repr nxt " "))))
               [0 ""]
               game-map)))))

(defn clear-game-state []
  (def food [])
  (def water [])
  (def ants [])
  (def dead-ants []))

(defn update-game
  ([] (update-game (java.io.BufferedReader. *in*)))
  ([rdr]
     (clear-game-state)
     (doseq [line (line-seq rdr)]
       (parse-update line))))

(defn get-ants [player]
  (filter (fn [x] (= player (nth x 2))) ants))

(defn my-ants []
  (get-ants 0))

(defn -main []
  (while (not (:ready settings))
    (init-game))
  (def game-map (make-long-map (:rows settings) (:cols settings)))
  (print-map))
