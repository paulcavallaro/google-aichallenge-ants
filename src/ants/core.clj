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
(def go false)
(def map-state (ref nil))


(defn rc->idx
  ([[row col]]
     (rc->idx row col (:cols settings)))
  ([row col]
     (rc->idx row col (:cols settings)))
  ([row col num-cols]
     (+  col (* row num-cols))))

(defn parse-update
  [line]
  (let [split-line (split line #" ")
        str->int (fn [x] (Integer. x))
        lst->ints (fn [lst] (vec (map str->int lst)))]
    (case (count split-line)
          1 (if (= (first split-line) "go")
              (def go true))
          2 (def turn (Integer. (second split-line)))
          3 (case (first split-line)
                  "f" (let [new-food (lst->ints (rest split-line))]
                        (def food (conj food new-food))
                        (dosync
                         (commute map-state
                                  (fn [x] (assoc x (rc->idx new-food) :F)))))
                  "w" (let [new-water (lst->ints (rest split-line))]
                        (def water (conj water new-water))
                        (dosync
                         (commute map-state
                                  (fn [x] (assoc x (rc->idx new-water) :W))))))
          4 (case (first split-line)
                  "a" (let [new-ant (lst->ints (rest split-line))]
                        (def ants (conj ants new-ant))
                        (dosync
                         (commute map-state
                                  (fn [x] (assoc x (rc->idx (take 2 new-ant))
                                                (keyword (str "A" (last new-ant))))))))
                  "d" (let [new-dead-ant (lst->ints (rest split-line))]
                        (def dead-ants (conj dead-ants new-dead-ant))
                        (dosync
                         (commute map-state
                                  (fn [x] (assoc x (rc->idx (take 2 new-dead-ant))
                                                (keyword (str "D" (last new-dead-ant)))))))))
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
  "Creates a map consisting of all land tiles"
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
  ([g-map]
     (print-map g-map (:rows settings) (:cols settings)))
  ([g-map rows cols]
     (println
      (second (reduce
               (fn [[num repr] nxt]
                 (if (= num cols)
                   (vector 1 (str repr "\n" nxt " "))
                   (vector (+ num 1) (str repr nxt " "))))
               [0 ""]
               g-map)))))

(defn clear-game-state []
  (def food [])
  (def water [])
  (def ants [])
  (def dead-ants [])
  (def go false))

(defn update-game
  [] (parse-update (read-line)))

(defn get-ants [player]
  (filter (fn [x] (= player (nth x 2))) ants))

(defn my-ants []
  (get-ants 0))

(defn end-turn []
  (def go false)
  (println "go")
  (flush))

(defn -main []
  (while (not (:ready settings))
    (init-game))
  (def game-map (make-map (:rows settings) (:cols settings)))
  (dosync
   (ref-set map-state game-map))
  (while true
    (println (str "turn is " turn))
    (print-map @map-state)
    (while (not go)
      (update-game))
    (println (str "turn is " turn))
    (end-turn)))
