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
(def last-go 'nil)
(def map-state (ref nil))


(defn rc->idx
  ([[row col]]
     (rc->idx row col (:cols settings)))
  ([row col]
     (rc->idx row col (:cols settings)))
  ([row col num-cols]
     (+  col (* row num-cols))))

(defn north
  ([[row col]]
     [(mod (- row 1) (:rows settings)) col])
  ([row col]
     [(mod (- row 1) (:rows settings)) col]))

(defn south
  ([[row col]]
     [(mod (+ row 1) (:rows settings)) col])
  ([row col]
     [(mod (+ row 1) (:rows settings)) col]))

(defn west
  ([[row col]]
     [row (mod (- col 1) (:cols settings))])
  ([row col]
     [row (mod (- col 1) (:cols settings))]))

(defn east
  ([[row col]]
     [row (mod (+ col 1) (:cols settings))])
  ([row col]
     [row (mod (+ col 1) (:cols settings))]))

(defn parse-update
  [line]
  (let [split-line (split line #" ")
        str->int (fn [x] (Integer. x))
        lst->ints (fn [lst] (vec (map str->int lst)))]
    (case (count split-line)
          1 (if (= (first split-line) "go")
              (do (def go true)
                  (def last-go (System/currentTimeMillis))))
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

(defn enemy-ants []
  (filter (fn [x] (not= 0 (nth x 2))) ants))

(defn distance
  [[r1 c1] [r2 c2]]
  (let [dr (min (Math/abs (- r1 r2))
                (- (:rows settings) (Math/abs (- r1 r2))))
        dc (min (Math/abs (- c1 c2))
                (- (:cols settings) (Math/abs (- c1 c2))))]
    (Math/sqrt (+ (Math/pow dr 2) (Math/pow dc 2)))))

(defn direction
  [[r1 c1 :as origin] [r2 c2 :as destination]]
  (reduce
   (fn [accum [dir dist]]
     (if (< dist (distance origin destination))
       (conj accum dir)
       accum))
   []
   [["N" (distance (north origin) destination)]
    ["E"  (distance (east  origin) destination)]
    ["S" (distance (south origin) destination)]
    ["W"  (distance (west  origin) destination)]]))

(defn visible?
  ([coords]
     (some
      (fn [[row col owner]]
        (<= (distance [row col] coords) (Math/sqrt (:viewradius2 settings))))
      (my-ants))))

(defn end-turn []
  (def go false)
  (println "go")
  (flush))

(defn clear-map-state []
  (dosync
   (ref-set map-state game-map)))

(defn passable?
  ([[row col]]
     (not= :W (nth @map-state (rc->idx row col))))
  ([row col]
     (not= :W (nth @map-state (rc->idx row col)))))

(defn random-order
  ([[row col owner :as ant]]
     (cond (passable? (north row col)) 
           (issue-order row col "N")
           (passable? (east row col))
           (issue-order row col "E")
           (passable? (south row col))
           (issue-order row col "S")
           (passable? (west row col))
           (issue-order row col "W"))))

(defn issue-order [row col dir]
  (println (str "o " row " " col " " dir))
  (flush))

(defn make-orders []
  (for [ant (my-ants)]
    (random-order ant)))

(defn time-remaining []
  (- (System/currentTimeMillis) last-go))

(defn -main []
  (while (not (:ready settings))
    (init-game))
  (def game-map (make-map (:rows settings) (:cols settings)))
  (while true
    (clear-map-state)
    (while (not go)
      (update-game))
    (println (str "turn is " turn))
    (print-map @map-state)
    (make-orders)
    (end-turn)))
