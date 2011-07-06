(ns ants.test.core
  (:use [ants.core])
  (:use [clojure.test])
  (:import (java.io BufferedReader FileReader)))

;(deftest replace-me ;; FIXME: write
;  (is false "No tests have been written."))


(deftest test-init-game
  (init-game (BufferedReader. (FileReader. "test/resources/game_init.input")))
  (print-map))

(deftest test-update-game
  (update-game (BufferedReader. (FileReader. "test/resources/game_update.input"))))
