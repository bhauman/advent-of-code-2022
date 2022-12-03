(ns adv2022.day2.sol
  (:require
   [clojure.java.io :as io]))

(def rock 0)
(def paper 1)
(def scissors 2)

(def first-column {'A rock 'B paper 'C scissors})
(def you-play-column {'X rock 'Y paper 'Z scissors})

(def input-data
  (read-string (slurp (io/resource "adv2022/day2/input.txt"))))

(def part1-data
  (->> input-data 
       (map (merge first-column you-play-column))
       (partition 2)))

(defn compare [g h]
  (let [diff (- g h)]
    (if (= 2 (Math/abs diff)) 
      (/ (- diff) 2)
      diff)))

(defn score [[g h]]
  (+ 1 h
     (cond
       (= 0 (compare h g)) 3 ;; tied
       (< 0 (compare h g)) 6 ;; won
       true 0))) ;; lost

(defn part1 [x] (apply + (map score x)))

#_(= 12645 (part1 part1-data))

;; part 2

(def lose -1)
(def draw 0)
(def win 1)

(def outcome-column {'X lose 'Y draw 'Z win})

(def part2-data
  (->> input-data 
       (map (merge first-column outcome-column))
       (partition 2)))

(defn score-outcome [[g outcome]]
  (score [g (-> g (+ outcome) (mod 3))]))

(defn part2 [x] (apply + (map score-outcome x)))

#_(= 11756 (part2 part2-data))












