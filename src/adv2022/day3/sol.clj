(ns adv2022.day3.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]))

(defn encode [ch]
  (let [ord (int ch)]
    (cond
      (<= 97 ord) (- ord 96)
      (<= 65 ord) (+ ord -64 26))))

(def data (->> (slurp "src/adv2022/day3/input.txt")
               string/split-lines
               (map (comp (partial map encode)
                          seq))))

(def intersection-score
  (comp first
        #(apply set/intersection (map set %))))

(defn part1 [data]
  (->> data
       (map (comp
             intersection-score
             #(split-at (/ (count %) 2) %)))
       (apply +)))

#_(= (part1 data) 7428)

(defn part2 [data]
  (->> data
       (partition 3)
       (map intersection-score)
       (apply +)))

#_(= 2650 (part2 data))




