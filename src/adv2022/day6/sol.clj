(ns adv2022.day6.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]))


(def data (seq (slurp "src/adv2022/day6/input.txt")))


(defn start-of-uniq-sequence [size data]
  (->> data
       (partition size 1)
       (take-while #(> size (count (distinct %))))
       count
       (+ size)))

(defn part1 [data]
  (start-of-uniq-sequence 4 data))

#_(= 1155 (part1 data))

(defn part2 [data]
  (start-of-uniq-sequence 14 data))

#_(= 2789 (part2 data))
