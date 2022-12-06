(ns adv2022.day6.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]))


(def data (seq (slurp "src/adv2022/day6/input.txt")))


(defn end-of-uniq-sequence-marker [size data]
  (->> data
       (partition size 1)
       (take-while #(> size (count (distinct %))))
       count
       (+ size)))

(defn part1 [data]
  (end-of-uniq-sequence-marker 4 data))

#_(= 1155 (part1 data))

(defn part2 [data]
  (end-of-uniq-sequence-marker 14 data))

#_(= 2789 (part2 data))
