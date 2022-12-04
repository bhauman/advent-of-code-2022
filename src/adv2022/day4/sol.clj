(ns adv2022.day4.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]))

(def data (-> (slurp "src/adv2022/day4/input.txt")
              (string/replace "-" ",")
              (read-string)
              (->> (partition 2) (partition 2))))

(defn contains? [[a b] [c d]]
  (<= a c d b))

(defn contained? [a b]
  (or (contains? a b)
      (contains? b a)))

(defn part1 [data]
  (count (filter #(apply contained? %) data)))

#_(= 453 (part2 data))

(defn intersects? [[a b] [c d]]
  (or (<= a c b)
      (<= a d b)
      (<= c a d)
      (<= c b d)))

(defn part2 [data]
  (count (filter #(apply intersects? %) data)))

#_(= 919 (part2 data))
