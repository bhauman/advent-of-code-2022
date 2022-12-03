(ns adv2022.day1.sol
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def parse-int #(Integer/parseInt %))

(def data (->> (line-seq (io/reader (io/resource "adv2022/day1/input.txt")))
               (partition-by string/blank?)
               (remove (comp string/blank? first))
               (map #(map parse-int %))))

(defn totals [data]
  (mapv #(apply + %) data))

(def part1 (->> (totals data)
               (apply max)))

(def part2 (->> (totals data)
               (sort >)
               (take 3)
               (apply + )))










