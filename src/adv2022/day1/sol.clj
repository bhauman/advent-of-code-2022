(ns adv2022.day1.sol
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def data (->> (line-seq (io/reader (io/resource "adv2022/day1/input.txt")))
               (partition-by string/blank?)
               (remove (comp string/blank? first))
               (map #(map read-string %))))

(defn totals [data]
  (mapv #(apply + %) data))


(defn part1 [data]
  (->> (totals data)
       (apply max)))

#_(= 69912 (part1 data))

(defn part2 [data]
  (->> (totals data)
       (sort >)
       (take 3)
       (apply +)))

#_(= 208180 (part2 data)) 













