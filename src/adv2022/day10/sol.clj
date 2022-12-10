(ns adv2022.day10.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.math :as math]
   [clojure.math.combinatorics :as combo]))

(defn parse [s]
  (->> (string/split-lines s)
       (map #(edn/read-string (str "[" % "]")))))

(def input (parse (slurp "src/adv2022/day10/input.txt")))

(defn x-values [instructions]
  (->> instructions
       (mapcat #(cond-> (list %1)
                  (= (first %1) 'addx) (conj '[noop])))
       (reductions (fn [x [i a]]
                     (cond-> x
                       (= 'addx i) (+ a)))
                   1)))

(defn part1 [instructions]
  (->> (x-values instructions)
       (partition 20 40)
       (mapv last)
       (map * (iterate (partial + 40) 20))
       (reduce +)))

(assert (= 13740 (part1 input)))

(defn part2 [instructions]
  (->> (x-values instructions)
       (map-indexed
        #(if (<= (dec %2) (mod %1 40) (inc %2))
          "#"
          "."))
       (partition 40)
       (map string/join)
       (string/join "\n")
       (println)))

#_(part2 input)

