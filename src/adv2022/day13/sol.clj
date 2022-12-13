(ns adv2022.day13.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]))

(def real-input (slurp "src/adv2022/day13/input.txt"))

(defn parse [input]
  (->> input
       string/split-lines
       (map edn/read-string)
       (partition 2 3)))

(defn comparer [a b]
  (cond
    (every? integer? [a b])
    (compare a b)
    (every? sequential? [a b])
    (if-let [res (->> (map comparer a b) (filter #(not (zero? %))) first)]
      res
      (comparer (count a) (count b)))
    :else ;; one is a list and one is an integer
    (apply comparer (map #(cond-> % (integer? %) vector) [a b]))))

(defn part1 [pairs]
  (->> pairs
       (map-indexed
        #(vector %1 (apply comparer %2)))
       (filter #(= -1 (second %)))
       (map (comp inc first))
       (apply +)))

#_(= 5350 (part1 (parse real-input)))

(defn part2 [pairs]
  (->> (apply concat [[[2]] [[6]]] pairs)
       (sort comparer)
       (map-indexed vector)
       (filter #(or (= [[2]] (second %))
                    (= [[6]] (second %))))
       (map (comp inc first))
       (apply *)))

#_(= 19570 (part2 (parse real-input)))

