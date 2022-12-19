(ns adv2022.day18.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.edn :as edn]
   [clojure.math :as math]
   [clojure.math.combinatorics :as combo]
   [adv2022.grid :as grid]
   [adv2022.utils :as util]))

(def input-test (slurp "src/adv2022/day18/input.test"))

(def input-real (slurp "src/adv2022/day18/input.txt"))

(defn parse [input]
  (set (map vec (partition 3 (edn/read-string (str "[" input "]"))))))

(def cardinals [[0 1 0]
                [0 -1 0]
                [1 0 0]
                [-1 0 0]
                [0 0 1]
                [0 0 -1]])

(defn neighbors [p]
  (mapv #(mapv + p %) cardinals))

(defn part1 [points]
  (->> points
       (map #(- 6 (count (filter points (neighbors %)))))
       (reduce +)))

#_(= 64 (part1 (parse input-test)))

#_(= 4608 (part1 (parse input-real)))

(defn bfs-flood-filler [bounds points start]
  (let [seen (atom #{})]
    (loop [q (util/queue start)
           accum #{}]
      (if-let [curr-point (peek q)]
        (if (@seen curr-point)
          (recur (pop q) accum)
          (do
            (swap! seen conj curr-point)
            (recur
             (into (pop q) (->> (neighbors curr-point)
                                (remove points)
                                (filter (partial grid/in-bounds? bounds))))
             (conj accum curr-point))))
        accum))))

(defn find-outside [points]
  (let [bounds (mapv (juxt (comp dec first)
                           (comp inc second))
                     (grid/coord-bounds points))
        start-point (mapv first bounds)]
    (bfs-flood-filler bounds points start-point)))

(defn part2 [points]
  (let [points points
        outside-set (find-outside points)]
    (->> points
         (map
          (fn [p]
            (->> (neighbors p)
                 (filter #(outside-set %))
                 count)))
         (reduce +))))

#_(= 58 (part2 (parse input-test)))

#_(= 2652 (time (part2 (parse input-real))))


















