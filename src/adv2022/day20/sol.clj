(ns adv2022.day20.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.math :as math]
   [clojure.data.priority-map :refer [priority-map] :as pri]))

(def input-test (slurp "src/adv2022/day20/input.test"))
(def input-real (slurp "src/adv2022/day20/input.txt"))
(def parse #(as-> % x (str "[" % "]") (edn/read-string x)))

(def KEYMULT  10000000)

(defn init-pri-map [x]
  (into (priority-map)
        (map-indexed (fn [i v]
                       [[v i] (* (inc i) KEYMULT)]) x)))

(defn previous-pri [pri-map value]
  (first (pri/rsubseq pri-map < value)))

(defn rebalance [pri-map]
  (->> pri-map
       (map-indexed
        (fn [i [v _]] [v (* (inc i) KEYMULT)]))
       (into (priority-map))))

(defn destination-loc [pri-map [num _ :as num-key]]
  (let [num-pri-val (get pri-map num-key)
        pri-map (dissoc pri-map num-key)
        num-places (mod (abs num) (count pri-map))]
    (when (not (zero? num-places))
      (first (if (pos? num)
               (drop num-places
                     (concat (pri/subseq pri-map > num-pri-val)
                             (seq pri-map)))
               (drop (dec num-places)
                     (concat (pri/rsubseq pri-map < num-pri-val)
                             (rseq pri-map))))))))

(defn move [pri-map num-key]
  (if (zero? (first num-key))
    pri-map
    (if-let [[_ a-pri-val :as after-loc]
             (destination-loc pri-map num-key)]
      (let [pri-map (dissoc pri-map num-key)
            [_ b-pri-val] (or (previous-pri pri-map a-pri-val)
                              [nil Long/MAX_VALUE])
            pri-diff (- a-pri-val b-pri-val)]
        (cond-> pri-map
          true (assoc num-key (+ b-pri-val (math/floor-div pri-diff 2)))
          (< pri-diff 10) rebalance))
      pri-map)))

(defn grove-coords [resv]
  (->> (cycle resv)
       (drop-while #(not= 0 %))
       (iterate (partial drop 1000))
       (map first)
       (drop 1)
       (take 3)))

(defn part1 [input]
  (let [pri-map (init-pri-map input)]
    (->> (reduce move pri-map (keys pri-map))
         (map ffirst)
         grove-coords
         (reduce +))))

#_(= 3 (part1 (parse input-test)))

#_(= 4151 (part1 (parse input-real)))

(defn part2 [input]
  (let [input (map #(* 811589153 %) input)
        pri-map (init-pri-map input)]
    (->> (iterate #(reduce move % (keys pri-map))  pri-map)
         (drop 1)
         (take 10)
         last
         (map ffirst)
         grove-coords
         (reduce +))))

#_(= 1623178306 (part2 (parse input-test)))

#_(= 7848878698663
     (do
       (def res (part2 (parse input-real)))
       (prn [:res res])
       res))
