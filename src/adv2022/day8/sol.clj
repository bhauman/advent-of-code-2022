(ns adv2022.day8.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.walk :refer [postwalk postwalk-demo]]
   [clojure.edn :as edn]))

(def grid-data
  (->> (slurp "src/adv2022/day8/input.txt")
       string/split-lines
       (mapv (partial mapv (comp parse-long str)))))

(def height (count grid-data))
(def width  (count (first grid-data)))

(defn addr-right [row-num x] [row-num x])
(defn addr-left [row-num x] [row-num (- width (inc x))])

(defn addr-down [col-num x] [x col-num])
(defn addr-up [col-num x] [(- height (inc x)) col-num])

(defn visible-indexes [row]
  (reduce
   (fn [{:keys [cmax] :as accum} [val idx]]
     (cond-> accum
       (> val cmax)
       (-> (assoc :cmax val)
           (update :indexes conj idx))))
   {:cmax -1
    :indexes []}
   (map vector row (range))))

(defn visible-indexes-dir [addr-fn idx row]
  (->> (visible-indexes row)
       :indexes
       (map (partial addr-fn idx))))

(def part1 
  (->> (map-indexed (fn [idx tree-row]
                      (concat
                       (visible-indexes-dir addr-down idx tree-row)
                       (visible-indexes-dir addr-up idx (reverse tree-row))))
                    (apply map vector grid-data))
       (concat (map-indexed (fn [idx tree-row]
                              (concat
                               (visible-indexes-dir addr-right idx tree-row)
                               (visible-indexes-dir addr-left idx (reverse tree-row))))
                            grid-data))
       (mapcat distinct)
       distinct
       count))

(assert (= part1 1733))

(defn get-4-directions [grid-data [row col]]
  (concat
   ((juxt
    (comp reverse first)
    (comp (partial drop 1) second))
   (split-at col (get grid-data row)))
  ((juxt
    (comp reverse first)
    (comp (partial drop 1) second))
   (split-at row (get (apply mapv vector grid-data) col)))))

(defn take-upto-and-next [pred coll]
  (let [[a b] (split-with pred coll)]
    (concat a (if (not-empty b) [(first b)]))))

(defn visible-tree-score [grid-data addr]
  (let [start-tree (get-in grid-data addr)]
    (->> (get-4-directions grid-data addr)
         (map (partial take-upto-and-next (partial > start-tree)))
         (map count)
         (apply *))))

(def part2 []
  (time
   (reduce max -1
           (for [x (range 99)
                 y (range 99)]
             (visible-tree-score grid-data [x y])))))

#_(= (part2) 284648)






