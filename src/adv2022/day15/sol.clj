(ns adv2022.day15.sol
  (:require
   [clojure.edn :as edn]
   [clojure.string :as string]
   [clojure.math.combinatorics :as combo]))

(defn parse-input [s]
  (as-> s x
    (str "[" x "]")
    (string/replace x #":|=" " ")
    (->> x
         edn/read-string
         (filter number?)
         (partition 4)
         (map (comp
               (partial map (comp vec reverse))
               (partial partition 2))))))

(def test-input (parse-input (slurp "src/adv2022/day15/example-input.txt")))
(def real-input (parse-input (slurp "src/adv2022/day15/input.txt")))

(defn man-dist [p1 p2]
  (apply + (mapv abs (map - p1 p2))))

(defn in-range? [[s-pos b-pos :as sensor] pos]
  (<= (man-dist s-pos pos) (man-dist s-pos b-pos)))

(defn sensor-extents-for-row [[[sy sx :as s-pos] b-pos :as sensor] row]
  (when (in-range? sensor [row sx])
    (let [dist (apply man-dist sensor)
          v-diff (abs (- sy row))
          h-diff (- dist v-diff)]
      [(- sx h-diff) (+ sx h-diff)])))

(defn simplify-extents [[begin end :as ext] [begin1 end1 :as ext1]]
  (cond
    (and (< end begin1)
         (= (inc end) begin1)) [begin end1]
    (<= begin begin1 end1 end) [begin end]
    (<= begin1 begin end end1) [begin1 end1]
    (<= begin begin1 end end1) [begin end1]
    (<= begin1 begin end1 end) [begin1 end]))

(defn simplify-all-extents [extents]
  (reduce (fn [accum n]
            (if-let [simpl (simplify-extents (last accum) n)]
              (conj (vec (butlast accum)) simpl)
              (conj accum n)))
          [(first extents)]
          (rest extents)))

(defn beacons-in-extent [[ext-min ext-max :as ext] row beacons]
  (filter #(and (= row (first %)) (<= ext-min (second %) ext-max))
          beacons))

(defn non-beacon-positions-in-row [sensors target-row]
  (let [beacons (map second sensors)
        simplified-extents
        (->> sensors
             (keep #(sensor-extents-for-row % target-row))
             (sort-by first)
             simplify-all-extents)
        pos-count
        (->> simplified-extents
             (map #(inc (abs (apply - %))))
             (apply +))
        beacon-count
        (->> simplified-extents
             (mapcat #(beacons-in-extent % target-row beacons))
             distinct
             count)]
    (- pos-count beacon-count)))

#_(= 26 (non-beacon-positions-in-row test-input 10))

;; part 1
(assert (= 5838453 (non-beacon-positions-in-row real-input 2000000)))

(defn truncate-extents [extents trunc-min trunc-max]
  (keep
   (fn [[min-x max-x]]
     (cond
       (< max-x trunc-min) nil
       (<= min-x trunc-min max-x) [trunc-min max-x]
       
       (< trunc-max min-x) nil
       (<= min-x trunc-max max-x) [min-x trunc-max]
       :else [min-x max-x]))
   extents))

(defn row-extents [sensors target-row max-x]
  (let [simplified-extents
        (->> sensors
             (keep #(sensor-extents-for-row % target-row))
             (sort-by first)
             simplify-all-extents)
        trunc-ext (truncate-extents simplified-extents 0 max-x)]
    (when (> (count trunc-ext) 1)
      trunc-ext)))

(defn part2 [sensors max-xy]
  (let [row-ext
        (->> (shuffle (range 0 (inc max-xy))) ;; random search
             (pmap #(when-let [res (row-extents sensors % max-xy)]
                      (vector res %)))
             (filter not-empty)
             first)]
    (let [[[_ [x-begin _]] y] row-ext
          x (dec x-begin)]
      [x y (+ (* x 4000000) y)])))

#_(time (= 56000011 (last (part2 test-input 20))))

#_(time
   (do
     (def res2 (last (part2 real-input 4000000)))
     (prn res2)
     (assert (= res2 12413999391794))))





