(ns adv2022.day14.sol
  (:require
   [clojure.edn :as edn]
   [clojure.string :as string]
   [clojure.math.combinatorics :as combo]))

(def test-input
  "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

(def real-input (slurp "src/adv2022/day14/input.txt"))

(def SOURCE-POS [0, 500])

(defn parse [s]
  (->> s
       string/split-lines
       (map #(edn/read-string (str "[" % "]")))
       (mapv (comp
              (partial map (comp vec reverse))
              (partial partition 2)
              (partial remove symbol?)))))

(def ROCK \#)
(def SAND \O)

(defn interpolate-points [p1 p2]
  (let [[[y x :as p1'] [y1 x1 :as p2']] (sort (map vec [p1 p2]))]
    (map vector 
     (if (= x x1) (range y (inc y1)) (repeat y))
     (if (= y y1) (range x (inc x1)) (repeat x)))))

(defn draw-line [grid line]
  (reduce
   (fn [grid [from to]]
     (reduce #(assoc %1 (vec %2) ROCK) grid (interpolate-points from to)))
   grid (partition 2 1 line)))

(def draw-lines (partial reduce draw-line {}))

(defn sand-falls [grid start-point limit floor]
  (when-not (or (grid start-point) (zero? limit)) 
    (if-let [new-pos (->> (map (partial map + start-point) [[1 0] [1 -1] [1 1]])
                          (filter (complement #(or (grid %) (= floor (first %)))))
                          first)]
      (sand-falls grid new-pos (dec limit) floor)
      start-point)))

(defn fill-with-sand [grid limit floor]
  (->> (iterate #(when-let [rest-pos (sand-falls % SOURCE-POS limit floor)]
                   (assoc % rest-pos SAND))
                grid)
       (take-while not-empty)))

(defn extents [grid-map]
  (let [pos (conj (keys grid-map) SOURCE-POS)
        ys (sort (map first pos))
        xs (sort (map second pos))]
    [[(first ys) (first xs)] [(last ys) (last xs)]]))

(defn print-grid [grid-map]
  (println "---------------------")
  (let [[[y x] [y1 x1]] (extents grid-map)]
    (doseq [y' (range y (inc y1))]
     (->> (range x (inc x1))
          (mapv (fn [x'] (get grid-map [y' x'] \.)))
          (apply str)
          println))))

(defn part1 [input]
  (let [all-steps (-> (parse input)
                       draw-lines
                       (fill-with-sand 1000 nil))]
    (print-grid (last all-steps))
    (dec (count all-steps))))

#_(= 1078 (part1 real-input))

(defn part2 [input]
  (let [grid (draw-lines (parse input))
        [[_ _] [max-y _]] (extents grid)
        all-steps (fill-with-sand grid 1000 (+ max-y 2))]
    #_(print-grid (last all-steps))
    (dec (count all-steps))))

#_(= 30157 (part2 real-input))





