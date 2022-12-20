(ns adv2022.day17.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.edn :as edn]
   [clojure.math :as math]
   [clojure.math.combinatorics :as combo]))

(def input-test (slurp "src/adv2022/day17/input.test"))
(def input-real (slurp "src/adv2022/day17/input.txt"))

(def parse (comp
            cycle
            (partial keep {\< [0 -1] \> [0 1]})))

(defn horiz-shape [x y length]
  (map vector (repeat y) (range x (+ x length))))

(defn vert-shape [x y length]
  (map vector (range y (+ y length)) (repeat x)))

(def shapes-map {:horiz (horiz-shape 0 0 4)
                 :vert  (vert-shape 0 0 4)
                 :cross (distinct (concat (horiz-shape 0 1 3)
                                          (vert-shape 1 0 3)))
                 :square  (concat (horiz-shape 0 0 2)
                                (horiz-shape 0 1 2))
                 :L     (distinct (concat (horiz-shape 0 2 3)
                                      (vert-shape 2 0 3)))})

(def shapes-list (mapv shapes-map [:horiz :cross :L :vert :square]) )

(defn translate [shape pos]
  (set (map (partial mapv + pos) shape)))

(defn translate-jet [shape pos pos-set]
  (let [res (translate shape pos)
        xs (sort (map second res))]
    (if-not (and (<= 0 (first xs) (last xs) 6)
                 (empty? (set/intersection pos-set res)))
      shape
      res)))

(defn find-top [position-set]
  (reduce min 1 (map first position-set)))

(defn shape-height [positions]
  (inc (abs (- (find-top positions)
               (reduce max -1 (map first positions))))))

(def floor (into #{} (horiz-shape 0 1 7)))

(defn find-rest [[position-set top jets] shape]
  (let [start-pos [(- top 3 (shape-height shape)) 2]
        start-shape (translate shape start-pos)]
    (reduce
     (fn [[shape [jet-dir & rest-jets]] _]
       (let [pushed-lr (translate-jet shape jet-dir position-set)
             pushed-down (translate pushed-lr [1 0])]
         (if (or (not-empty (set/intersection position-set pushed-down))
                 (not-empty (set/intersection floor pushed-down)))
           (reduced [pushed-lr rest-jets])
           [pushed-down rest-jets])))
     [start-shape jets]
     (range))))

(defn add-shape [[position-set top jets :as state] shape]
  (let [[new-shape jets] (find-rest state shape)]
    [(into position-set new-shape)
     (min top (find-top new-shape))
     jets]))

(defn render-data [position-set]
  (let [top (find-top position-set)
        grid (vec (repeat (inc (abs top))
                          (vec (repeat 7 0))))]
    (->> position-set
         (map (fn [[y x]] [(+ (abs top) y) x]))
         (reduce #(assoc-in %1 %2 1) grid))))

(defn draw-data [data]
  (println "_ start _")
  (->> data
       (map #(map {0 "." 1 "#"} %))
       (mapv #(str "|" (string/join %) "|"))
       (string/join "\n")
       println)
  (println "+-------+\n")
  data)

(defn draw-position-set [position-set]
  (draw-data (render-data position-set))
  position-set)


(defn part1 [input rocks]
  (->> (cycle shapes-list)
       (reductions add-shape [#{} 1 input])
       (drop 1)
       (map first)
       (take rocks)
       last
       draw-position-set
       find-top
       abs
       inc))

#_(= 3068 (part1 (parse input-test) 2022))

#_(= 3100 (part1 (parse input-real)  2022))

(defn find-periodic [start-window limit tops]
  (loop [window-size start-window]
    (let [partitioned (partition window-size tops)
          offset-window (first partitioned)
          potentially-equal (take 4 (rest partitioned))]
      (cond
        (or (= limit window-size)
            (< (count potentially-equal) 2))
        [::failure window-size (count tops) (count potentially-equal)]
        (apply = potentially-equal) [window-size
                                     offset-window
                                     (first potentially-equal)]
        :else (recur (inc window-size))))))

(defn solve-for-height [target-shape-num
                        [cycle-size offset-window repeated-window]]
  (let [remainder (rem target-shape-num cycle-size)
        shapes-at-end (take remainder repeated-window)
        multiplier (dec (/ (- target-shape-num remainder)
                           cycle-size))]
    (+ (reduce + offset-window)
       (* multiplier (reduce + repeated-window))
       (reduce + shapes-at-end))))

(defn part2 [input]
  (->> (cycle shapes-list)
       (reductions add-shape [#{} 1 input])
       (drop 1)
       (map second)
       (partition 2 1)
       (map #(abs (apply - %)))
       (find-periodic 1 2000)
       (solve-for-height 1000000000000)))


#_(= 1514285714288 (part2 (parse input-test)))

#_(= 1540634005751 (part2 (parse input-real)))











