(ns adv2022.day23.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.core.match :refer [match]]
   [adv2022.grid :as grid]
   [clojure.math.combinatorics :as combo]))

;; I'm working on this in dec 2023
#_(remove-ns 'adv2022.day23.sol)

#_(add-tap (bound-fn* clojure.pprint/pprint))

(defn parse-into-point-set [l]
  (into #{}
        (for [y (range (count l))
              x (range (count (first l)))
              :when (= \# (get-in l [y x]))]
          [y x])))

(def input (->> (slurp "src/adv2022/day23/input.txt")
                (string/split-lines)
                (mapv vec)
                parse-into-point-set))

(def E [0 1])
(def S [1 0])
(def W [0 -1])
(def N [-1 0])
(def NW (mapv + N W))
(def NE (mapv + N E))
(def SW (mapv + S W))
(def SE (mapv + S E))

(def north-side (with-meta [NW N NE] {:side N}))
(def south-side (with-meta [SW S SE] {:side S}))
(def west-side (with-meta [NW W SW] {:side W}))
(def east-side (with-meta [NE E SE] {:side E}))

(def side-dirs [north-side south-side west-side east-side])

(def dirs? #{E S W N NW NE SW SE})

(defn next-pos [pos dir] (mapv + dir pos))

(defn next-positions [pos dirs]
  (mapv (partial next-pos pos) dirs))

(def next-positions-mem (memoize next-positions))

(defn check-positions [elf-pos-set pos dirs]
  (->> (next-positions-mem pos dirs)
       (keep elf-pos-set)
       not-empty))

(defn stay-still-for-round? [elf-pos-set pos]
  (->> (check-positions elf-pos-set pos dirs?)
       empty?))

(defn propose-move [elf-pos-set elf-pos side-dirs-in-order]
  (when-let [dir
             (some->> side-dirs-in-order
                      (filter #(empty? (check-positions elf-pos-set elf-pos %)))
                      first
                      meta
                      :side)]
    [elf-pos (next-pos elf-pos dir)]))

(defn round [[elf-pos-set side-dirs-in-order]]
  [(->> elf-pos-set
        (filter (complement
                 (partial stay-still-for-round? elf-pos-set)))
        (mapv #(propose-move elf-pos-set % side-dirs-in-order))
        (group-by last)
        (filter #(= 1 (count (second %))))
        (map (comp first second))
        (reduce (fn [acc [cur-pos new-pos]]
                  (-> acc
                      (disj cur-pos)
                      (conj new-pos)))
                elf-pos-set))
   (take 4 (rest (cycle side-dirs-in-order)))])

;; part 1
#_(->> [*input* side-dirs]
       (iterate round)
       rest
       (map first)
       (take 10)
       last
       ((juxt count grid/coord-bounds))
       ((fn [[num-elfs bounds]]
          (- (apply * (map #(inc (abs (apply - %)))
                           bounds))
             num-elfs)))) ;; => 3689

;;part 2

#_(def part2-result
    (->> [*input* side-dirs]
         (iterate round)
         (map first)
         (partition 2 1)
         (take-while #(not= (first %) (second %)))
         count
         inc)) ;; => 965   22 seconds
