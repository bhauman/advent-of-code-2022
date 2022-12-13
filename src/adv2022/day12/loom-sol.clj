(ns adv2022.day12.loom-sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.math.combinatorics :as combo]
   [loom.graph :as gr]
   [loom.alg :as alg]))

(def real-data (slurp "src/adv2022/day12/input.txt"))

(def START :S)
(def END :E)

(defn parse [txt]
  (->> txt
       string/split-lines
       (mapv (partial mapv (fn [c]
                           (condp = c
                             \S START
                             \E END
                             (- (int c) (dec (int \a)))))))))

(defn all-positions [grid]
  (map vec
       (combo/cartesian-product
        (-> grid count range)
        (-> grid first count range))))

(defn grid-find-all-element-pos [grid elem]
  (->> (all-positions grid)
       (filter #(= (get-in grid %) elem))))

(defn grid-find-element-pos [grid elem]
  (->> (grid-find-all-element-pos grid elem)
       first
       vec))

(defn grid-neighbors [node]
  (mapv (partial mapv + node) [[0 -1] [0 1] [-1 0] [1 0]]))

(defn can-traverse? [grid node n]
  (when-let [v (get-in grid n)]
    (<= -1000 (- v (get-in grid node)) 1)))

(defn all-edges [grid]
  (mapcat
   (fn [pos]
     (->> pos
          grid-neighbors
          (filter (partial can-traverse? grid pos))
          (map (partial vector pos))))
   (all-positions grid)))


(defn short-path [grid]
  (let [start (grid-find-element-pos grid START)
        end (grid-find-element-pos grid END)
        norm-grid (-> grid
                      (assoc-in start 1)
                      (assoc-in end 26))]
    (alg/bf-path
     (apply gr/add-edges (gr/digraph) (all-edges norm-grid))
     start end)))

;; part 1
#_(= 440 (-> (short-path (parse real-data)) count dec))

(defn part2 [grid]
  (let [start (grid-find-element-pos grid START)
        end (grid-find-element-pos grid END)
        norm-grid (-> grid
                      (assoc-in start 1)
                      (assoc-in end 26))
        starts (grid-find-all-element-pos grid 1)
        graph (apply gr/add-edges (gr/digraph) (all-edges norm-grid))]
    (->> starts
         (keep #(alg/bf-path graph % end))
         (map (comp dec count))
         sort
         first)))

#_(= 439 (part2 (parse real-data)))






