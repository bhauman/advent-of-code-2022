(ns adv2022.day12.sol
  (:require
   [clojure.string :as string]
   [clojure.math.combinatorics :as combo])
  (:import
   [clojure.lang PersistentQueue]))

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

(defn queue [& args]
  (into PersistentQueue/EMPTY args))

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

(defn bfs [start children-fn end-fn?]
  (loop [q (queue start)]
    (if-let [our-node (peek q)]
      (if (end-fn? our-node)
        our-node
        (recur (into (pop q) (children-fn our-node))))
      ::failure)))

(defn make-children-fn [grid]
  (letfn [(can-traverse? [node n]
            (when-let [v (get-in grid n)]
              (<= -1000 (- v (get-in grid node)) 1)))]
    (let [seen (atom #{})]
      (fn [[node path]]
        (if (@seen node)
          []
          (do
            (swap! seen conj node)
            (into []
                  (comp
                   (filter (partial can-traverse? node))
                   (filter (complement @seen))
                   (map (fn [n] [n ((fnil + 0) path 1)])))
                  (grid-neighbors node))))))))

(defn make-end-fn [end-node] #(-> % first (= end-node)))

(defn part1 [grid]
  (let [start (grid-find-element-pos grid START)
        end (grid-find-element-pos grid END)
        grid (-> grid
                 (assoc-in start 1)
                 (assoc-in end 26))]
    (last (bfs [start]
               (make-children-fn grid)
               (make-end-fn end)))))

#_(= (part1 (parse real-data)) 440)

(defn part2 [grid]
  (let [start (grid-find-element-pos grid START)
        end (grid-find-element-pos grid END)
        grid (-> grid
                 (assoc-in start 1)
                 (assoc-in end 26))
        starts (grid-find-all-element-pos grid 1)]
    (->> starts
         (map #(bfs [%]
                    (make-children-fn grid)
                    (make-end-fn end)))
         (filter (complement keyword?))
         (map last)
         (reduce min))))

#_(= 439 (part2 (parse real-data)))
