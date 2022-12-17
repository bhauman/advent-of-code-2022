(ns adv2022.day16.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.edn :as edn]
   [clojure.math.combinatorics :as combo])
  (:import
   [clojure.lang PersistentQueue]
   [java.awt Toolkit]))

(defn beep []
  (.beep (Toolkit/getDefaultToolkit)))

(defn parse [s]
  (-> s
      (string/replace #"=|;" " ")
      string/split-lines
      (->>
       (map #(edn/read-string (str "[" % "]")))
       (map (comp vec (partial filter #(or (number? %)
                                 (and (symbol? %)
                                      (= 2 (count (str %)))))))))))

(def test-input (parse (slurp "src/adv2022/day16/input.test")))
(def real-input (parse (slurp "src/adv2022/day16/input.txt")))

(defn make-graph [input]
  (reduce (fn [accum [name flow _ & children]]
            (update accum name
                    assoc
                    :flow flow
                    :children (vec children)))
          {}
          input))

(defn queue [& args]
  (into PersistentQueue/EMPTY args))

;; functions for simple traversal
(defn simple-make-children-fn [graph]
  (let [seen (atom #{})]
    (fn [[node path]]
      (if (@seen node)
        []
        (let [{:keys [children]} (get graph node)]
          (swap! seen conj node)
          (into []
                (comp
                 (filter (complement @seen))
                 (map (fn [n] [n (cons node path)])))
                children))))))

(defn simple-make-end-fn [node]
  #(= node (first %)))

(defn bfs [start children-fn end-fn?]
  (loop [q (queue start)]
    (if-let [our-node (peek q)]
      (if (end-fn? our-node)
        our-node
        (recur (into (pop q) (children-fn our-node))))
      ::failure)))

(defn nodes-with-flow [graph]
  (->> (sort-by (comp :flow val) > graph)
       (filter #(< 0 (-> % val :flow)))))

(defn path-from-to [graph start end]
  (flatten
   (bfs [start '()]
       (simple-make-children-fn graph)
       (simple-make-end-fn end))))

(defn get-shortest-paths-to-destinations [graph start destinations]
  (map (partial path-from-to graph start) destinations))

(defn all-distances [graph start]
  (let [destinations (set (cons start
                                (map first (nodes-with-flow graph))))]
    (reduce (fn [accum start]
              (reduce
               (fn [accum path]
                 (assoc-in accum [(last path) (first path)] path))
               accum (get-shortest-paths-to-destinations
                      graph
                      start
                      (disj destinations start))))
            {} (sort destinations))))

(defn cost [graph dest path]
  (- (* 2 (count path))
     (get-in graph [dest :flow])))

(defn traversal-costs [graph routes]
  (->> routes
       (map (fn [[k v]]
              [k (into {}
                       (map (juxt first
                                  (partial apply cost graph)))
                       v)]))
       (into {})))

(defn path-step-length [routes path]
  (->> (partition 2 1 path)
       (map #(count (get-in routes %)))
       (apply +)))

(def ^:dynamic *minutes* 30)
(def ^:dynamic *take-opt* 4)

(defn tps-make-children-fn [routes costs]
  (fn [path node]
    (let [length (path-step-length routes path)
          children
          (->> (remove (into #{node} path) (keys (get routes node)))
               (filter #(<= (+ length (count (get-in routes [node %]))) *minutes*)))]
      (if *take-opt*
        (->> children
             (map (fn [next-child] [node
                                    next-child
                                    (get-in costs [node next-child])]))
             
             (sort-by last)
             (take *take-opt*)
             (map second))
        children))))

(defn tps-make-score-fn [graph routes]
  (fn [path]
    (->> (partition 2 1 (reverse path))
         (map #(get-in routes %))
         (reduce (fn [[flow score path-length] path]
                   [(+ flow (get-in graph [(first path) :flow]))
                    (+ score (* flow (count path)))
                    (+ path-length (count path))]) [0 0 0])
         ((fn [[flow score path-length]]
            (+ score (* (- *minutes* path-length) flow)))))))

(defn tps-report [children-fn score-fn combine-fn path node]
  (let [children (children-fn path node)]
    (if (empty? children)
      (score-fn (cons node path))
      (combine-fn (map #(tps-report
                         children-fn
                         score-fn
                         combine-fn
                         (cons node path) %) children)))))

(defn part1 [input]
  (let [graph (make-graph input)
        all-routes (all-distances graph 'AA)
        costs (traversal-costs graph all-routes)]
    (tps-report
     (tps-make-children-fn all-routes costs)
     (tps-make-score-fn graph all-routes)
     #(apply max %)
     (list)
     'AA)))

#_(combo/cartesian-product [1] [1])
#_(= 1651 (part1 test-input))
#_(= 1751 (part1 real-input))

(def ^:dynamic *costs* nil)

(defn binary-tps-report [children-fn score-fn seen path1 node1 path2 node2]
  (let [children1 (remove (set seen) (children-fn path1 node1))
        children2 (remove (set seen) (children-fn path2 node2))
        children-pairs
        (->> (combo/cartesian-product children1 children2)
             (remove (fn [[a b]] (= a b)))
             (map (fn [[c1 c2 :as pair]]
                    (vary-meta pair assoc
                               ::cost (+ (get-in *costs* [node1 c1])
                                         (get-in *costs* [node2 c2])))))
             (sort-by #(::cost (meta %)))
             (take 25))]
    (cond
      (and (empty? children1) (empty? children2))
      (+ (score-fn (cons node1 path1))
         (score-fn (cons node2 path2)))
      (and (= 1 (count children1))
           (= children1 children2))
      (max
       (binary-tps-report
        children-fn
        score-fn
        (conj seen (first children1))
        path1
        node1
        (cons node2 path2)
        (first children1))
       (binary-tps-report
        children-fn
        score-fn
        (conj seen (first children1))
        (cons node1 path1)
        (first children1)
        path2
        node2))
      (empty? children1)
      (apply max
             (map (fn [c2]
                    (binary-tps-report
                     children-fn
                     score-fn
                     (conj seen c2)
                     path1
                     node1
                     (cons node2 path2)
                     c2))
                  children2))
      (empty? children2)
      (apply max
             (map (fn [c1]
                    (binary-tps-report
                     children-fn
                     score-fn
                     (conj seen c1)
                     (cons node1 path1)
                     c1
                     path2
                     node2))
                  children1))
      :else
      (apply max
             (map (fn [[c1 c2]]
                    (binary-tps-report
                     children-fn
                     score-fn
                     (into seen [c1 c2])
                     (cons node1 path1)
                     c1
                     (cons node2 path2)
                     c2))
                  children-pairs
                  )))))


(defn part2 [input]
  (binding [*minutes* 26
            *take-opt* false]
    (let [graph (make-graph input)
          all-routes (all-distances graph 'AA)
          costs (traversal-costs graph all-routes)]
      (binding [*costs* costs]
        (binary-tps-report
         (memoize (tps-make-children-fn all-routes costs))
         (memoize (tps-make-score-fn graph all-routes))
         #{}
         (list)
         'AA
         (list)
         'AA)))))

#_(= 1707 (part2 test-input))

#_(time
   (do
     (def res (part2 real-input))
     (prn res)
     (beep)))



