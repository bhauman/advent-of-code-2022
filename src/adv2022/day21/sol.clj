(ns adv2022.day21.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.math :as math]
   [clojure.walk :refer [postwalk postwalk-demo]]
   [net.cgrand.xforms :as x]
   [clojure.data.priority-map :refer [priority-map] :as pri]
   [clojure.core.match :refer [match]]))

#_(add-tap (bound-fn* clojure.pprint/pprint))
;; can sort by deps
;; or can solve in steps

(def input-test (slurp "src/adv2022/day21/input.test"))
(def input-real (slurp "src/adv2022/day21/input.txt"))

(defn parse [s]
  (->> (string/replace s #":" "")
       string/split-lines
       (map #(str "[" % "]"))
       (map edn/read-string)
       (sort-by (comp number? second))
       reverse))

(defn init-state [monkeys]
  (let [init (->> monkeys
                  (map (fn [[k v & vs]]
                         [(keyword k)
                          (if (number? v)
                            v
                            (let [d1 v
                                  [op d2] vs 
                                  [dep1 dep2] (map keyword [d1 d2])]
                              (vary-meta (fn [monkey-map]
                                           (let [arg1 (get monkey-map dep1)
                                                 arg2 (get monkey-map dep2)]
                                             (assert (and arg1 arg2))
                                             ((eval op) arg1 arg2)))
                                         assoc
                                         :deps #{dep1 dep2}
                                         :symbolic (fn [monkey-map]
                                                     (list
                                                      op
                                                      (get monkey-map dep1 dep1)
                                                      (get monkey-map dep2 dep2))))))]))
                  (group-by (comp number? second)))]
    {:solved (into {} (get init true))
     :left (into {} (get init false))}))

(defn solve [{:keys [solved left symbolic?] :as state}]
  (let [get-f (if symbolic? #(-> % meta :symbolic) identity)
        more-solved
        (->> left
             (filter #(every? solved (-> % second meta :deps)))
             (reduce #(assoc %1 (first %2) ((get-f (second %2)) solved)) {}))]
    (-> state
        (update :solved merge more-solved)
        (update :left (fn [left] (apply dissoc left (keys more-solved)))))))

(defn part1 [input]
  (-> (->> (init-state input)
           (iterate solve)
           (drop-while #(get-in % [:left :root]))
           first)
      (get-in [:solved :root])))

(assert (= 152 (part1 (parse input-test))))

(assert (= 157714751182692 (part1 (parse input-real))))

;; tempting to put together a function for part2

(defn update-for-part2 [input]
  (-> (into {}
            (map (juxt first identity))
            input)
      (dissoc 'humn)
      (update 'root (fn [[k arg1 op arg2]] [k arg1 '- arg2]))
      vals
      (->> (sort-by (comp number? second)))
      reverse))

(defn get-symbolic-solution [input]
  (let [initially-solved
        (-> (->> input
                 update-for-part2
                 init-state
                 (iterate solve)
                 (partition 2 1)
                 (drop-while
                  (fn [[state next-state]] (not= state next-state)))
                 ffirst))]
    (-> initially-solved
        (assoc-in [:solved :humn] 'humn)
        (assoc :symbolic? true)
        (->>
         (iterate solve)
         (drop-while #(get-in % [:left :root]))
         first)
        (get-in [:solved :root]))))

(defn norm [[op exp1 exp2 :as all]]
  (condp = op
    '- (if (number? exp2)
         ['+ (* -1 exp2) exp1]
         ['+ exp1 ['* -1 exp2]])
    '/ (if (number? exp2)
         ['* (/ 1 exp2) exp1 ]
         all)
    (if (number? exp2) [op exp2 exp1] all)))

(defn simplify-exp [[op x exp2 :as arg]]
  (match [op (if (sequential? exp2) (vec exp2) [])]
         ['+ ['+ y other]] ['+ (+ x y) other]
         ['* ['* y other]] ['* (* x y) other]
         ['* ['+ y other]] ['+ (* x y) ['* x other]]
         :else arg))

(def normalize (partial postwalk #(cond-> % (sequential? %) norm)))
(def simplifier (partial postwalk #(cond-> % (sequential? %) simplify-exp)))

(defn part2 [input]
  (-> (get-symbolic-solution input)
      normalize
      simplifier
      simplifier
      ((fn [[plus b [mult a x]]]
         (assert (= ['+ '* 'humn] [plus mult x]))
         (/ (- b) a)))))

(assert (= 301 (part2 (parse input-test))))
(assert (time (= 3373767893067 (part2 (parse input-real)))))

