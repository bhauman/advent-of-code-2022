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

(def input-test (slurp "src/adv2022/day21/input.test"))
(def input-real (slurp "src/adv2022/day21/input.txt"))

(defn parse [s]
  (->> (string/replace s #":" "")
       string/split-lines
       (map #(str "[" % "]"))
       (map edn/read-string)))

(defn make-monkey-fn [[d1 op d2]]
  (let [dep-keys (map keyword [d1 d2])]
    (vary-meta #(apply (resolve op) (map % dep-keys))
               assoc
               :deps (set dep-keys)
               :symbolic #(cons op (map % dep-keys)))))

(defn init-state [monkeys]
  (->> monkeys
       (map (fn [[k & [v & _ :as vs]]]
              [(keyword k)
               (if (number? v) v (make-monkey-fn vs))]))
       (group-by (comp number? second))
       (reduce-kv #(assoc %1 (if %2 :solved :left) (into {} %3)) {})))

(defn make-solve [get-f]
  (fn [{:keys [solved left] :as state}]
    (let [more-solved
          (->> left
               (filter #(every? solved (-> % val meta :deps)))
               (reduce #(assoc %1 (first %2) ((get-f (val %2)) solved)) {}))]
      (-> state
          (update :solved merge more-solved)
          (update :left #(apply dissoc % (keys more-solved)))))))

(defn part1 [input]
  (-> (->> (init-state input)
           (iterate (make-solve identity))
           (drop-while #(get-in % [:left :root])))
      first
      (get-in [:solved :root])))

(assert (= 152 (part1 (parse input-test))))

(assert (= 157714751182692 (part1 (parse input-real))))

(defn update-for-part2 [input]
  (-> (into {} (map (juxt first identity)) input)
      (dissoc 'humn)
      (update 'root (fn [[k arg1 op arg2]] [k arg1 '- arg2]))
      vals))

(defn get-symbolic-solution [input]
  (-> input
      (->> update-for-part2
           init-state
           (iterate (make-solve identity))
           (partition 2 1)
           (drop-while #(apply not= %))
           ffirst)
      (assoc-in [:solved :humn] 'humn)
      (->>
       (iterate (make-solve #(-> % meta :symbolic)))
       (drop-while #(get-in % [:left :root]))
       first)
      (get-in [:solved :root])))

(defn norm [[op exp1 exp2 :as all]]
  (condp = op
    '- (if (number? exp2)
         ['+ (* -1 exp2) exp1]
         ['+ exp1 ['* -1 exp2]])
    '/ (if (number? exp2)
         ['* (/ 1 exp2) exp1]
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

