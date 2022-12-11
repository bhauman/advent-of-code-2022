(ns adv2022.day11.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.math :as math]
   [clojure.math.combinatorics :as combo]))

(def start-monkeys (-> (str "[" (slurp "src/adv2022/day11/input.txt") "]")
                       (string/replace #"(\:)" "")
                       (edn/read-string)
                       (->> (partition-by #(= 'Monkey %))
                            (mapv (partial filter #(or (number? %) (#{'old '* '+} %))))
                            (keep not-empty)
                            (map rest)
                            (mapv #(split-with number? %)))))

(def ^:dynamic *releif* true)

(defn monkey-op [monkeys item [_old op op-arg divis-by t-monkey f-monkey :as args]]
  (let [new-item (cond-> item
                   true ((if (= op '*) * +) (if (= op-arg 'old) item op-arg))
                   *releif* (math/floor-div 3))]
    (update-in monkeys [(if (zero? (mod new-item divis-by)) t-monkey f-monkey) 0]
               conj new-item)))

(defn turn [monkeys [items op-args]]
  (reduce #(monkey-op %1 %2 op-args) monkeys items))

(defn do-round [[_ monkeys]]
  (->> (reductions
        (fn [[_ monkeys] idx]
          (let [monkey (get monkeys idx)]
            [{idx (-> monkey first count)}
             (turn
              (assoc-in monkeys [idx 0] '())
              monkey)]))
        [{} monkeys]
        (range (count monkeys)))
       (filter first)))

(defn run-rounds [num-rounds monkeys]
    (->> 
     (iterate (comp do-round last) [[{} monkeys]])
     (drop 1)
     (take num-rounds)
     (reduce concat)
     (map first)
     (apply merge-with +)
     vals
     (sort >)
     (take 2)
     (apply *)))


(def part1 (partial run-rounds 20))

(assert (= 88208 (part1 start-monkeys)))

(defn part2 [monkeys]
  (binding [*releif* false]
    ))



