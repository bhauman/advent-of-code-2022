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

(defn monkey-op [monkeys item [_old op op-arg divis-by t-monkey f-monkey :as args]]
  (let [new-item (-> item
                     ((if (= op '*) * +) (if (= op-arg 'old) item op-arg))
                     (math/floor-div 3))]
    (update-in monkeys [(if (zero? (mod new-item divis-by)) t-monkey f-monkey) 0]
               conj new-item)))

(defn turn [monkeys [items op-args]]
  (reduce #(monkey-op %1 %2 op-args) monkeys items))

(defn do-round [count-accum-monkeys]
  (reduce
   (fn [[accum monkeys] idx]
     (let [monkey (get monkeys idx)]
       [(update accum idx (fnil + 0) (-> monkey first count))
        (turn
         (assoc-in monkeys [idx 0] '())
         monkey)]))
   count-accum-monkeys
   (range (count (second count-accum-monkeys)))))

(defn run-rounds [num-rounds monkeys]
  (->> (iterate do-round [{} monkeys])
       (take (inc num-rounds))
       last
       first
       vals
       (sort >)
       (take 2)
       (apply *)))

(def part1 (partial run-rounds 20))

(assert (= 88208 (part1 start-monkeys)))
#_(assert (= 10605 (part1 start-monkeys))) ;; test


;; part2 modulo nums

(def ^:dynamic *base-mod* 0)

(defn modulo* [a b m]
  (mod (* (mod a m) (mod b m)) m))

(defn modulo+ [a b m]
  (mod (+ (mod a m) (mod b m)) m))

(defn mod-op [op op-arg mod-num]
  (let [op-arg (if (= op-arg 'old) mod-num op-arg)]
    (condp = op
      '+ (modulo+ mod-num op-arg *base-mod*)
      '* (modulo* mod-num op-arg *base-mod*))))

(defn monkey-operate [monkeys item [_old op op-arg divis-by t-monkey f-monkey :as args]]
  (let [new-item (mod-op op op-arg item)]
    (update-in monkeys [(if (zero? (mod new-item divis-by))
                          t-monkey
                          f-monkey) 0]
               conj new-item)))

(defn monkey-turn [monkeys [items op-args]]
  (reduce #(monkey-operate %1 %2 op-args) monkeys items))

(defn monkey-around [count-accum-monkeys]
  (reduce
   (fn [[accum monkeys] idx]
     (let [monkey (get monkeys idx)]
       [(update accum idx (fnil + 0) (-> monkey first count))
        (monkey-turn
         (assoc-in monkeys [idx 0] '())
         monkey)]))
   count-accum-monkeys
   (range (count (second count-accum-monkeys)))))

(defn part2 [monkeys num-rounds]
  (let [mods (map (comp #(nth % 3) second) monkeys)]
    (binding [*base-mod* (apply * mods)]
      (->>
       (iterate monkey-around [{} monkeys])
       (take (inc num-rounds))
       last
       first
       vals
       (sort >)
       (take 2)
       (apply *)))))

#_(assert (= 21115867968 (part2 start-monkeys 10000)))
(assert (= 210627168 (part2 start-monkeys 1000)))

;; this can be reduced to a single implementation but I've spent enough time already




