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

(defn modulo* [a b m]
  (mod (* (mod a m) (mod b m)) m))

(defn modulo+ [a b m]
  (mod (+ (mod a m) (mod b m)) m))

(defn create-initial-modulo-number [mods num]
  (->> mods
       (map (juxt identity (partial mod num)))
       (into {})))

(defn initialize-items [monkeys]
  (let [mods (map (comp #(nth % 3) second) monkeys)]
    (mapv
     #(update % 0 (partial map (partial create-initial-modulo-number mods)))
     monkeys)))

(defn update-vals [m f] 
  (reduce-kv (fn [mp k v] (assoc mp k (f v k))) {} m))

(defn square-mod-num [mod-num]
  (update-vals mod-num (fn [v m] (modulo* v v m))))

(defn add-mod-num [mod-num arg]
  (update-vals mod-num (partial modulo+ arg)))

(defn mult-mod-num [mod-num arg]
  (update-vals mod-num (partial modulo* arg)))

(defn op-on-modulo-number [op op-arg mod-num]
  (if (= op-arg 'old)
    (square-mod-num mod-num)
    (condp = op
      '+ (add-mod-num mod-num op-arg)
      '* (mult-mod-num mod-num op-arg))))

(defn mod-num-modulo [mod-num m]
  (get mod-num m))

(defn monkey-operate [monkeys item [_old op op-arg divis-by t-monkey f-monkey :as args]]
  (let [new-item (op-on-modulo-number op op-arg item)]
    (update-in monkeys [(if (zero? (mod-num-modulo new-item divis-by))
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
  (->>
   (iterate monkey-around [{} (initialize-items monkeys)])
   (take (inc num-rounds))
   last
   first
   vals
   (sort >)
   (take 2)
   (apply *)))

#_(assert (= 21115867968 (part2 start-monkeys 10000)))
(assert (= 210627168 (part2 start-monkeys 1000)))






