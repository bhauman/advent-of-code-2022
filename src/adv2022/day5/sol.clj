(ns adv2022.day5.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]))

(def columns
  ;; file prepared with emacs rectangle commands and a macro
  (->> (str "[" (slurp "src/adv2022/day5/input-columns.txt") "]")
       read-string
       (partition-by integer?)
       (remove (comp integer? first))
       (mapv #(map first %))))

(def commands
  (->> (str "[" (slurp "src/adv2022/day5/input.txt") "]")
       read-string
       (filter integer?)
       (partition 3)))

(defn move-a->b [columns [index-a index-b]]
  (-> columns
      (update (dec index-a) rest)
      (update (dec index-b) conj (first (nth columns (dec index-a))))))

#_(move-a->b columns [1 2])

(defn execute-command [columns [times a b]]
  (reduce move-a->b columns (repeat times [a b])))

(defn part1 [columns commands]
  (map first
       (reduce execute-command columns commands)))

#_(= '(S V F D L G L W V)
     (part1 columns commands))

(defn move-x-a->b [columns [num index-a index-b]]
  (-> columns
      (update (dec index-a) #(drop num %))
      (update (dec index-b) #(concat (take num (nth columns (dec index-a))) %))))

#_(move-x-a->b columns [2 1 2])

(defn part1 [columns commands]
  (map first
       (reduce move-x-a->b columns commands)))



