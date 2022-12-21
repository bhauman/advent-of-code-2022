(ns adv2022.day20.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.math :as math]
   [clojure.data.priority-map :refer [priority-map] :as pri]))

(def input-test (slurp "src/adv2022/day20/input.test"))
(def input-real (slurp "src/adv2022/day20/input.txt"))
(def parse #(as-> % x (str "[" % "]") (edn/read-string x)))

(def KEYMULT  10000000)

(defn init-pri-map [x]
  (into (priority-map)
        (map-indexed (fn [i v]
                       [[v i] (* (inc i) KEYMULT)]) x)))

(defn previous-pri [pri-map value]
  (first (pri/rsubseq pri-map < value)))

(defn rebalance [pri-map]
  (->> pri-map
       (map-indexed
        (fn [i [v _]] [v (* (inc i) KEYMULT)]))
       (into (priority-map))))

(defn destination-loc [pri-map [num _ :as num-key]]
  (let [num-pri-val (get pri-map num-key)
        pri-map (dissoc pri-map num-key)
        num-places (mod (abs num) (count pri-map))]
    (when (not (zero? num-places))
      (first (if (pos? num)
               (drop num-places
                     (concat (pri/subseq pri-map > num-pri-val)
                             (seq pri-map)))
               (drop (dec num-places)
                     (concat (pri/rsubseq pri-map < num-pri-val)
                             (rseq pri-map))))))))

(defn move [pri-map num-key]
  (if (zero? (first num-key))
    pri-map
    (if-let [[_ a-pri-val :as after-loc]
             (destination-loc pri-map num-key)]
      (let [pri-map (dissoc pri-map num-key)
            [_ b-pri-val] (or (previous-pri pri-map a-pri-val)
                              [nil Long/MAX_VALUE])
            pri-diff (- a-pri-val b-pri-val)]
        (cond-> pri-map
          true (assoc num-key (+ b-pri-val (math/floor-div pri-diff 2)))
          (< pri-diff 10) rebalance))
      pri-map)))

(defn grove-coords [resv]
  (->> (cycle resv)
       (drop-while #(not= 0 %))
       (iterate (partial drop 1000))
       (map first)
       (drop 1)
       (take 3)))

(defn part1 [input]
  (let [pri-map (init-pri-map input)]
    (->> (reduce move pri-map (keys pri-map))
         (map ffirst)
         grove-coords
         (reduce +))))

#_(= 3 (part1 (parse input-test)))

#_(= 4151 (part1 (parse input-real)))

(defn part2 [input]
  (let [input (map #(* 811589153 %) input)
        pri-map (init-pri-map input)]
    (->> (iterate #(reduce move % (keys pri-map))  pri-map)
         (drop 1)
         (take 10)
         last
         (map ffirst)
         grove-coords
         (reduce +))))

#_(= 1623178306 (part2 (parse input-test)))

#_(= 7848878698663
     (do
       (def res (part2 (parse input-real)))
       (prn [:res res])
       res))

;; ALTERNATE IMPLEMENTATION
;; now that I understand the problem better
;; simulate a cicular list
;; takes longer because negative nums are translated into positive nums

(defn lazy-insert [lst item idx limit]
  (if (= idx 0)
    (cons item (take limit lst))
    (cons (first lst)
          (lazy-seq
           (lazy-insert (rest lst) item (dec idx) (dec limit))))))

(defn move-item [lst [index-diff _ :as item-a]]
  (if (zero? index-diff)
    lst
    (let [new-size (dec (count lst))
          diff (cond->> (mod (abs index-diff) new-size)
                 (neg? index-diff) (- new-size))
          items (->> (cycle lst)
                     (drop-while #(not= item-a %))
                     rest
                     (take new-size))]
      (lazy-insert (concat items items) item-a diff new-size))))

(defn test-part1 [input]
  (let [lst (map-indexed
             (comp vec reverse vector)
             input)]
    (->> (reduce move-item lst lst)
         (map first)
         grove-coords
         (reduce +))))

#_(= 3 (test-part1 (parse input-test)))
#_(= 4151 (time (test-part1 (parse input-real))))

(defn test-part2 [input]
  (let [input (map #(* 811589153 %) input)
        lst (map-indexed
             (comp vec reverse vector)
             input)]
    (->> (iterate #(reduce move-item % lst) lst)
         (drop 1)
         (take 10)
         last
         (map first)
         grove-coords
         (reduce +))))

#_(= 1623178306 (test-part2 (parse input-test)))


#_(= 7848878698663
     (do
       (def res-test (test-part2 (parse input-real)))
       (prn [:res res-test])
       res-test))
