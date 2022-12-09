(ns adv2022.day9.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]))

(def input (slurp "src/adv2022/day9/input.txt"))

(defn parse [s]
  (->> (edn/read-string (str "[" s "]"))
       (partition 2)
       (map reverse)
       (mapcat #(apply repeat %))))

(defn up [[x y]] [x (dec y)])
(defn down [[x y]] [x (inc y)])
(defn left [[x y]] [(dec x) y])
(defn right [[x y]] [(inc x) y])

(let [dir-map-fn {'U up 'D down 'L left 'R right}]
  (defn move-head-pos [pos dir]
    ((dir-map-fn dir) pos)))

(defn move-tail-pos [head-pos tail-pos]
  (let [diff (mapv - head-pos tail-pos)
        diff-abs (mapv #(Math/abs %) diff)]
    (cond
      (#{[0 0] [0 1] [1 1]} (sort diff-abs)) tail-pos
      ;; same row or column
      (first (filter zero? diff-abs)) 
      (condp = diff
        [0 2]  (down tail-pos)
        [0 -2] (up tail-pos)
        [2 0]  (right tail-pos)
        [-2 0] (left tail-pos))
      ;; diagonal case
      (#{[1 2] [2 2]} (sort diff-abs))
      (let [[diff-x diff-y] diff]
        (cond-> tail-pos
          (> diff-y 0) (down)
          (< diff-y 0) (up)
          (> diff-x 0) (right)
          (< diff-x 0) (left)))
      :else (throw (ex-info "Shouldn't get here" {:t tail-pos
                                                  :h head-pos
                                                  :diff diff
                                                  :d-abs diff-abs})))))

(defn part1 [data]
  (->> (reductions
        (fn [accum inst]
          (-> accum
              (update :head-pos move-head-pos inst)
              (#(assoc % :tail-pos (move-tail-pos (:head-pos %) (:tail-pos %))))))
        {:head-pos [0 4]
         :tail-pos [0 4]}
        (parse input))
       (map :tail-pos)
       distinct
       count))

#_(assert (= 5710 (part1 (parse input))))

(defn part2 [data]
  (->> (reductions
        (fn [accum inst]
          (reductions
           move-tail-pos-help
           (move-head-pos (first accum) inst)
           (rest accum)))
        (repeat 10 [0 0])
        data)
       (map last)
       distinct
       count))

#_(assert (= 2259 (part2 (parse input))))


