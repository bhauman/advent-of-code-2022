(ns adv2022.day22.sol
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.core.match :refer [match]]
   [adv2022.grid :as grid]
   [clojure.math.combinatorics :as combo]))

;; I'm working on this in dec 2023
#_(remove-ns 'adv2022.day22.sol)

#_(add-tap (bound-fn* clojure.pprint/pprint))

(def in (->> (slurp "src/adv2022/day22/input.txt")
             (string/split-lines)))

(def ^:dynamic *input* (mapv vec (butlast (butlast in))))

(defn pp-in [& [x]]
  (map #(apply str %) (or x *input*)))

(def inst (->> (partition-by #{\R \L} (last in))
               (cons [\R])
               (partition 2)
               (map (fn [[[d] n]]
                      [d (parse-long (apply str n))]))))

(def start-pos (fn [] [0 (count (take-while #{\space} (first *input*)))]))
(def start-dir [-1 0]) ;; start north so first turn is to right
(def height #(count *input*))
(def width #(apply max (map count *input*)))

(defn get-loc [pos] (get-in *input* pos))

(defn turn-right [[y x]] [x (- y)])

(def turn-left (comp turn-right turn-right turn-right))

(defn point? [a]
  (and (vector? a)
       (= (count a) 2)
       (every? number? a)))

(def dir?
  (into {}
        (map-indexed
         (fn [i d] [d i])
         (take 4 (iterate turn-right [0 1])))))

(defn next-pos [dir pos]
  {:pre [(dir? dir) (point? pos)]
   :post [(point? %)]}
  (mapv + dir pos))

(defn next-pos-wrap [dir pos]
  {:pre [(dir? dir) (point? pos)]
   :post [(point? %)]}
  (mapv mod (next-pos dir pos) [(height) (width)]))

(defn next-pos-ignore-spaces [dir pos]
  (let [res (next-pos-wrap dir pos)
        ch (get-loc res)]
    (if (or (= ch \space) (nil? ch))
      (recur dir res)
      res)))

(defn edge-val? [ch]
  (or (= ch \space) (nil? ch)))

(defn advance-pos [dir pos]
  {:pre [(dir? dir) (point? pos)]
   :post [(point? %)
          (not (edge-val? (get-loc %))) ]}
  (let [next-pos (next-pos-ignore-spaces dir pos)]
    (if (= \# (get-loc next-pos))
      pos
      next-pos)))

(defn moving-fn [{:keys [pos dir] :as state} [turn places :as inst]]
  {:pre [(dir? dir)
         (point? pos)
         (not (edge-val? (get-loc pos)))
         (vector? inst)
         (#{\R \L} turn)
         (number? places)]
   :post [(map? %) (find % :pos) (find % :dir)]}
  (->> (assoc state :dir (condp = turn
                           \R (turn-right dir)
                           \L (turn-left dir)))
       (iterate #(assoc % :pos (advance-pos (:dir %) (:pos %))))
       (drop places)
       first))

(defn password [{:keys [pos dir]}]
  {:pre [(point? pos) (dir? dir)]
   :post [(number? %)]}
  (+ (* 1000 (inc (first pos)))
     (* 4 (inc (second pos)))
     (dir? dir)))

;; part 1
#_(->> (reductions
        moving-fn
        {:pos (start-pos)
         :dir start-dir}
        inst)
       last
       password
       ) ;; => 93226


(def sector-width #(/ (width) 3))

(def translate #(mapv + %1 %2))
(def rotate-right turn-right)

(defn rotater [turn-fn]
  (fn [origin p]
    (-> (translate p (map - origin))
        turn-fn
        (translate origin)
        (->> (mapv long)))))

(def rotate-right-around (rotater turn-right))
(def rotate-left-around (rotater turn-left))

(defn sector [pos]
  (mapv #(int (/ % (sector-width))) pos))

(defn origin-of-sector [pos]
  (mapv (partial * (sector-width)) (sector pos)))

(defn center-of-sector [pos]
  (mapv (partial + (double (/ (dec (sector-width)) 2)))
        (origin-of-sector pos)))

(defn sector-roll-right [pos]
  (let [center (center-of-sector pos)]
    (rotate-right-around center pos)))

(defn sector-roll-left [pos]
  (let [center (center-of-sector pos)]
    (rotate-left-around center pos)))

;;; put it into normal form

(defn normal-form [m pos-in-sector dir]
  ;; rotate last sector [3 0] right
  (let [[orig-y orig-x] (origin-of-sector pos-in-sector)]
    (->> (for [y (range orig-y (+ orig-y (sector-width)))
               x (range orig-x (+ orig-x (sector-width)))]
           [y x])
         (reduce (fn [acc pos]
                   (let [new-pos
                         (translate (sector-roll-left pos)
                                    (mapv (partial * (sector-width)) dir))]
                     (-> acc
                         (assoc-in new-pos (get-loc pos))
                         (assoc-in pos \space))))
                 (mapv
                  #(vec (take (width)
                              (concat % (repeat \space))))
                  m)))))

(def norm-input
  (normal-form *input* [(dec (height)) 0] [0 1]))

(defn get-norm-loc [pos] (get-in norm-input pos))

(defn in-middle-sector-vert? [pos]
  (= 1 (second (sector pos))))

(defn roll-right-from-direction [[orig-dir cur-dir pos]]
  [orig-dir
   (turn-right cur-dir)
   (let [new-pos (->> (turn-right orig-dir)
                      (mapv (partial * (sector-width)))
                      (translate (sector-roll-right pos)))]
     [(mod (first new-pos) (height)) (second new-pos)])])

(defn rolling-search [roll-dir orig-dir pos]
  (->> (iterate roll-right-from-direction [roll-dir orig-dir pos])
       (take 5) ;; limit allows failure
       (drop-while #(edge-val? (get-norm-loc (last %))))
       first
       (drop 1)))

(defn rolling-outer-search [roll-dir orig-dir pos]
  ;; this position is not an edge position
  (->> (iterate roll-right-from-direction [roll-dir orig-dir pos])
       (drop 1)
       (take 5)
       (drop-while #(edge-val?
                     (get-norm-loc
                      (next-pos (second %) (last %)))))
       first
       (drop 1)))

(defn middle-next-dir-pos-vert [dir pos]
  ;; up and down movement allowed
  (if (#{[1 0] [-1 0]} dir)
    [dir (next-pos-wrap dir pos)]
    (let [pos' (next-pos dir pos)]
      (if (edge-val? (get-norm-loc pos'))
        (rolling-search dir dir pos')
        [dir pos']))))

(defn lanes-next-dir-pos-vert [dir pos]
  (let [pos' (next-pos dir pos)]
    ;; in a lane if you an edge value you are on the outer edge
    (if (edge-val? (get-norm-loc pos')) 
      (let [[next-dir next-edge-pos]
            (rolling-outer-search
             (condp = (second (sector pos))
               0 [0 -1]
               2 [0 1])
             dir
             pos)
            ;; need to move forward to not be edge value 
            pos' (next-pos next-dir next-edge-pos)]
        (assert (not (edge-val? (get-norm-loc pos'))))
        [next-dir pos'])
      [dir pos'])))

(defn cube-wrapping-next-dir-pos [[dir pos]]
  (if (in-middle-sector-vert? pos)
    (middle-next-dir-pos-vert dir pos)
    (lanes-next-dir-pos-vert dir pos)))

(defn cube-advance-dir-pos [[dir pos :as dir-pos]]
  (let [next-dir-pos (cube-wrapping-next-dir-pos dir-pos)]
    (if (= \# (get-norm-loc (last next-dir-pos)))
      dir-pos
      next-dir-pos)))

(defn cube-moving-fn [{:keys [pos dir] :as state} [turn places :as inst]]
  (->> (assoc state :dir (condp = turn
                           \R (turn-right dir)
                           \L (turn-left dir)))
       (iterate #(let [[dir' pos']
                       (cube-advance-dir-pos [(:dir %) (:pos %)])]
                   (-> %
                       (assoc :pos pos')
                       (assoc :dir dir')
                       (assoc :inst inst))))
       (drop places)
       first))

;; part 2
#_(->> (reductions
        cube-moving-fn
        {:pos (start-pos)
         :dir start-dir}
        inst)
       last
       password) ;; => 37415



