(ns adv2022.day7.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.walk :refer [postwalk postwalk-demo]]))

(def dir-commands-output
  (->> (slurp "src/adv2022/day7/input.txt")
       (string/split-lines)
       (map (comp read-string #(str "[" % "]")))
       (partition-by #(or (= '[$ ls] %)
                          (= 'dir (first %))
                          (number? (first %))))
       (mapcat (fn [[com & rest :as coms]]
                 (if (= '[$ ls] com)
                   [(conj com (vec rest))]
                   coms)))))

(defn make-dir-tree [{:keys [path tree] :as data} [_ command arg]]
  (cond
    (= 'ls command)
    (-> (update-in data path
                   #(reduce (fn [dir-map [typ nam :as entry]]
                                        (assoc dir-map nam (if (dir? entry) {} typ))) %
                            arg )))
    (= '[cd /] [command arg])
    (assoc data :path [:tree])
    (= '[cd ..] [command arg]) 
    (update data :path #(if (= % [:tree]) [:tree] (vec (butlast %))))
    :else 
    (update data :path conj arg)))

(defn directory-totals [input-commands]
  (->> input-commands
       (reduce make-dir-tree {:path [:tree] :tree {}})
       :tree
       (postwalk (fn [node]
                   (if (map? node)
                     [(apply + (map #(if (vector? %) (first %) %) (vals node)))
                      (filter vector? (vals node))]
                     node)))
       flatten))

(defn part1 [data]
  (->> (directory-totals data)
       (filter #(<= % 100000))
       (reduce +)))

(assert (= 1770595 (part1 dir-commands-output))
          )

(defn part2 [dir-commands-output]
  (let [space-max  70000000
        space-needed 30000000
        totals (directory-totals dir-commands-output)
        space-avail (- space-max (reduce max totals))
        smallest-amount-needed (- space-needed space-avail)]
    (first (filter #(> % smallest-amount-needed) (sort totals)))))

(assert (= 2195372 (part2 dir-commands-output))
          )







