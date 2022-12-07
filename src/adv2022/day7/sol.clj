(ns adv2022.day7.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.walk :refer [postwalk postwalk-demo]]))


(def dir? #(= 'dir (first %)))
(def file? #(number? (first %)))
(def command? #(= '$ (first %)))
(defn command-pred [com-sym]
  (fn [%] (and (command? %) (= (second %) com-sym))))
(def ls? (command-pred 'ls))
(def cd? (command-pred 'cd))

(def cd-up? #(= '[$ cd ..] %))
(def cd-root? #(= '[$ cd /] %))
(def cd-dir? #(and (cd? %)
                   (not (cd-up? %))
                   (not (cd-root? %))))

(def dir-commands-output
  (->> (slurp "src/adv2022/day7/input.txt")
       (string/split-lines)
       (map (comp read-string #(str "[" % "]")))
       (partition-by #(or (= '[$ ls] %)
                          (dir? %)
                          (file? %)))
       (mapcat (fn [[com & rest :as coms]]
                 (if (ls? com) [(conj com (vec rest))] coms)))))

(defn make-dir-tree [{:keys [path tree] :as data} command]
  (cond
    (ls? command)
    (-> (update-in data path
                   #(reduce (fn [dir-map [typ nam :as entry]]
                                        (assoc dir-map nam (if (dir? entry) {} typ))) %
                            (last command))))
    (cd-root? command)
    (assoc data :path [:tree])
    (cd-up? command)
    (update data :path #(if (= % [:tree]) [:tree] (vec (butlast %))))
    (cd-dir? command)
    (update data :path conj (last command))))

(defn parse-into-file-tree [dir-walk-output]
  (reduce make-dir-tree {:path [:tree] :tree {}} dir-walk-output))

(defn total-size [node]
  (->> (vals node)
       (map #(if (map? %) (::size %) %))
       (reduce +)))

(defn annotate-dir-totals [tree-result]
  (let [res 
        (postwalk
         (fn [node]
           (if (and (map-entry? node) (map? (second node)))
             [(first node)
              (assoc
               (second node)
               ::size
               (total-size (second node)))]
             node))
         tree-result)]
    (assoc res ::size (total-size res))))

(defn directory-totals [data]
  (->> data
       parse-into-file-tree
       :tree
       annotate-dir-totals
       (tree-seq map? #(filter map? (vals %)))
       (map ::size)))

(defn part1 [data]
  (->> (directory-totals data)
       (filter #(<= % 100000))
       (apply +)))

#_(assert (= 1770595 (part1 dir-commands-output)))

(defn part2 [dir-commands-output]
  (let [space-max  70000000
        space-needed 30000000
        totals (directory-totals dir-commands-output)
        space-avail (- space-max (reduce max totals))
        smallest-amount-needed (- space-needed space-avail)]
    (first (filter #(> % smallest-amount-needed) (sort totals)))))

#_(assert (= 2195372 (part2 dir-commands-output)))







