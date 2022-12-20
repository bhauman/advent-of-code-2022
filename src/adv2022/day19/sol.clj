(ns adv2022.day19.sol
  (:require
   [clojure.string :as string]
   [clojure.set :as set]
   [clojure.edn :as edn]
   [clojure.math :as math]
   [clojure.math.combinatorics :as combo]
   [adv2022.grid :as grid]
   [adv2022.utils :as util]))

(def input-test (slurp "src/adv2022/day19/input.test"))
(def input-real (slurp "src/adv2022/day19/input.txt"))

(defn parse [s]
  (->> s
       string/split-lines
       (map #(str "[" (string/replace % ":" " ") "]"))
       (map edn/read-string)
       (map (partial filter number?))))

(defn can-build? [{:keys [wallet blueprint-costs] :as blu}]
  (let [{:keys [ore clay obs]} wallet
        [_ ore-r-ore clay-r-ore obs-r-ore obs-r-clay
         geo-r-ore geo-r-obs]
        blueprint-costs]
    (cond-> #{}
      (and (>= ore geo-r-ore) (>= obs geo-r-obs)) (conj :geo-robot)
      (and (>= ore obs-r-ore) (>= clay obs-r-clay)) (conj :obs-robot)
      (>= ore clay-r-ore) (conj :clay-robot)
      (>= ore ore-r-ore) (conj :ore-robot))))

(defn pay-for-robot [{:keys [wallet blueprint-costs] :as blu} robot]
  (if ((can-build? blu) robot)
    (let [[_ ore-r-ore clay-r-ore obs-r-ore
           obs-r-clay geo-r-ore geo-r-obs]
          blueprint-costs]
      (update blu :wallet
              (fn [wallet]
                (condp = robot
                  :ore-robot (update wallet :ore - ore-r-ore)
                  :clay-robot (update wallet :ore - clay-r-ore)
                  :obs-robot (-> wallet
                                 (update :ore - obs-r-ore)
                                 (update :clay - obs-r-clay))
                  :geo-robot (-> wallet
                                 (update :ore - geo-r-ore)
                                 (update :obs - geo-r-obs))))))
    blu))

(defn add-robot [blu robot]
  (update-in blu [:robots robot] inc))

(defn robots-work [{:keys [robots] :as blu}]
  (let [{:keys [ore-robot clay-robot obs-robot geo-robot]} robots]
    (-> blu
        (update-in [:wallet :ore] + ore-robot)
        (update-in [:wallet :clay] + clay-robot)
        (update-in [:wallet :obs] + obs-robot)
        (update-in [:wallet :geo] + geo-robot))))

(defn make-blueprint [blueprint-costs]
  {:id (first blueprint-costs)
   :wallet {:ore 0 :clay 0 :obs 0 :geo 0}
   :robots {:ore-robot 1 :clay-robot 0 :obs-robot 0 :geo-robot 0}
   :blueprint-costs blueprint-costs})

;; on turn you can build one robot
;; on turn your robots do work

(defn blu-value [{:keys [wallet robots]}]
  (let [{:keys [ore clay obs geo]} wallet
        {:keys [ore-robot clay-robot obs-robot geo-robot]} robots]
    (+ (* 1 obs) (* 10000 geo) (* 1 obs-robot) (* 100 geo-robot))))

(defn do-turn [blu robot]
  (cond-> blu
    robot (pay-for-robot robot)
    true robots-work
    robot (add-robot robot)))

(defn children [blu]
  (->> (conj (can-build? blu) nil)
       (map (partial do-turn blu))
       (map #(update % ::depth inc))))

(defn bfs-leveler [previous-level]
  (->> previous-level
       (mapv children)
       (reduce into #{})
       (sort-by blu-value >)
       (take 3000)))

(defn optimizing-game-player [blueprint minutes]
  (->> (iterate bfs-leveler [(assoc blueprint ::depth 0)])
       (take (inc minutes))
       last
       first))

(defn quality-level [{:keys [id wallet]}]
  (* id (get wallet :geo)))

(defn play-to-quality-level [blu]
  (-> blu
      (optimizing-game-player 24)
      quality-level))

(defn part1 [blus]
  (->> blus
       (map play-to-quality-level)
       (reduce +)))

#_(= 33 (part1 (map make-blueprint (parse input-test))))

#_(= 1681
     (do
       (def res (part1 (map make-blueprint (parse input-real))))
       (prn res)
       res))

;; part 2

(defn part2 [blus]
  (->> (take 3 blus)
       (map #(optimizing-game-player % 32))
       (map #(get-in % [:wallet :geo]))))

#_(= [56 62]
     (part2 (map make-blueprint (parse input-test))))

#_(= 5394
     (do (def res2
           (apply *
                  (part2 (map make-blueprint (parse input-real)))))
         res2))



