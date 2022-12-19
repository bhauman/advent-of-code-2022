(ns adv2022.utils
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.math.combinatorics :as combo])
  (:import
   [clojure.lang PersistentQueue]))

(defn queue [& args]
  (into PersistentQueue/EMPTY args))
