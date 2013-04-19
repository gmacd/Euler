; e40-e41
(ns Euler.e40
  (:use [Euler.core :as core]
        [Euler.primes :as primes])
  (:require [clojure.math.combinatorics :as combo]))


(defn perms [items]
  (let [subsets (for [x (range 9 0 -1)] (range x 0 -1))]
    (reduce concat (map #(combo/permutations %) subsets))))

(defn e40 []
  (->> (perms [9 8 7 6 5 4 3 2 1])
       (map core/digits-to-number)
       (filter odd?)
       (filter primes/prime?)))
