; e40-e41
(ns Euler.e40
  (:use [Euler.core :as core]
         [Euler.primes :as primes])
  (:require [clojure.math.combinatorics :as combo]))

(defn e40 []
  (->> (combo/permutations [9 8 7 6 5 4 3 2 1])
       (map core/digits-to-number)
       (filter odd?)
       (filter primes/prime?)))

(defn perms []
  (->> (combo/subsets [1 2])
       (reverse)
       (map #(flatten (combo/permutations %)))))

;(defn e41 []
;  (first (filter (comp prime? zeroless-pandigital?)
;                 (reverse (all-primes 987654321)))))

;(defn e41-test []
;  (take 10 (primes 987654321))
