; e40-e41
(ns Euler.e40
  (:require
   [Euler.core :as core]
   [Euler.primes :as primes]))

(defn e41 []
  (first (filter (comp core/prime? core/zeroless-pandigital?)
                 (reverse (primes/all-primes 987654321)))))

(defn e41-test []
  (take 10 (primes/primes 987654321))
