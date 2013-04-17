(ns Euler.e20
  (:use Euler.util))

(defn e20 []
  (->> 100N
      (fac)
      (str)
      (seq)
      (map #(Character/digit % 10))
      (apply +)))
