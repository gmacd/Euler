(ns Euler.core)

(defn multiple? [a b]
  "Is b a multiple of a?"
  (= 0 (rem a b)))

(defn multiple-of-any? [a bs]
  "Are any of bs a multiple of a?"
  (some #(multiple? a %) bs))

(defn multiples-in-range [xs limit]
  "Return a lazy seq of all the multiples of any of xs between 1 and limit-1"
  (filter #(multiple-of-any? % xs) (range limit)))

(defn e1 [xs limit]
  "Return sum of all multiples of xs between 1 and limit-1"
  (reduce + (multiples-in-range xs limit)))


(defn fib []
  "Lazy fib seq"
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0N 1N])))

(defn e2 [limit]
  "Return sum of even fibs <= 4,000,000"
  (reduce + (filter even? (for [x (fib) :while (< x (+ limit 2))] x))))


; Euclid's algorithm
(defn gcd [a b]
  (if (= b 0)
    a
    (recur b (mod a b))))


(defn largest-prime-factor [a]
  (loop [pos-factor a]
    (if (= (gcd a pos-factor) 0)
      pos-factor)
    (recur (- pos-factor 1))))

