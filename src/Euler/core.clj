(ns Euler.core
  (:use [Euler.primes :as primes]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn multiple? [a b]
  "Is b a multiple of a?"
  (= 0 (rem a b)))

(defn multiple-of-any? [a bs]
  "Are any of bs a multiple of a?"
  (some #(multiple? a %) bs))

(defn multiples-in-range [xs limit]
  "Return a lazy seq of all the multiples of any of xs between 1 and limit-1"
  (filter #(multiple-of-any? % xs) (range limit)))


(defn fib []
  "Lazy fib seq"
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0N 1N])))

(defn sum-of-fibs [limit]
  "Return sum of even fibs <= 4,000,000"
  (reduce + (filter even? (for [x (fib) :while (< x (+ limit 2))] x))))


; Euclid's algorithm
(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn factor? [a pos-factor]
  (zero? (rem a pos-factor)))

(defn largest-prime-factor [a]
  (loop [pos-factor (int (Math/ceil (Math/sqrt a)))]
    (if (<= pos-factor 1)
      0
      (if (and (factor? a pos-factor) (primes/prime? pos-factor))
        pos-factor
        (recur (dec pos-factor))))))

(defn binomial-coeff [n] (/ (* n (+ n 1)) 2))

(defn all-binomial-coeffs []
  (for [i (drop 1 (range))] (binomial-coeff i)))

(defn digits [a]
  "Return seq of digits of integer a as ints"
  (map #(Character/digit % 10)
       (seq (if (instance? BigDecimal a)
              (.toPlainString a)
              (.toString a)))))

(defn digits-to-number [digits]
  "Convert the separate digits to a single number.  E.g. [1 2 3] -> 123"
  (apply +
         (for [i (range 0 (count digits))
               :let [d (nth digits i)
                     exp (- (dec (count digits)) i)
                     mul (int (Math/pow 10 exp))]]
           (* d mul))))

(defn palindrome? [a]
  (let [digits (digits a)]
    (= digits (reverse digits))))

(defn pandigital? [a]
  (apply distinct? (digits a)))

(defn zeroless-pandigital? [a]
  (let [digits (digits a)]
    (and (not-any? zero? digits)
        (apply distinct? digits))))

(defn all-permutations [things]
  (if (= 1 (count things))
    (list things)
    (for [head things
          tail (all-permutations (disj (set things) head))]
      (do
        (cons head tail)))))

(defn sum-of-squares [limit]
  "Sum of squares from 1 to limit (inclusive)"
  (reduce
   +
   (map #(int (Math/pow % 2)) (range 1 (inc limit)))))

(defn square-of-sum [limit]
  "Square of sum of numbers from 1 to limit (inclusive)"
  (int (Math/pow (reduce + (range 1 (inc limit))) 2)))

(defn any-divides [n divisors]
  "Do any of divisors divide n?"
  (true? (some #(zero? (mod n %)) divisors)))
