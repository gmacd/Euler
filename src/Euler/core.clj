(ns Euler.core)

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

(defn prime? [n]
  (cond
   (<= n 1) false
   (<= n 3) true
   (= (mod n 2) 0) false
   (= (mod n 3) 0) false
   :else
   (let [limit (Math/floor (Math/sqrt n))]
     (loop [pos-factor 5]
       (cond
        (>= pos-factor limit) true
        (= 0 (mod n pos-factor)) false
        (= 0 (mod n (+ pos-factor 2))) false
        :else (recur (+ pos-factor 6)))))))

(defn largest-prime-factor [a]
  (loop [pos-factor (int (Math/ceil (Math/sqrt a)))]
    (if (<= pos-factor 1)
      0
      (if (and (factor? a pos-factor) (prime? pos-factor))
        pos-factor
        (recur (dec pos-factor))))))


(defn num-digits [a]
  (count (take-while #(> % 0) (iterate #(int (/ % 10.0)) a))))

(defn digit-at [a digit-idx]
  (let [num-digits (num-digits a)
        multiple-of-10 (int (Math/pow 10 (- num-digits digit-idx)))]
	  (mod (int (/ a multiple-of-10)) 10)))

(defn digits [a]
  "Lazy seq of digits of a"
  (if (zero? a)
    [0]
    (for [idx (range 1 (+ 1 (num-digits a)))]
      (digit-at a idx))))

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


(defn split-number [a]
  (reduce conj []
          (for [n (range 1 (inc (num-digits a)))
                :let [digit (digit-at a n)]]
      	    digit)))
