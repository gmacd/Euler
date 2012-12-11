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

(defn sum-of-fibs [limit]
  "Return sum of even fibs <= 4,000,000"
  (reduce + (filter even? (for [x (fib) :while (< x (+ limit 2))] x))))

(def e2 (sum-of-fibs 4000000))



; Euclid's algorithm
(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn factor? [a pos-factor]
  (zero? (rem a pos-factor)))

(defn prime? [a]
  (cond
   	(< a 0) false
  	(<= a 2) true
    :else
   		(loop [pos-factor (Math/ceil (Math/sqrt a))]
        (if (<= pos-factor 1)
          true
          (if (factor? a pos-factor)
            false
            (recur (dec pos-factor)))))))

(defn largest-prime-factor [a]
  (loop [pos-factor (int (Math/ceil (Math/sqrt a)))]
    (if (<= pos-factor 1)
      0
      (if (and (factor? a pos-factor) (prime? pos-factor))
        pos-factor
        (recur (dec pos-factor))))))

(def e3 (largest-prime-factor 600851475143))



(defn num-digits [a]
  (count (take-while #(> % 0) (iterate #(int (/ % 10.0)) a))))

(defn digit-at [a digit-idx]
  (let [num-digits (num-digits a)
        multiple-of-10 (int (Math/pow 10 (- num-digits digit-idx)))]
	  (mod (int (/ a multiple-of-10)) 10)))

(defn palindrome? [a]
  (let [digits (for [idx (range 1 (+ 1 (num-digits a)))]
                 (digit-at a idx))]
    (= digits (reverse digits))))

(defn e4 []
  (apply max
         (for [a (range 999 99 -1)
               b (range (+ a 1) 99 -1)
               :let [c (* a b)]
               :when (palindrome? c)]
           c)))


