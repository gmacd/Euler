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


(defn sum-of-squares [limit]
  "Sum of squares from 1 to limit (inclusive)"
	(reduce
   +
   (map #(int (Math/pow % 2)) (range 1 (inc limit)))))

(defn square-of-sum [limit]
  "Square of sum of numbers from 1 to limit (inclusive)"
  (int (Math/pow (reduce + (range 1 (inc limit))) 2)))

(defn e6 [limit]
  (- (square-of-sum limit) (sum-of-squares limit)))


; e7
; Hacky mess of magic numbers based on code here:
; http://clojuredocs.org/clojure_core/clojure.core/reduce
(nth (reduce
 (fn [primes number]
   (if (some zero? (map (partial mod number) primes))
     primes
     (conj primes number)))
 [2] (take 60000 (iterate #(+ 2 %) 3))) 10000)


(defn split-number [a]
	(reduce conj []
  	      (for [n (range 1 (inc (num-digits a)))
    	          :let [digit (digit-at a n)]]
      	    digit)))



;(defn num-digits [a]
;  (count (take-while #(> % 0) (iterate #(bigint (/ % 10.0)) a))))

;(defn digit-at [a digit-idx]
;  (let [num-digits (num-digits a)
;        multiple-of-10 (bigint (Math/pow 10 (- num-digits digit-idx)))]
;	  (mod (bigint (/ a multiple-of-10)) 10)))

; TODOOOO
(defn split-number [a]
	(reduce conj []
  	      (for [n (range 1 (inc (num-digits a)))
    	          :let [digit (digit-at a n)]]
      	    digit)))

(defn e8 [a]
  (apply max (for [group (partition 5 1 (split-number a))
	           :let [sum (apply + group)]]
  	     sum)))

(BigInteger. "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")
(e8 (BigInteger. "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"))
