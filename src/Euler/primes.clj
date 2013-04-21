(ns Euler.primes)

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

(defn- array-and
  "And each pair of items in seqs a and b"
  [a b]
  (map #(and %1 %2) a b))


(defn- array-of-possible-primes [n limit]
  (let [lowerBound (Math/pow n 2)]
    (concat [false false]
            (for [i (range 2 (inc limit))]
              (if (< i lowerBound)
                true
                (not (zero? (rem i n))))))))


(defn- next-n [n coll]
  (ffirst (filter second (drop (inc n) (map-indexed vector coll)))))


(defn- sieve-of-erastothenes-bools [limit]
  (loop [n 2
         bools (array-of-possible-primes n limit)]
    (if (>= (Math/pow n 2) limit)
      bools
      (let [n2 (next-n n bools)]
      	(recur n2
               (array-and bools (array-of-possible-primes n2 limit)))))))


(defn- sieve-of-erastothenes-ints [limit]
  (map first (filter second (map-indexed vector (sieve-of-erastothenes-bools limit)))))

(defn all-primes [limit]
  "Return all primes up to limit (lazy)"
  (sieve-of-erastothenes-ints limit))

(defn divisible-by? [a divisors]
  (true? (some #(zero? (mod a %)) divisors)))

; primes cannot be written efficiently as a function, because
; it needs to look back on the whole sequence. contrast with
; fibs and powers-of-2 which only need a fixed buffer of 1 or 2
; previous values.
(def primes
  (concat 
   [2 3 5 7]
   (lazy-seq
    (let [primes-from
	  (fn primes-from [n [f & r]]
	    (if (some #(zero? (rem n %))
		      (take-while #(<= (* % %) n) primes))
	      (recur (+ n f) r)
	      (lazy-seq (cons n (primes-from (+ n f) r)))))
	  wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
			6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
			2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel)))))
