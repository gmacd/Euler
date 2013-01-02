(ns Euler.e10)

; e10
(defn array-and
  "And each pair of items in seqs a and b"
  [a b]
  (map #(and %1 %2) a b))


(defn array-of-possible-primes [n limit]
  (let [lowerBound (Math/pow n 2)]
    (concat [false false]
      (for [i (range 2 (inc limit))]
        (if (< i lowerBound)
          true
          (not (zero? (rem i n))))))))


(defn next-n [n coll]
  (ffirst (filter second (drop (inc n) (map-indexed vector coll)))))


(defn sieve-of-erastothenes-bools [limit]
  (loop [n 2
         bools (array-of-possible-primes n limit)]
    (if (>= (Math/pow n 2) limit)
      bools
      (let [n2 (next-n n bools)]
      	(recur n2
               (array-and bools (array-of-possible-primes n2 limit)))))))


(defn sieve-of-erastothenes-ints [limit]
  (map first (filter second (map-indexed vector (sieve-of-erastothenes-bools limit)))))


(defn e10 [limit] (reduce + (sieve-of-erastothenes-ints limit)))
;142913828922 in 2.5 mins in repl
;(time (e10 2000000))
