(ns Euler.e1
  (:use Euler.core))

(defn e1 [xs limit]
  "Return sum of all multiples of xs between 1 and limit-1"
  (reduce + (multiples-in-range xs limit)))

(def e2 (sum-of-fibs 4000000))

(def e3 (largest-prime-factor 600851475143))

(defn e4 []
  (apply max
         (for [a (range 999 99 -1)
               b (range (+ a 1) 99 -1)
               :let [c (* a b)]
               :when (palindrome? c)]
           c)))

(defn e6 [limit]
  (- (square-of-sum limit) (sum-of-squares limit)))


                                        ; e7
                                        ; Hacky mess of magic numbers based on code here:
                                        ; http://clojuredocs.org/clojure_core/clojure.core/reduce
                                        ;(nth (reduce
                                        ; (fn [primes number]
                                        ;   (if (some zero? (map (partial mod number) primes))
                                        ;     primes
                                        ;     (conj primes number)))
                                        ; [2] (take 60000 (iterate #(+ 2 %) 3))) 10000)

(defn e8 [s]
  (let [nums (map #(Character/digit % 10) (seq s))]
    (apply max (for [group (partition 5 1 nums)
                     :let [sum (apply * group)]]
                 sum))))

(e8 "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")



                                        ; e9
                                        ;(def pows
                                        ;  (into-array
                                        ;    (for [i (range 0 1001)
                                        ;          :let [p (int (Math/pow i 2))]] p)))

                                        ;(def set1
                                        ;  (into-array
                                        ;    (for [a (range 1 1001)
                                        ;          b (range (inc a) 1001)
                                        ;          c (range (inc b) 1001)
                                        ;          :when (= 1000 (+ a b c))]
                                        ;      [(nth pows a) (nth pows b) (nth pows c)])))

                                        ;(def set1sq
                                        ;  (first (filter #(= (+ (nth % 0) (nth % 1)) (nth % 2)) set1)))

                                        ;(def triplet (map #(int (Math/sqrt %)) set1sq))
                                        ;(def soln (reduce * triplet))

                                        ; Combined version
(defn e9 []
  (let [set1 (for [a (range 1 1001)
                   b (range (inc a) 1001)
                   c (range (inc b) 1001)
                   :when (= 1000 (+ a b c))]
               [(Math/pow a 2) (Math/pow b 2) (Math/pow c 2)])
        set1sq (first (filter #(= (+ (nth % 0) (nth % 1)) (nth % 2)) set1))
        triplet (map #(int (Math/sqrt %)) set1sq)]
    (reduce * triplet)))

                                        ;(def alglim 1001)
