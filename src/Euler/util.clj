(ns Euler.util)

(defn indexed
  "Returns a lazy sequence of [index, item] pairs, where items come
  from 's' and indexes count up from zero.

  (indexed '(a b c d))  =>  ([0 a] [1 b] [2 c] [3 d])"
  [s]
  (map vector (iterate inc 0) s))

(defn positions
  "Returns a lazy sequence containing the positions at which pred
   is true for items in coll."
  [pred coll]
  (for [[idx elt] (indexed coll) :when (pred elt)] idx))


(defn num-digits [a]
  (count (take-while #(> % 0) (iterate #(int (/ % 10.0)) a))))

(defn digit-at [a digit-idx]
  (let [num-digits (num-digits a)
        multiple-of-10 (int (Math/pow 10 (- num-digits digit-idx)))]
	  (mod (int (/ a multiple-of-10)) 10)))


(defn fac [n] (apply * (range 1 (inc n))))
