(ns project-euler.base
  (:require [clojure.math.numeric-tower :as math]))

(defn sum
  "Take the sum of a collection"
  ([xx] (sum xx 0))
  ([xx total]
    (if (empty? xx)
      total
      (recur (rest xx) (+ (first xx) total)))))

(defn prod
  "Takes the product of a collection"
  ([xx] (prod xx 1))
  ([xx total]
    (if (empty xx)
      total
      (recur (rest xx) (* (first xx) total)))))

(defn sqrt [n]
  "Find the square root"
  (math/sqrt n))

(defn square [n]
  "Squares n"
  (* n n))

(defn cube [n]
  "Cubes n"
  (* n n n))

(defn expo
  "Takes a to the power of n"
  ([a n] (expo a n 1))
  ([a n total]
    (if (zero? n)
      total
      (recur a (- n 1) (* total a)))))

(def fibo
  "The fibonacci sequence"
  (lazy-cat [0 1] (map + fibo (rest fibo))))

(defn divisors [n]
  "Find all the divisors of n, returns a vector and includes n"
  (conj
    (filterv
      (comp zero? (partial rem n))
        (range 1 (inc (/ n 2)))) n))

(defn divisors-no-n [n]
  "Find all the divisors of n, returns a vector and does not have n at the end"
  (filterv
    (comp zero? (partial rem n))
      (range 1 (inc (/ n 2)))))

(def certainty 5)

(defn prime? [n]
  "Checks if n is prime or not"
  (.isProbablePrime (BigInteger/valueOf n) certainty))

(defn triangle-number [n]
  "Finds the nth triangle-number"
  (sum (range (inc n))))

(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
     (let [new-sum (+ sum n)]
       (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def triangle-numbers (tri*))

(defn fact
  "Take the factorial of x"
  ([x] (fact x 1))
  ([x total]
    (if (= x 1)
      total
      (recur (- x 1) (*' total x)))))

(defn mean [xx]
  "Find the mean of a set"
  (/ (sum xx) (count xx)))

(defn prob-map [xx]
  "Creates a probability-map for a discrete set"
  (let [total (count xx)]
    (into {}
      (map
        (fn [k]
          [(keyword (str k))
          (/ (count (filter #( = % k) xx)) total)])
           xx))))

(defn pmf [xx n]
  "Find the probability of n in collection"
  (get (prob-map xx) (keyword (str n))))

(defn expected-value [xx]
  "Find the expected value of a collection"
  (sum
    (map
      #(double (* (pmf xx %) %))
        (into #{} xx))))

(defn variance [xx]
  "Variance of a set"
  (-
    (expected-value (mapv square xx))
    (square (expected-value xx))))

(defn std [xx]
  "Find the standard deviation of a set"
  (sqrt (variance xx)))

(defn explode-to-digits [number]
  "Turn a string number into a sequence of its digits"
      (map #(Character/digit % 10) (str number)))
