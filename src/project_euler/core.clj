(ns project-euler.core
  (:require [clojure.string :as str]
            [project-euler.base :as base]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn pe1 [n]
  "Finds the sum of a set of number mod 3 and 5 below n"
  (base/sum (filter #(zero? (min (mod % 3) (mod % 5))) (range n))))

(def fibo
  "Defines the fibonacci sequence"
  (lazy-cat [0 1] (map + fibo (rest fibo))))

(defn pe2 [n]
  "Find the sum of fibonacci numbers up to n times"
  (base/sum (take-while (partial >= n)
                        (filter even? fibo))))

(defn getprime [num cur limit]
  "Finds the heighst prime"
  (if (> cur limit)
      num
      (if (= num cur)
        num
        (if (zero? (mod num cur))
          (getprime (/ num cur ) cur limit)
          (getprime num (inc cur) limit)))))

(defn pe3 [num]
  "Finds all the primes of the number given"
  (let [limit (long (Math/sqrt num))]
    (getprime num 2 limit)))

(defn palindrome? [string]
  "Checks to see if current string is a palindrome"
  (= (reverse (str string)) (seq (str string))))

(defn pe4 [num1 num2]
  "Finds the largest palindrome in a given range"
  (apply max
    (filter palindrome?
        (for
          [a (range num1 num2)
           b (range num1 num2)]
           (* a b)))))

(defn gcd [a b]
  "Finds the greatest common demonenator of two numbers for use in lcm"
  (if (zero? b) a (recur b (mod a b))))

(defn lcm [a b]
  "Finds the lowest common multiple of two numbers"
  (/ (* a b) (gcd a b)))

(defn pe5 [start end]
  "Finds the lowest common multple of all the bumber in a range"
  (reduce #(lcm %1 %2) (range start end)))

(defn square [x]
  "Finds the square of x"
  (* x x))

(defn sum-of-squares [n]
  "Finds the sum of squares from 1 to n"
    (base/sum (map square (take n (iterate inc 1)))))

(defn square-of-sums [n]
  "Finds the square of the sum of 1 to n"
  (square (base/sum (take n (iterate inc 1)))))

(defn pe6 [n]
  "Finds the difference of the square of sums vs sum of squares"
  (- (square-of-sums n) (sum-of-squares n)))

(defn pe7 [n]
  "Get the nth prime number"
   (nth (take n (filter base/prime? (iterate inc 2))) (- n 1)))

(defn explode-to-digits [number]
  "Turn a string number into a sequence of its digits"
      (map #(Character/digit % 10) (str number)))

(defn pe8 [xx n]
  "Given a sequence find the greatest product of n sequential numbers"
  (apply max (map base/prod (partition n 1 (explode-to-digits xx)))))

(def fibo-sq
  (lazy-cat []))

(def not-nil?
  "My own not nil test"
  (complement nil?))

(defn pe9-filt-fn [x]
  "Filter for pe9"
  (= 1000 (base/sum x)))

(defn pe9 [n]
  "This is stupid but worked"
    (filter pe9-filt-fn (filter not-nil? (for [x (range 1 n) y (range 1 n) z (range 1 n)]
        (if (= (* z z) (+ (* x x) (* y y))) [x y z])))))

(def certainty 5)

(defn prime? [n]
  "Test if n is a prime"
  (.isProbablePrime (BigInteger/valueOf n) certainty))

(defn pe10 [n]
  "Finds the sum of all the primes lower than n"
  (base/sum (filter prime? (range 0 n))))

(defn triangle-number [n]
  "Generates the nth triangle number"
  (base/sum (range 1 (+ n 1))))

(def triangles
  "List of the triangle numbers"
  (map triangle-number (iterate inc 1)))

(defn divisors [n]
  "Find all the divisors of n"
  (conj (filterv (comp zero? (partial rem n)) (range 1 (inc (/ n 2)))) n))

(def t-divs
  "All the divisors of triangle numbers"
  (map divisors (take triangles)))

(defn collatz [n]
  "The collatz sequence for n"
  (if (= n 1) '(1)
    (cons n
      (cond
        (even? n) (collatz (/ n 2))
        (= 1 n) 1
        (odd? n) (collatz (+ (* 3 n) 1))))))

(defn collatz-seqs [n]
  (map collatz (take-while (partial >= n) (iterate inc (- n (/ n 5))))))

(defn pe14 [n]
  "Find the count of the collatz sequence for n"
  (def seq-main (collatz-seqs n))
  (def base (map count seq-main))
  (def test (apply max base))
  (def id (.indexOf base test))
  (first (nth seq-main id)))

(defn grid [x y]
  (+ (* x (+ y 1)) (* (+ x 1) y)))

(defn a-to-the-n [a n]
  (base/prod (take n (repeat a))))

(defn pe16 [a n]
  "Given a sequence find the greatest product of n sequential numbers"
  (base/sum (explode-to-digits (a-to-the-n a n))))

(def fib-seq
  ((fn rfib [a b]
    (lazy-seq (cons a (rfib b (+ a b)))))
    0 1))

(defn fs1 [q' p']
  "Generate the three children fibo-squares"
  [[(- p' q') q' p' (- (* 2 p') q')]
  [q' p' (+ q' p') (+ (* 2 q') p')]
  [p' q' (+ p' q') (+ (* 2 p') q')]])

(def fibo-squares
  "A list of the fibonacci-squares to represent "
  ((fn fibo-square [q q' p p']
    (lazy-cat (cons [q q' p p'] (fs1 q' p')))) 1 1 2 3))

(defn fact-1 [x]
    (loop [n x f 1]
        (if (= n 1)
            f
            (recur (dec n) (*' f n)))))

(defn pe20 [x]
  "Find the sum of the digits of a factorial"
  (base/sum (explode-to-digits (base/fact x))))

(defn divisors-2 [n]
  "Find all the divisors of n"
  (filter (comp zero? (partial rem n)) (range 1 n)))

(defn sum-divs [n]
  "Sum the divisors"
  (base/sum (divisors-2 n)))

(defn amicable? [n]
  "Test to see if the number is amicable"
  (def test (sum-divs n))
  (if (and (= n (sum-divs test)) (not= test n)) true false))

(defn pe21 [x]
  "Find the sum of all the amicable numbers"
  (base/sum (filter amicable? (range x))))

(defn contains-value? [element coll]
    (boolean (some #(= element %) coll)))

(defn pe474 [n d]
  "Find the number of divisors of n with d as the last digit"
  (count
    (filter not-nil? (map
      #(if (contains-value? d %) %)
        (map
          #(into [] %)
            (map explode-to-digits (divisors n)))))))


(defn to-hash-map [x y]
  {(keyword x) y})

(def alphabet-map
  (into {}
    (map to-hash-map
      (map #(str %)
        (map char (range 97 123)))
      (range 1 27))))

(defn explode-to-letters [string]
  "Turn a string number into a sequence of its digits"
      (map str (seq string)))

(defn string-value [string]
  (base/sum
    (map #(get alphabet-map (keyword %))
      (explode-to-letters (str/lower-case string)))))

(defn file-to-list [file]
  "Takes a text file and turns it to a list of it's contents"
  (map #(str/replace % #"\"" "")
    (str/split (slurp file) #",")))

(defn file-to-values [file]
  "Takes in a file and turns it into a sequence letter sequences of the words"
  (map string-value
    (sort
      (file-to-list file))))

(defn pract [file]
  (let [test (sort (file-to-list file))]
    (map #(* (string-value %) (+ (.indexOf test %) 1)) test)))

(defn pe22 [file]
  "Project euler 22"
  (base/sum pract))
