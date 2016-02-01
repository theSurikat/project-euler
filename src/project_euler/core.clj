(ns project-euler.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn pe1 [n]
  "Finds the sum of a set of number mod 3 and 5 below n"
  (apply + (filter #(zero? (min (mod % 3) (mod % 5))) (range n))))

(def fibo
  "Defines the fibonacci sequence"
  (lazy-cat [0 1] (map + fibo (rest fibo))))

(defn pe2 [n]
  "Find the sum of fibonacci numbers up to n times"
  (apply + (take-while (partial >= n)
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
    (apply + (map square (take n (iterate inc 1)))))

(defn square-of-sums [n]
  "Finds the square of the sum of 1 to n"
  (square (apply + (take n (iterate inc 1)))))

(defn pe6 [n]
  "Finds the difference of the square of sums vs sum of squares"
  (- (square-of-sums n) (sum-of-squares n)))

(defn pe7 [n]
  "Get the nth prime number"
   (nth (take n (filter prime? (iterate inc 2))) (- n 1)))

(defn explode-to-digits [number]
  "Turn a string number into a sequence of its digits"
      (map #(Character/digit % 10) (str number)))

(defn pe8 [xx n]
  "Given a sequence find the greatest product of n sequential numbers"
  (apply max (map #(apply * %) (partition n 1 (explode-to-digits xx)))))

(def fibo-sq
  (lazy-cat []))

(defrecord fibonacci-square
  "The structure of a fibonacci-square"
  [q q' p p' left middle right])

(def not-nil?
  "My own not nil test"
  (complement nil?))

(defn pe9-filt-fn [x]
  "Filter for pe9"
  (= 1000 (apply + x)))

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
  (apply + (filter prime? (range 0 n))))

(defn triangle-number [n]
  "Generates the nth triangle number"
  (apply + (range 1 (+ n 1))))

(def triangles
  "List of the triangle numbers"
  (map triangle-number (iterate inc 1)))

(defn divisors [n]
  "Find all the divisors of n"
  (filter (comp zero? (partial rem n)) (range 1 (+ n 1))))

(def t-divs
  "All the divisors of triangle numbers"
  (map divisors (take triangels)))

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
  (apply *' (take n (repeat a))))

(defn pe16 [a n]
  "Given a sequence find the greatest product of n sequential numbers"
  (apply + (explode-to-digits (a-to-the-n a n))))
