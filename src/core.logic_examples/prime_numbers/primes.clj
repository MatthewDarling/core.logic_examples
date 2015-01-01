(ns core.logic-examples.prime-numbers.primes
  (:require [clojure.math.numeric-tower :refer :all]
            [clojure.core.match :refer [match]]
            [clojure.core.logic :as logic]
            [clojure.core.logic.fd :as fd])
  (:import [java.lang Math]))

;;; Binary quadratic equation solvers
;; The Sieve of Atkin is expressed mathematically by dividing every n
;; by 12, and depending on the remainder, solving a specific
;; irreducible binary quadratic form
;;
;; core.logic has an eq macro in its namespace for finite domains,
;; which can actually solve these equations for us
;;
;; Any number, n, for which there are an uneven number of solutions to
;; these equations is a prime number
(defn remainder-1-or-5
  "Solve for 4x^2 + y^2"
  [stopping-point n]
  (logic/run* [q]
    (logic/fresh [x y]
      (logic/== q [x y])
      (fd/in x y (fd/interval 1 stopping-point))
      (fd/eq (= (+ (* 4 x x) (* y y)) n)))))

(defn remainder-7
  "Solve for 3x^2 + y^2"
  [stopping-point n]
  (logic/run* [q]
    (logic/fresh [x y]
      (logic/== q [x y])
      (fd/in x y (fd/interval 1 stopping-point))
      (fd/eq (= (+ (* 3 x x) (* y y)) n)))))

(defn remainder-11
  "Solve for 3x^2 - y^2, where x > y"
  [stopping-point n]
  (logic/run* [q]
    (logic/fresh [x y]
      (logic/== q [x y])
      (fd/in x y (fd/interval 1 stopping-point))
      (fd/> x y)
      (fd/eq (= (- (* 3 x x) (* y y)) n)))))

(defn prime?
  "Match on the remainder of n/12, and solve the appropriate
  equation. If the number of results returned is odd, the number is
  prime."
  [stopping-point n]
  (let [results (match [(mod n 12)]
                       [(:or 1 5)] (remainder-1-or-5 stopping-point n)
                       [(:or 7)] (remainder-7 stopping-point n)
                       [(:or 11)] (remainder-11 stopping-point n)
                       :else nil)]
    (when (-> results count odd?) n)))

;;; Implementation
(defn remove-multiples
  "Any numbers which are a multiple of a smaller prime number are
  obviously not prime, so we have to remove them."
  [prime-set limit]
  (let [sorted-primes (sort prime-set)
        vals-to-remove (fn [n]
                         (let [n2 (* n n)]
                           (range n2 limit n2)))]
    (reduce #(apply disj %1
                    (vals-to-remove %2))
            prime-set sorted-primes)))

(defn get-primes
  "For all numbers n between 1 and limit, check for solutions to the
  Atkin binary quadratic forms and use these results to decide which
  numbers are prime."
  [limit]
  (let [stopping-point (int (java.lang.Math/ceil (sqrt limit)))
        initial-prime-set (reduce conj #{2 3 5}
                                  (keep (partial prime? stopping-point)
                                        (range 1 (inc limit))))]
    (remove-multiples initial-prime-set limit)))

(defn sum-primes
  "Returns the sum of all primes that are equal to or less than the
  provided value.  Primes are 2, 3, 5, 7, 11, ..."
  [max]
  (if (pos? max)
    (apply + (get-primes max))
    0))
