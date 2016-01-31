#lang racket

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

#| Exercise 1.29.  Simpson's Rule is a more accurate method of numerical integration than the method 
illustrated above. Using Simpson's Rule, the integral of a function f between a and b is approximated
as

h/3 * (y[0] + 4y[1] + 2y[2] + 4y[3] + 2y[4] + ... + 2y[n-2] + 4y[n-1] + y[n])

where h = (b - a)/n, for some even integer n, and y[k] = f(a + kh). (Increasing n increases the 
accuracy of the approximation.) Define a procedure that takes as arguments f, a, b, and n and returns 
the value of the integral, computed using Simpson's Rule. Use your procedure to integrate cube 
between 0 and 1 (with n = 100 and n = 1000), and compare the results to those of the integral 
procedure shown above.
|#

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (term-4 x) (* 4 (y x)))
  (define (term-2 x) (* 2 (y x)))
  (define (plus-2 x) (+ x 2))
  (* (/ h 3)
     (+ (y 0)
        (y n)
        (sum term-4 1 plus-2 (- n 2))
        (sum term-2 2 plus-2 (- n 1)))))

(define (identity x) x)
(define (return-1 x) 1)

(simpson-integral return-1 0 1 100)
(simpson-integral return-1 0 1 1000)
(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1000)

#| Exercise 1.30.  The sum procedure above generates a linear recursion. The procedure can be 
rewritten so that the sum is performed iteratively. Show how to do this by filling in the missing 
expressions in the following definition:
|#

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (increment x) (+ x 1))

(sum-iter identity 1 increment 100)

#| Exercise 1.31.   
a. The sum procedure is only the simplest of a vast number of similar abstractions that can be 
captured as higher-order procedures. Write an analogous procedure called product that returns the 
product of the values of a function at points over a given range. Show how to define factorial in 
terms of product. Also use product to compute approximations to  using the formula

pi/4 = (2 * 4 * 4 * 6 * 6 * 8 ...)/(3 * 3 * 5 * 5 * 7 * 7)
|#

(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))))

(product-recursive identity 1 increment 5)

(define (add-2 x) (+ x 2))
(define (square x) (* x x))

(define (pi-approximation n)
  (define (double x) (* x 2))
  (define (numerator-term x) (- (+ x 2) (modulo x 2)))
  (define (denominator-term x) (+ x (modulo x 2) 1))
  (* 4 (/ (product-recursive numerator-term 1 increment n)
          (product-recursive denominator-term 1 increment n))))

(pi-approximation 1)
(pi-approximation 10)
(pi-approximation 100)
(pi-approximation 1000)
   
#| 
b.  If your product procedure generates a recursive process, write one that generates an iterative 
process. If it generates an iterative process, write one that generates a recursive process.
|#

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(product identity 1 increment 5)

#| Exercise 1.32.  a. Show that sum and product (exercise 1.31) are both special cases of a still 
more general notion called accumulate that combines a collection of terms, using some general 
accumulation function:

(accumulate combiner null-value term a next b)

Accumulate takes as arguments the same term and range specifications as sum and product, together 
with a combiner procedure (of two arguments) that specifies how the current term is to be combined 
with the accumulation of the preceding terms and a null-value that specifies what base value to use 
when the terms run out. Write accumulate and show how sum and product can both be defined as simple 
calls to accumulate.
|#

(define (accumulate-iter combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-iter combiner null-value term (next a) next b))))

(define (sum-new term a next b) (accumulate-iter + 0 term a next b))
(define (product-new term a next b) (accumulate-iter * 1 term a next b))

(sum-new identity 1 increment 100)
(product-new identity 1 increment 5)

#|
b. If your accumulate procedure generates a recursive process, write one that generates an iterative 
process. If it generates an iterative process, write one that generates a recursive process.
|#

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

#| Exercise 1.33.  You can obtain an even more general version of accumulate (exercise 1.32) by 
introducing the notion of a filter on the terms to be combined. That is, combine only those terms 
derived from values in the range that satisfy a specified condition. The resulting 
filtered-accumulate abstraction takes the same arguments as accumulate, together with an additional 
predicate of one argument that specifies the filter. Write filtered-accumulate as a procedure. 
Show how to express the following using filtered-accumulate:
|#

(define (filtered-accumulate combiner null-value predicate term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result 
                                 (if (predicate a)
                                     (term a)
                                     null-value)))))
  (iter a null-value))

#|
a. the sum of the squares of the prime numbers in the interval a to b (assuming that you have a 
prime? predicate already written)
|#

#| (from 1.2) |#
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (if (= n 1)
      #f
      (= n (smallest-divisor n))))

(define (sum-squares-prime a b) (filtered-accumulate + 0 prime? square a increment b))

(sum-squares-prime 1 10)
(sum-squares-prime 1 100)

#|
b. the product of all the positive integers less than n that are relatively prime to n (i.e., all 
positive integers i < n such that GCD(i,n) = 1)
|#

(define (product-relatively-prime n)
  (define (relatively-prime? i)
    (define (iter j)
      (cond ((> j i) #t)
            ((and (divides? j i) (divides? j n)) #f)
            (else (iter (+ j 1)))))
    (iter 2))
  (filtered-accumulate * 1 relatively-prime? identity 1 increment (- n 1)))

(product-relatively-prime 4)
(product-relatively-prime 6)
(product-relatively-prime 8)

#| Exercise 1.34.  Suppose we define the procedure

(define (f g)
  (g 2))

Then we have

(f square)
4

(f (lambda (z) (* z (+ z 1))))
6

What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.
|#

(define (f g)
  (g 2))

#| (f f) becomes (f 2), and (f 2) throws an error because f takes a procedure as argument and
2 is not a procedure |#

#| Exercise 1.35. Show that the golden ratio phi (section 1.2.2) is a fixed point of the 
transformation x -> 1 + 1/x, and use this fact to compute phi by means of the fixed-point procedure.

COME BACK TO THIS ONCE YOU HAVE INTERNET
|#

#| Exercise 1.36. Modify fixed-point so that it prints the sequence of approximations it generates, 
using the newline and display primitives shown in exercise 1.22. Then find a solution to xx = 1000 
by finding a fixed point of x   log(1000)/log(x). (Use Scheme's primitive log procedure, which 
computes natural logarithms.) Compare the number of steps this takes with and without average 
damping. (Note that you cannot start fixed-point with a guess of 1, as this would cause division by 
log(1) = 0.)
|#

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess num-guesses)
    (display num-guesses)
    (display ": ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ num-guesses 1)))))
  (try first-guess 1))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)

(define (average x y) (/ (+ x y) 2))

(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2.0)

#| Exercise 1.37.  a. An infinite continued fraction is an expression of the form

f = N[1] / (D[1] + (N[2] / (D[2] + (N[3] / (D[3] + ...)))))

As an example, one can show that the infinite continued fraction expansion with the Ni and the Di 
all equal to 1 produces 1/, where  is the golden ratio (described in section 1.2.2). One way to 
approximate an infinite continued fraction is to truncate the expansion after a given number of 
terms. Such a truncation -- a so-called k-term finite continued fraction -- has the form

f = N[1] / (D[1] + (N[2] / (... + (N[k] / D[k]))))

Suppose that n and d are procedures of one argument (the term index i) that return the Ni and Di of 
the terms of the continued fraction. Define a procedure cont-frac such that evaluating 
(cont-frac n d k) computes the value of the k-term finite continued fraction. Check your procedure 
by approximating 1/phi using

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           k)

for successive values of k. How large must you make k in order to get an approximation that is 
accurate to 4 decimal places?
|#

(define (cont-frac n d k)
  (define (iter k result)
    (if (= k 0)
        result
        (iter (- k 1) (/ (n k) (+ (d k) result)))))
  (iter k 0))

(define phi (/ (+ (sqrt 5) 1) 2))

(define (find-phi)
  (define (iter k)
    (let ((v (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)))
      (display k)
      (display ": ")
      (display v)
      (newline)
      (cond ((and (not (= (get-n-digits v 4) (get-n-digits (/ 1 phi) 4))) 
                  (< k 100)) 
             (iter (+ k 1))))))
  (iter 1))

#| COME BACK TO THIS |#
(define (get-n-digits x n)
  x)

(/ 1 phi)
(find-phi)

#| b. If your cont-frac procedure generates a recursive process, write one that generates an 
iterative process. If it generates an iterative process, write one that generates a recursive process.
|#

(define (cont-frac-rec n d k)
  (define (iter-from i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i)
           (+ (d i) (iter-from (increment i))))))
  (iter-from 1))

#| Exercise 1.38.  In 1737, the Swiss mathematician Leonhard Euler published a memoir De Fractionibus 
Continuis, which included a continued fraction expansion for e - 2, where e is the base of the 
natural logarithms. In this fraction, the Ni are all 1, and the Di are successively 
1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program that uses your cont-frac procedure from 
exercise 1.37 to approximate e, based on Euler's expansion.
|#

(+ 2 (cont-frac (lambda (x) 1.0)
                (lambda (x)
                  (if (= (modulo x 3) 2)
                      (* (/ (+ x 1) 3) 2)
                      1))
                20))

#| Exercise 1.39.  A continued fraction representation of the tangent function was published in 1770 
by the German mathematician J.H. Lambert:

tan(x) = x / (1 - (x^2 / (3 - (x^3 / (5 - ...)))))

where x is in radians. Define a procedure (tan-cf x k) that computes an approximation to the tangent 
function based on Lambert's formula. K specifies the number of terms to compute, as in exercise 1.37.
|#

#| COME BACK TO THIS |#
(define (tan-cf x k)
  0)


