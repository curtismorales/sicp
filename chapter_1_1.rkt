#lang racket


#| Exercise 1.1.  Below is a sequence of expressions. What is the result printed by the interpreter 
in response to each expression? Assume that the sequence is to be evaluated in the order in which it 
is presented.
---
10
10

(+ 5 3 4)
12

(- 9 1)
8

(/ 6 2)
3

(+ (* 2 4) (- 4 6))
6

(define a 3)
(define b (+ a 1))

(+ a b (* a b))
19

(= a b)
#f

(if (and (> b a) (< b (* a b)))
    b
    a)
4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
16

(+ 2 (if (> b a) b a))
6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
16

|#

#| Exercise 1.2. Translate the following expression into prefix form |#
(/ (+ 5 (/ 1 2) (- 2 (- 3 (+ 6 (/ 1 5))))) (* 3 (- 6 2) (- 2 7)))

#| Exercise 1.3. Define a procedure that takes three numbers as arguments and returns the sum of the 
squares of the two larger numbers. |#
(define (mymin . xs)
  (if (empty? xs) 
      '()
      (let mymin-helper ([x (car xs)] [rest (cdr xs)])
        (if (empty? rest)
            x
            (if (< x (car rest))
                (mymin-helper x (cdr rest))
                (mymin-helper (car rest) (cdr rest)))))))

(define (remove-from-list-once x xs)
  (if (empty? xs)
      xs
      (if (= x (car xs))
          (cdr xs)
          (cons (car xs) (remove-from-list-once x (cdr xs))))))

(define (square x) (* x x))

(define (sum xs)
  (if (empty? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

(define (mymap f xs)
  (if (empty? xs)
      '()
      (cons (f (car xs)) (mymap f (cdr xs)))))

(define (sum-squares-largest a b c)
  (sum (mymap square (remove-from-list-once (mymin a b c) (list a b c)))))
    
(sum-squares-largest 4 5 6)

(define (sum-squares-largest2 a b c)
  (if (or (> a b) (= a b)) 
      (if (> b c)
          (+ (* a a) (* b b))
          (+ (* a a) (* c c)))
      (if (> a c)
          (+ (* a a) (* b b))
          (+ (* b b) (* c c)))))
   
(sum-squares-largest2 4 5 6)

#| Exercise 1.4.  Observe that our model of evaluation allows for combinations whose operators are 
compound expressions. Use this observation to describe the behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
---
If b is positive, apply the + operand to a and b; otherwise, apply the - operand. In other words, add
b to a if b is positive, and add -b to a otherwise. Or, add |b| to a.

|#

#| Exercise 1.5.  Ben Bitdiddle has invented a test to determine whether the interpreter he is faced 
with is using applicative-order evaluation or normal-order evaluation. He defines the following two 
procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

Then he evaluates the expression

(test 0 (p))

What behavior will Ben observe with an interpreter that uses applicative-order evaluation? What 
behavior will he observe with an interpreter that uses normal-order evaluation? Explain your answer. 
(Assume that the evaluation rule for the special form if is the same whether the interpreter is using
normal or applicative order: The predicate expression is evaluated first, and the result determines 
whether to evaluate the consequent or the alternative expression.)
---
normal-order:
test is defined as
(if (= x 0)
    0
    y)

x is 0
y is (p)

so it becomes:
(if (= 0 0)
    0
    (p))

which returns 0

applicative-order:
evaluate the arguments:
(p) becomes (p) in an infinite loop

|#

#| Exercise 1.6.  Alyssa P. Hacker doesn't see why if needs to be provided as a special form. ``Why 
can't I just define it as an ordinary procedure in terms of cond?'' she asks. Alyssa's friend Eva Lu 
Ator claims this can indeed be done, and she defines a new version of if:

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

Eva demonstrates the program for Alyssa:

(new-if (= 2 3) 0 5)
5

(new-if (= 1 1) 0 5)
0

Delighted, Alyssa uses new-if to rewrite the square-root program:

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

What happens when Alyssa attempts to use this to compute square roots? Explain. 
---
Infinite loop? But why not the if?
|#

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x)
                     x)))

(define (new-sqrt x)
  (new-sqrt-iter 1.0 x))

#| Exercise 1.7.  The good-enough? test used in computing square roots will not be very effective for
finding the square roots of very small numbers. Also, in real computers, arithmetic operations are 
almost always performed with limited precision. This makes our test inadequate for very large 
numbers. Explain these statements, with examples showing how the test fails for small and large 
numbers. An alternative strategy for implementing good-enough? is to watch how guess changes from 
one iteration to the next and to stop when the change is a very small fraction of the guess. Design 
a square-root procedure that uses this kind of end test. Does this work better for small and large 
numbers?
--
small numbers:
A difference of 0.001 is crappy if your number is much smaller than that. Every number will end
dividing by 2 until we reach just under sqrt(0.001)
For example, (sqrt 0.0000000000001) gives 0.03125000000106562

large numbers:
For very large numbers, we only keep track of the first N digits. If our guess only has the first N
digits, we have to get lucky and hit a number that, when squared, produces exactly the N digits in
the number we're trying to find the square root of. We could end up in an infinite loop.

Although it seems to work in racket?

|#

(define (better-good-enough? guess x)
  (< (abs (- (square guess) x)) (/ x 1000000.0)))

(define (better-sqrt-iter guess x)
  (if (better-good-enough? guess x)
      guess
      (better-sqrt-iter (improve guess x)
                 x)))

(define (better-sqrt x)
  (better-sqrt-iter 1.0 x))

(better-sqrt 0.000000000001)

#| Exercise 1.8.  Newton's method for cube roots is based on the fact that if y is an approximation 
to the cube root of x, then a better approximation is given by the value

(x/y^2 + 2y)/3 

Use this formula to implement a cube-root procedure analogous to the square-root procedure. (In 
section 1.3.4 we will see how to implement Newton's method in general as an abstraction of these 
square-root and cube-root procedures.) |#

(define (cube x) (* x x x))

(define (cube-good-enough? guess x)
  (< (abs (- (cube guess) x)) (/ x 1000000.0)))

(define (cube-improve guess x)
  (/ (+ (/ x (square guess)) (* 2.0 guess)) 3.0))

(define (cubert-iter guess x)
  (if (cube-good-enough? guess x)
      guess
      (cubert-iter (cube-improve guess x) x)))

(define (cubert x)
  (cubert-iter 1.0 x))

(cubert 64)
