#lang racket

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

#| Exercise 2.1.  Define a better version of make-rat that handles both positive and negative 
arguments. Make-rat should normalize the sign so that if the rational number is positive, both the 
numerator and denominator are positive, and if the rational number is negative, only the numerator 
is negative.
|#

(define (make-rat n d)
  (let ((g (gcd n d))
        (n-sign (if (xor (< n 0) (< d 0)) (* -1 (abs n)) (abs n))))
    (cons (/ n-sign g) (/ (abs d) g))))

(print-rat (make-rat 1 5))
(print-rat (make-rat -1 -5))
(print-rat (make-rat -1 5))
(print-rat (make-rat 1 -5))

#| Exercise 2.2.  Consider the problem of representing line segments in a plane. Each segment is 
represented as a pair of points: a starting point and an ending point. Define a constructor 
make-segment and selectors start-segment and end-segment that define the representation of segments 
in terms of points. Furthermore, a point can be represented as a pair of numbers: the x coordinate 
and the y coordinate. Accordingly, specify a constructor make-point and selectors x-point and 
y-point that define this representation. Finally, using your selectors and constructors, define a 
procedure midpoint-segment that takes a line segment as argument and returns its midpoint (the point 
whose coordinates are the average of the coordinates of the endpoints). To try your procedures, 
you'll need a way to print points:
|#

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment a b)
  (cons a b))
(define (start-segment x)
  (car x))
(define (end-segment x)
  (cdr x))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (midpoint-segment s)
  (make-point (average (x-point (start-segment s)) (x-point (end-segment s)))
              (average (y-point (start-segment s)) (y-point (end-segment s)))))

(define (average a b)
  (/ (+ a b) 2))

(print-point (midpoint-segment (make-segment (make-point 1.0 1.0) (make-point 5.0 4.0))))

#| Exercise 2.3.  Implement a representation for rectangles in a plane. (Hint: You may want to make 
use of exercise 2.2.) In terms of your constructors and selectors, create procedures that compute the 
perimeter and the area of a given rectangle. Now implement a different representation for rectangles. 
Can you design your system with suitable abstraction barriers, so that the same perimeter and area 
procedures will work using either representation?
|#

(newline)

(define (make-rect p1 p2 p3 p4)
  (cons (cons p1 p2) (cons p3 p4)))
(define (p1 r)
  (car (car r)))
(define (p2 r)
  (cdr (car r)))
(define (p3 r)
  (car (cdr r)))
(define (p4 r)
  (cdr (cdr r)))

(define (perimeter r)
  (* 2 (+ (distance (p1 r) (p2 r))
          (distance (p2 r) (p3 r)))))

(define (area r)
  (* (distance (p1 r) (p2 r))
     (distance (p2 r) (p3 r))))

(define (distance p1 p2)
  (sqrt (+ (square (- (x-point p1) (x-point p2)))
           (square (- (y-point p1) (y-point p2))))))

(define (square x) (* x x))

(define (seg-length s)
  (distance (start-segment s) (end-segment s)))

(define rect1 
  (make-rect
   (make-point 0.0 0.0)
   (make-point 3.0 0.0)
   (make-point 3.0 5.0)
   (make-point 0.0 5.0)))

(perimeter rect1)
(area rect1)
 
#| Exercise 2.4.  Here is an alternative procedural representation of pairs. For this 
representation, verify that (car (cons x y)) yields x for any objects x and y.
|#
#| COME BACK TO THIS
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))
|#
#| What is the corresponding definition of cdr? (Hint: To verify that this works, make use of the 
substitution model of section 1.1.5.)
|#

#| 
(define (cdr z)
  (z (lambda (p q) q)))
|#

#| Exercise 2.5.  Show that we can represent pairs of nonnegative integers using only numbers and 
arithmetic operations if we represent the pair a and b as the integer that is the product 2^a * 3^b. 
Give the corresponding definitions of the procedures cons, car, and cdr.
|#

#| COME BACK TO THIS |#
(define (int-cons a b)
  0)

#| Exercise 2.6.  In case representing pairs as procedures wasn't mind-boggling enough, consider 
that, in a language that can manipulate procedures, we can get by without numbers (at least insofar 
as nonnegative integers are concerned) by implementing 0 and the operation of adding 1 as
|#

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

#| This representation is known as Church numerals, after its inventor, Alonzo Church, the logician 
who invented the lambda calculus.

Define one and two directly (not in terms of zero and add-1). (Hint: Use substitution to evaluate 
(add-1 zero)). Give a direct definition of the addition procedure + (not in terms of repeated 
application of add-1).
|#    

(define one
  (lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) y)) f) x)))))

(define two
  (lambda (f) (lambda (x) (f (((lambda (g) (lambda (y) (g (((lambda (h) (lambda (z) z)) g) y)))) f) x)))))


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (display-interval i)
  (newline)
  (display (lower-bound i))
  (display "->")
  (display (upper-bound i)))

#| Exercise 2.7.  Alyssa's program is incomplete because she has not specified the implementation of 
the interval abstraction. Here is a definition of the interval constructor:
|#

(define (make-interval a b) (cons a b))

#| Define selectors upper-bound and lower-bound to complete the implementation. |#

(define (upper-bound i)
  (max (car i) (cdr i)))
(define (lower-bound i)
  (min (car i) (cdr i)))

#| Exercise 2.8.  Using reasoning analogous to Alyssa's, describe how the difference of two 
intervals may be computed. Define a corresponding subtraction procedure, called sub-interval.

A: x - y It should be between lower(x) - upper(y) and upper(x) - lower(y)
|#

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(display-interval
 (sub-interval
  (make-interval 10 20)
  (make-interval 5 10)))

#| Exercise 2.9.  The width of an interval is half of the difference between its upper and lower 
bounds. The width is a measure of the uncertainty of the number specified by the interval. For some 
arithmetic operations the width of the result of combining two intervals is a function only of the 
widths of the argument intervals, whereas for others the width of the combination is not a function 
of the widths of the argument intervals. Show that the width of the sum (or difference) of two 
intervals is a function only of the widths of the intervals being added (or subtracted). Give 
examples to show that this is not true for multiplication or division.

A: 
width(i) = (1/2) * (upper(i) - lower(i))
width(i1 + i2) = (1/2) * (upper(i1 + i2) - lower(i1 + i2))
lower(i1 + i2) = lower(i1) + lower(i2)
upper(i1 + i2) = upper(i1) + upper(i2)
=> width(i1 + i2) = (1/2) * (upper(i1) + upper(i2) - lower(i1) - lower(i2))
=> width(i1 + i2) = (1/2) * (upper(i1) - lower(i1)) + (1/2) * (upper(i2) - lower(i2))
=> width(i1 + i2) = width(i1) + width(i2)

not true for multiplication and division:
i1 = 1->3
i2 = 1->11
i3 = 11->21
width(i1) = 1
width(i2) = 5
width(i3) = 5

multiplication:
i1 * i2 = 1->33
width(i1 * i2) = 16
i1 * i3 = 11->63
width(i1 * i3) = 26

division:
i2 / i1 = 1->11 * ((1/3)->1)
        = (1/3)->11
width(i2 / i1) = 5 + 1/3
i3 / i1 = 11->21 * ((1/3)->1)
        = (3 + 2/3)->21
width(i3 / i1) = 8 + 2/3

|#

#| Exercise 2.10.  Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and 
comments that it is not clear what it means to divide by an interval that spans zero. Modify 
Alyssa's code to check for this condition and to signal an error if it occurs.
|#

(define (div-interval x y)
  (if (and (>= (upper-bound y) 0) (<= (lower-bound y) 0))
      (error "Possible divide by 0")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

#|(div-interval (make-interval 10 20) (make-interval -5 5))|#

#| Exercise 2.11.  In passing, Ben also cryptically comments: ``By testing the signs of the 
endpoints of the intervals, it is possible to break mul-interval into nine cases, only one of which 
requires more than two multiplications.'' Rewrite this procedure using Ben's suggestion.
|#

(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond 
      ((< ux 0)
       (cond
         ((< uy 0)
          (make-interval (* ux uy) (* lx ly)))
         ((< ly 0)
          (make-interval (* lx uy) (* lx ly)))
         (else
          (make-interval (* lx uy) (* ux ly)))))
      ((< lx 0)
       (cond
         ((< uy 0)
          (make-interval (* ux ly) (* lx ly)))
         ((< ly 0)
          (make-interval (min (* lx uy) (* ux ly)) (max (* lx ly) (* ux uy))))
         (else
          (make-interval (* lx uy) (* ux uy)))))
      (else 
       (cond
         ((< uy 0)
          (make-interval (* ux ly) (* lx uy)))
         ((< ly 0)
          (make-interval (* ux ly) (* ux uy)))
         (else
          (make-interval (* lx ly) (* ux uy))))))))
            
(display-interval
 (mul-interval
  (make-interval -1 -2)
  (make-interval -3 -4)))
(display-interval
 (mul-interval
  (make-interval -1 2)
  (make-interval -3 4)))
(display-interval
 (mul-interval
  (make-interval -1 -2)
  (make-interval 3 -4)))


(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

#| Exercise 2.12.  Define a constructor make-center-percent that takes a center and a percentage 
tolerance and produces the desired interval. You must also define a selector percent that produces 
the percentage tolerance for a given interval. The center selector is the same as the one shown 
above.
|#

(define (make-center-percent c p)
  (make-interval (- c (abs (* c (/ p 100.0)))) (+ c (abs (* c (/ p 100.0))))))
(define (percent-tolerance i)
  (* (/ (width i) (center i)) 100.0))

(display-interval (make-center-percent 100 10))
(newline)
(percent-tolerance (make-interval 95 105))

#| Exercise 2.13.  Show that under the assumption of small percentage tolerances there is a simple 
formula for the approximate percentage tolerance of the product of two intervals in terms of the 
tolerances of the factors. You may simplify the problem by assuming that all numbers are positive.
|#


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

#| Exercise 2.14.  Demonstrate that Lem is right. Investigate the behavior of the system on a 
variety of arithmetic expressions. Make some intervals A and B, and use them in computing the 
expressions A/A and A/B. You will get the most insight by using intervals whose width is a small 
percentage of the center value. Examine the results of the computation in center-percent form (see 
exercise 2.12).
|#

(define interval-a (make-center-percent 680 5))
(define interval-b (make-center-percent 800 8))

(define (display-center-percent i)
  (newline)
  (display (center i))
  (display ", ")
  (display (percent-tolerance i))
  (display "%"))

(display-center-percent (par1 interval-a interval-b))
(display-center-percent (par2 interval-a interval-b))

#| Exercise 2.15.  Eva Lu Ator, another user, has also noticed the different intervals computed by 
different but algebraically equivalent expressions. She says that a formula to compute with 
intervals using Alyssa's system will produce tighter error bounds if it can be written in such a 
form that no variable that represents an uncertain number is repeated. Thus, she says, par2 is a 
``better'' program for parallel resistances than par1. Is she right? Why?

A: That seems correct. When multiple operations are performed on an interval, and those operations 
are then combined, the errors are combined as well?

|#


