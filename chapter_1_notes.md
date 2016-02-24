# Chapter 1.1

Languages consist of

* primitive expressions
* means of combination
* means of data

Also, procedures and data

## 1.1.1 Expressions

An _expression_ can be a simple value, such as 10 or a complex
expression consisting of an _operator_ and 0 or more _operands_

The _arguments_ are the values of the operands

In Lisp, we place the operator on the left. For example:

```
(+ 1 2)
(* (+ 3 4) (- 7 5))
```

With more complicated expressions, we by convention indent the
expression for ease of reading, as in

```
(+ (* 2
      (+ (* 3 4)
         (+ 5 6)))
   (- 9 7))
```

## 1.1.2 Naming and the Environment

In Scheme we can name variables as with

```
(define size 2)
```

The memory that keeps track of name-object pairs is the _environment_
(specifically, in this case, the _global environment_)

## 1.1.3 Evaluating Combinations

To evaluate a combination, we must

"""
1. Evaluate the subexpressions of the combination
2. Apply the procedure that is the value of the leftmost subexpression
(the operator) to the arguments that are the values of the other 
subexpressions (the operands).
"""

`define` is a special form to which this rule does not apply. There
are some other special forms.

## 1.1.4 Compound Procedures

A procedure is defined with the following form:

```
(define (<name> <formal parameters>) <body>)
```

## 1.1.5 The Substitution Model for Procedure Application

"""
To apply a compound procedure to arguments, evaluate the body of the 
procedure with each formal parameter replaced by the corresponding 
argument.
"""

### Applicative order versus normal order

There are two methods that substituion can be applied to evaluate
combinations:

* applicative order:
  Each argument in the procedure is evaluated, and then the operator
  is evaluated and applied to the arguments
* normal order:
  First the operator is evaluated and expanded, then the the arguments
  are evaluated, following the same logic until the procedure is
  reduced to only primitive operators. Then each argument is evaluated

For example, suppose we define the procedure

```
(define (square x) (* x x))
```

and evaluate the expression

```
(square (square (+ 2 1)))
```

* applicative:
  ```
  (square (square (+ 2 1)))
  (square (square 3))
  (square (* 3 3))
  (square 9)
  (* 9 9)
  81
  ```
* normal:
  ```
  (square (square (+ 2 1)))
  (* (square (+ 2 1)) (square (+ 2 1)))
  (* (* (+ 2 1) (+ 2 1)) (* (+ 2 1) (+ 2 1)))
  (* (* 3 3) (* 3 3))
  (* 9 9)
  81
  ```

Lisp uses applicative-order evaluation

## 1.1.6 Conditional Expressions and Predicates

In Lisp, case analysis can be done with `cond`

The form of a `cond` expression is

```
(cond (<p1> <e1>) 
      (<p2> <e2>)
      ...
      (<pn> <en>)
      (else <em>))
```

Each `<p>` is a _predicate_ and each `<e>` is an expression.
The two in parentheses form a _clause_. `cond` evaluates to
the first expression whose corresponding predicate is true, or
if none are true, it evaluates to the expression after `else`
(`else` is optional)

Lisp also has `if`, `and`, `or`, and `not`

## 1.1.8 Procedures as Black-Box Abstractions

One of the key ideas in this chapter is that we can (and want
to) treat procedures as black boxes. For example, with the
`good-enough` function:

```
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
```

we don't need to know or care how `square` is implemented. We
treat `square` as an _abstraction_

### Local names and internal definitions

The parameter names of a procedure are local to the body of the
procedure. Additionally, we can define procedures within another
procedure. This binds the names of these procedures only within
the parent procedure.
