#lang racket

(require "grammar-macro.rkt")

(define-grammar eval-expr
  (terminals
   [Var var])
  (non-terminals
   [Lambda (lmbda lst) (lmbda lst)]
   [Filter (p lst)
           (filter (eval-expr p)
                   (eval-expr lst))]
   [Map (f lst)
        (map (eval-expr f)
             (eval-expr lst))]
   [Foldl (binop start lst)
          (foldl (eval-expr binop)
                 (eval-expr start)
                 (eval-expr lst))]
   [Add (left right)
        (+ (eval-expr left)
           (eval-expr right))]
   [Mul (left right)
        (* (eval-expr left)
           (eval-expr right))]
   [Ite (bexpr then-expr else-expr)
        (if (eval-bexpr bexpr)
            (eval-expr then-expr)
            (eval-expr else-expr))]))

(define-grammar eval-bexpr
  (terminals)
  (non-terminals
   [Greater (left right)
            (if (> (eval-expr left) (eval-expr right))
                #true
                #false)]
   (Conjunction (left right)
                (and (eval-bexpr left) (eval-bexpr right)))))

; build expression that is composed from mentioned grammar
(define var 5)
(define expr (Mul (Add 1 Var) 13))
(displayln expr)


(eval-expr expr)



(define (dropmins x)
  (map
   (λ (y)
     (filter
      (λ (z)
        (foldl
         (λ (w t)
             (or t (< w z)))
         #f y))
      y))
   x))


(dropmins (list (list 1 2 3) (list 0 1 2) (list 543 534 654)))
