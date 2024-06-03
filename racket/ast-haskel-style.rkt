#lang racket

;; Define the grammar (AST)
(struct Num (n) #:transparent)
(struct Mul (left right) #:transparent)
(struct Add (left right) #:transparent)

;; Define evaluation rules
(define (evalExpr expr)
  (match expr
    [(Num n) n]
    [(Mul n1 n2) (* (evalExpr n1) (evalExpr n2))]
    [(Add n1 n2) (+ (evalExpr n1) (evalExpr n2))]))

;; Declare and define the equation
(define equation
  (Add (Mul (Num 1) (Num 3))
       (Mul (Add (Num 2) (Num 3)) (Num 5))))

;; Main function (Racket doesn't have a main, so we use a script approach)
(begin
  (displayln "Evaluating: ")
  (displayln equation)
  (displayln "Output:")
  (displayln (evalExpr equation)))
