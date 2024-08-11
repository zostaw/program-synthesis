#lang racket

(provide define-grammar)

(require (for-syntax syntax/parse))
(require syntax/parse)


(define-syntax (define-grammar stx)
  #| define-grammar
  Macro for fast grammar definition.
  Example usage:

    ; define terminal constants/variables that are used in grammar's terminals
    (define zero 0)
    (define var 5)

    (define-grammar evaluate
      (terminals
        [Var var]
        [Zero zero])
      (non-terminals
        [Add (left right) (+ (evaluate left) (evaluate right))]
        [Mul (left right) (* (evaluate left) (evaluate right))]))

    ; build expression that is composed from mentioned grammar
    (define expr (Mul (Add Var Zero) 13))
    (displayln expr)

    (set! var 10) ; evaluate for var=10
    (evaluate expr)

  |#
  (syntax-parse stx
    [(_ grammar-name:id
        ((~literal terminals) rule-t ...)
        ((~literal non-terminals) rule-nt ...))
     (with-syntax
         ;; Caluse patterns for structs
         ;; terminals
         ([(define-terminal-structs ...)
           (for/list ([r (syntax->list #'(rule-t ...))])
             (syntax-parse r
               [(rule-name:id body:id)
                #'(struct rule-name () #:transparent)]))]
          ;; non-terminals
          [(define-non-terminal-structs ...)
           (for/list ([r (syntax->list #'(rule-nt ...))])
             (syntax-parse r
               [(rule-name:id (field:id ...) body:expr)
                #'(struct rule-name (field ...) #:transparent)]))]
          ;; Clause patterns for eval function
          ;; terminals
          [(match-terminal-clauses ...)
           (for/list ([r (syntax->list #'(rule-t ...))])
             (syntax-parse r
               [(rule-name:id body:id ...)
                #:with rule-name? (datum->syntax 
                                   #'rule-name
                                   (string->symbol 
                                    (string-append 
                                     (symbol->string (syntax-e #'rule-name))
                                     "?"))
                                   #'rule-name)
                #'[(? rule-name?) body ...]]))]
          ;; non-terminals
          [(match-non-terminal-clauses ...)
           (for/list ([r (syntax->list #'(rule-nt ...))])
             (syntax-parse r
               [(rule-name:id (field:id ...) body:expr)
                #'[(rule-name field ...)
                   (let ([field (if (and (struct? field) (object-name field))
                                    (grammar-name field)
                                    field)]
                         ...) body)]]))])
       #'(begin
           define-terminal-structs ...
           define-non-terminal-structs ...
           (define (grammar-name expr)
             (cond
               [(procedure? expr) (grammar-name (expr))]
               [else
                (match expr
                  match-terminal-clauses ...
                  match-non-terminal-clauses ...
                  [x x])]))
           (provide grammar-name)))]))


