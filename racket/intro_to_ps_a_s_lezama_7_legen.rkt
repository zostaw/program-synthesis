#lang rosette

(require rosette/lib/synthax)

(define int32? (bitvector 32))
(define (bv32 val)
  (bv val 32))

(define (legen i j)
  (bvadd (bvmul (?? int32?) i)
         (bvmul (?? int32?) j)
         (?? int32?)))


(define-symbolic x y int32?)

(define-values (a1 b1 c1) (values (bv32 2) (bv32 0) (bv32 3)))
(print-forms
 (synthesize
  #:forall (list x y)
  #:guarantee (assert (equal? (legen x y)
                              (bvadd (bvmul a1 x)
                                     (bvmul b1 y)
                                     c1)))))

(define-values (a2 b2 c2) (values (bv32 3) (bv32 2) (bv32 0)))
(print-forms
 (synthesize
  #:forall (list x y)
  #:guarantee (assert (equal? (legen x y)
                              (bvadd (bvmul a2 x)
                                     (bvmul b2 y)
                                     c2)))))





