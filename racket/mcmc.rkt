#lang racket

(require math/matrix)

(define (display-matrix Matrix
                        [title ""]
                        [precision 2])
  (define (num-list->dec-string num-list)
    (string-join (map (λ (num)
                        (real->decimal-string num precision))
                      num-list)))
  (display (format "~a\n~a"
                   title
                   (string-join
                    (foldr (λ (row acc)
                             (cons (num-list->dec-string (matrix->list row))
                                   acc))
                           '("\n")
                           (matrix-rows Matrix))
                    "\n"))))


(define (transpose Matrix)
  (define-values (tcols trows) (matrix-shape Matrix))
  (list->matrix trows tcols
                (flatten
                 (reverse
                  (for/fold ([rows '()])
                            ([orig-col (matrix-cols Matrix)])
                    (cons (matrix->list orig-col) rows))))))


(define (matmul A B)
  (define a-rows-len (matrix-num-rows A))
  (define b-cols-len (matrix-num-cols B))
  (define (cell-value i j)
    (matrix-dot (matrix-row A i)
                (transpose (matrix-col B j))))
  (list->matrix a-rows-len
                b-cols-len
                (flatten
                 (for/foldr ([rows-list '()])
                            ([j (in-range a-rows-len)])
                   (cons
                    (for/foldr ([row-vector '()])
                               ([k (in-range b-cols-len)])  
                      (cons (cell-value j k)
                            row-vector))
                    rows-list)))))


(define (K-n K n)
  (for/fold ([transition-n #f])
            ([i (in-range n)])
    (if (false? transition-n)
        K
        (matmul transition-n K))))


(define ftmc
  (let ([K (matrix [[0 0.1 0.9]
                    [0.3 0.3 0.4]
                    [0.7 0.25 0.05]])])

    (display "Implication of Fundamental Theorem of Markov Chains (paraphrase):
    \"as we move towards infinity, the initial state becomes irrelevant\"\n\n")
    (display-matrix K "K")
    (display-matrix (K-n K 2) (format "K^{2}"))
    (display-matrix (K-n K 5) "K^{5}")
    (display-matrix (K-n K 50) "K^{50}")
    (display-matrix (K-n K 500) "K^{500}")))

(define ftmc-periodic
  (let ([K (matrix [[0 1]
                    [1 0]])])

    (display "It doesn't apply to periodic functions.\n\n")
    (display-matrix K "K")
    (display-matrix (K-n K 50) "K^{50}")
    (display-matrix (K-n K 51) "K^{51}")))

(define ftmc-not-fully-connected
  (let ([K (matrix [[0.4 0.6 0 0]
                    [0.3 0.7 0 0]
                    [0 0 0.6 0.4]
                    [0 0 0.05 0.95]])])

    (display "It also requires the functions to be fully connected.\"\n\n")
    (display-matrix K "K")
    (display-matrix (K-n K 50) "K^{50}")))

ftmc
ftmc-periodic
ftmc-not-fully-connected