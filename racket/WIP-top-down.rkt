#lang racket

  #|
   Grow list of programs by composing them from grammar tree.
   It's analogical to *bottom-up-explicit-search.rkt* implementation,
     but here we compose programs in opposite direction, which makes the implementation a bit more complex,
     because we need to keep "holes" for expressions.
   
   Instead of using structs for each operation, here we have just *Expr* structure that basically has 2 fields:
   - type [U 'symbol #f] - this is name of the operation (it's equivalent to separate structs in bottom-up implementation
            It also identifies "hole" type, which is flagged by #f in place of type :)
            This flag is used by parsers to identify paths to holes.
            Basically this is how hole looks: (Expr #f #f)
             and this is how multiplication expr looks (Expr 'mul
                                                             (list (Expr ...)
                                                                   (Expr ...))
            Notice it has 2 Expr's. Their type will depend on the complete tree - it's recursive.
            At the end of the recusion you would have one of *terminator* types,
             so basically either 'var or 'one (or more if they are implemented).
   
   - args [U List(Expr) #f] - list of arguments, they define arguments for each type,
                        for example:
                            'var doesn't take args, so it has #f args
                            'mul might take 2 arguments, so it would be (list expr1 expr2)
                                 where expr1 and expr2 must be Expr's, the synthesizer initializes them with "holes"


  This implementation is not super efficient, it's mostly for learning purposes. I know for sure that it could be optimized at least within the range of one order, because I sometimes search multiple times for something that I could technically do in one run.
  |#


(struct Expr (type args)
  #:transparent)


;; Terminals evaluate directly to value, non-terminals are dependent on other expressions
;(define terminals (list (Expr 'var #f) (Expr 'one #f)))
(define terminals
  (list (Expr 'const 'terminator)
        (Expr 'var 'terminator)
        (Expr 'one 'terminator)
        (Expr 'op-mul 'terminator)
        (Expr 'op-add 'terminator)
        (Expr 'op-sub 'terminator)))
(define non-terminals
  (list #;(Expr 'add (list (Expr #f #f) (Expr #f #f)))
        #;(Expr 'mul (list (Expr #f #f) (Expr #f #f)))
        (Expr 'arith-expr (list (Expr #f #f) (Expr #f #f) (Expr #f #f)))
        (Expr 'map (list (Expr #f #f) (Expr #f #f)))
        (Expr 'function (list (Expr #f #f) (Expr #f #f)))))





(define (evaluator const)
  (define (eval expr)
    (match expr
      ;[(? list? expr) (begin (printf "here list ~a" expr) (eval (map (λ (el) (eval el)) expr)))]
      [(Expr 'const _) const]
      
      [(Expr 'var val) val]
      
      [(Expr 'one _) 1]
      
      [(Expr 'arith-expr (list arith-op expr-left expr-right))
             ((eval arith-op) (eval expr-left) (eval expr-right))]
      
      [(Expr 'op-add _) +]
      
      [(Expr 'op-mul _) *]

      [(Expr 'op-sub _) -]
      
      ;; Those below are really just placeholders at this point, just to keep an eye on them.
      ;[(Expr 'map (list fun lst)) (map (eval fun) (eval lst))]
      [(Expr 'map (list fun lst)) (eval (map (eval fun) (eval lst)))]
      [(Expr 'function (list expr-b)) (let ([var-expr-positions (find-var-expr expr-b)])
                                        (λ (arg)
                                        (begin
                                          #;(println "here????\n\n\n")
  
                                          (define expr-c (foldl (λ (var-expr-pos transformed-expr)
                                                                  #;(printf "transformed-expr: ~a\nexpr-b: ~a" transformed-expr expr-b)
                                                                  (new-expr (if (null? transformed-expr)
                                                                                expr-b
                                                                                transformed-expr)
                                                                            var-expr-pos
                                                                            (Expr 'var arg)))
                                                                '()
                                                                var-expr-positions))
                                           #;(printf "\nexpression b: [~a], c: [~a]  =eval>  [~a]\n" expr-b expr-c (eval expr-c))
                                           (eval expr-c))))]
      [val val]))
  eval)


(define (find-var-expr expr)
  
  (define (parse-path expr node-trace)
    (match expr
      [(Expr 'var _) (reverse node-trace)]
      [(Expr _ 'terminator) '()]
      [(Expr 'function _) '()]
      [(Expr _ nodes) (unwrap (filter (λ (lst) (not (empty? lst)))
                              (for/fold ([acc '()])
                                        ([id (length nodes)]
                                         [node nodes])
                                (cons (parse-path node
                                                  (cons id node-trace))
                                      acc))))]
      [else '()]))
  
  (if (Expr-args expr)
      (filter (λ (x)
                (not (null? x)))
              (parse-path expr '()))
      '()))


(define (synthesize inputs outputs #:max-depth [max-depth 2])
  #| *synthesize*
   Top-down search
   
   Arguments:
       inputs - list of training inputs
       outputs - list of training targets (must have same shape as inputs)
       max-depth - (Optional), determines depth of search
  |#
  (define plist (list (Expr #f #f)))
  (call/cc
   (lambda (return)
     (for ([current-depth (in-range max-depth)])
       (set! plist (grow plist))
       (set! plist (remove-duplicates plist))
       ;(set! plist (elim-equivalents plist inputs))
       #;(printf "terminated: ~a\n" (filter (λ (p)
                                            (null? (flatten (find-empty-exprs p))))
                                          plist))
       (for ([program (filter (λ (p)
                                (null? (flatten (find-empty-exprs p))))
                              plist)])
         (when (is-correct program inputs outputs)
           (return program)))
     (when (equal? (sub1 max-depth) current-depth)
         (return #f))))))




(define (new-expr parent-expr path expr-appendix)
    (let ([out
           (let ([type (Expr-type parent-expr)]
                 [args (Expr-args parent-expr)])
             (cond
               [(null? (cdr path)) (Expr type (list-set args
                                                        (car path)
                                                        expr-appendix))]
               [else (Expr type (list-set args
                                          (car path)
                                          (new-expr (list-ref args
                                                              (car path))
                                                    (cdr path)
                                                    expr-appendix)))]))])
      ;(printf "debug out: ~a\n" out)
      out))

(define (grow plist)
    
  (define vocab (append terminals non-terminals))
  ;(printf"\n\n\nnew generation...\n")
  (let ([outcome
         (flatten (map (λ (expr)
                         ;(printf "\n\nexpr: ~a\n" expr)
                         ;(printf "(find-empty-exprs expr): ~a\n" (find-empty-exprs expr))
                         (if (null? (find-empty-exprs expr))
                             vocab
                             (for*/fold ([acc '()])
                                        ([empty-expr-path (find-empty-exprs expr)]
                                         [expr-appendix-candidate vocab])
                               (begin
                                 ;(printf "\nempty-expr-path: ~a\n" empty-expr-path)
                                 ;(printf "expr-appendix-candidate: ~a\n" expr-appendix-candidate)
                        
                                 (let ([new-exp (cons
                                                 (new-expr expr empty-expr-path expr-appendix-candidate)
                                                 acc)])
                                   ;(printf "new expr:\n~a\n" new-exp)
                                   new-exp)))))
                       plist))])
    ;(printf "\noutcome:\n~a\n" outcome)
    outcome))


(define (remove-duplicates plist)
  (cond
    [(empty? plist) '()]
    [(equal? 1 (length plist)) plist]
    [else (for/fold ([acc '()])
                    ([i (range (length plist))]
                     [item-a plist])
            (cond
              [(null? acc) (cons item-a acc)]
              [(ormap (λ (item-b) (equal? item-a item-b)) acc)
                acc]
              [else   (cons item-a acc)]))]))


(define (is-correct program inputs outputs)
  (define eval (evaluator inputs))
  (equal? (with-handlers ([exn:fail? (λ (e) #f)])
            (eval program))
          outputs))





(define (unwrap lst)
  (define (atomic-list? lst)
    (and (list? lst)
         (andmap number? lst)))
  (cond
    [(atomic-list? lst) (list lst)]
    [(list? lst) (append-map unwrap lst)]
    [else (list lst)]))
  




(define (find-empty-exprs expr)
  (define (parse-path expr node-trace)
    (match expr
      [(Expr _ 'terminator) '()]
      [(Expr _ #f) (reverse node-trace)]
      [(Expr _ nodes) (unwrap (filter (λ (lst) (not (empty? lst)))
                              (for/fold ([acc '()])
                                        ([id (length nodes)]
                                         [node nodes])
                                (cons (parse-path node
                                                  (cons id node-trace))
                                      acc))))]
      [else '()]))
  
  (if (Expr-args expr)
      (filter (λ (x)
                (not (null? x)))
              (parse-path expr '()))
      '()))









(module+ test
  (displayln "\n###########################################\n↓TESTS↓
###########################################")
  (displayln "\nTest find-empty-exprs:")
  (find-empty-exprs (Expr 'add
                          (list
                           (Expr 'var 'terminator)
                           (Expr 'mul
                                 (list
                                  (Expr 'add
                                        (list
                                         (Expr 'one 'terminator)
                                         (Expr 'one #f)
                                         (Expr 'mul (list
                                                     (Expr 'one 'terminator)
                                                     (Expr 'var #f)))))
                                  (Expr 'var 'terminator))))))
  (let ([input (list 1 2 3)]
        [output (list 1 2 3)]
        [max-depth 2])
    (printf "\nTest synthesize [~a -> ~a]:\n" input output)
    (synthesize input output #:max-depth max-depth))

  
  (let ([input (list 1 2 3)])
    (let ([eval (evaluator input)]
          [expr (Expr 'map
                      (list (Expr 'function
                                  (list
                                   (Expr 'arith-expr
                                         (list (Expr 'op-add 'terminator)
                                               (Expr 'one 'terminator)
                                               (Expr 'var 'terminator)))))
                                        
                            (Expr 'const 'terminator)))])
      (printf "\nTest eval [~a]:\n" expr)
      (eval expr)))

  
  (let ([input 1]
        [output 2]
        [max-depth 4])
    (printf "\nTest synthesize [~a -> ~a]:\n" input output)
    (synthesize input output #:max-depth max-depth))
  

  #;(let ([input (list 1 2 3)]
        [output (list 2 3 4)]
        [max-depth 6])
    (printf "\nTest synthesize [~a -> ~a]:\n" input output)
    (synthesize input output #:max-depth max-depth))

  )
