#lang racket

(provide all-defined-out)

;; Grammar
(struct Num () #:transparent)
(struct Zero () #:transparent)
(struct Add (a b) #:transparent)
(struct Mul (a b) #:transparent)
(struct Inc (x) #:transparent)
(struct Dec (x) #:transparent)


(define (eval expr n)
  #| *eval*
   Evaluation rules
   Arguments:
       expr - expression to evaluate, should be composed from *grammar*
       n - input value for the expression/unknown
           you can think of expression as f(x), then n:=x
  |#
  (cond
    [(procedure? expr) (eval (expr) n)]
    [else
      (match expr
        [(? Num?) n]
        [(? Zero?) 0]
        [(Add left right) (+ (eval left n) (eval right n))]
        [(Mul left right) (* (eval left n) (eval right n))]
        [(Inc x) (add1 (eval x n))]
        [(Dec x) (sub1 (eval x n))])]))



(define (synthesize inputs outputs #:max-depth [max-depth 2])
  #| *synthesize*
   Bottom-up explicit search for program
   that satisfies relationship *p(input) == output* for all inputs, outputs.
   The program search is based on grammar defined by:
      - *terminals* list - initial terminals
      - *non-terminals* list - composition rules
      - *eval* rules
   The search is performed until program satisfies input-output relationship or depth limit reached.
   If none of the programs found in iteration satisfies input-output relationship,
     then they are used *as* terminals in the next iteration.
   
   Arguments:
       inputs - list of training inputs
       outputs - list of training targets (must have same shape as inputs)
       max-depth - (Optional), determines depth of search
  |#
  (define plist terminals)
  (call/cc
   (lambda (return)
     (for ([current-depth (in-range max-depth)])
       (set! plist (grow plist))
       (set! plist (elim-equivalents plist inputs))
       (for ([p plist])
         (when (is-correct p inputs outputs)
           (return p)))))))

;; Terminals evaluate directly to value, non-terminals are dependent on other expressions
(define terminals (list Num Zero))
(define non-terminals (list Add Mul Inc Dec))


(define (grow plist)
  #| *grow*
   Expand list of terminals by composing them from grammar tree.
   I.e. if we have:
   
       plist/terminals   = (list Num Zero)
       non-terminals     = (list Add Inc)
   
   then we'll append following terminals to program list:
   
       (list
         (Add Num Zero)
         (Add Zero Num)
         (Inc Num)
         (Inc Zero))
   
   And as a result the function will return list of programs that appends above to the input list.
   So, essentially we end up with:
        
        plist = (list Num Zero (Add Num Zero) (Add Zero Num) (Inc Num) (Inc Zero))
   
   Arguments:
       plist - list of intial terminals
  |#
  (foldl append
         plist
         (map (λ (nt)
                (cond
                  [(eq? nt Add)
                   (map (λ (combination)
                          (Add (first combination) (second combination)))
                        (combinations plist 2))]
                  [(eq? nt Mul)
                   (map (λ (combination)
                          (Mul (first combination) (second combination)))
                        (combinations plist 2))]
                  [(eq? nt Inc)
                   (map (λ (pl) (Inc pl)) plist)]
                  [(eq? nt Dec)
                   (map (λ (pl) (Dec pl)) plist)]))
              non-terminals)))


(define (elim-equivalents plist inputs [seen '()])
  #| *elim-equivalents*
   Removes programs that yield same results for given inputs.
   To be more precise, it executes all programs on same inputs
   and then keeps only single program for each output.
   I.e. if we have:
           
           plist = (list Num Zero (Add Num Num) (Mul Num Zero))
   
   then it will remove (Mul Num Zero) because it's equivalent to Zero.
   
   Arguments:
      plist - list of initial terminals
      inputs - training input data
      seen - recursion feature,
             it holds a list of programs that were already found in plist
             keep it empty, unless you want to explicitely remove particular programs
  |#
  (cond
    [(empty? plist) '()]
    [else
     (define first-p (first plist))
     (define rest-p  (rest  plist))
     (define result (map (λ (input) (eval first-p input)) inputs))
     (if (member result seen)
         (elim-equivalents rest-p inputs seen)
         (cons first-p
               (elim-equivalents
                rest-p
                inputs
                (cons result seen))))]))

(define (is-correct p inputs outputs)
  (andmap (λ (input output)
            (equal? (eval p input) output))
          inputs outputs))


(display "\nSynthesize f(X)=X function: f(10)=")
(define test-inputs (list 1 2 3))
(define test-outputs (list 1 2 3))
(eval (synthesize test-inputs test-outputs) 10)

(display "\nSynthesize f(X)=0 function: f(10)=")
(set! test-inputs (list 1 2 8))
(set! test-outputs (list 0 0 0))
(eval (synthesize test-inputs test-outputs) 10)

(display "\nSynthesize f(X)=X+1 function: f(10)=")
(set! test-inputs (list 1 2 15))
(set! test-outputs (list 2 3 16))
(eval (synthesize test-inputs test-outputs) 10)

(display "\nSynthesize f(X)=7*X+1 function: f(10)=")
(set! test-inputs (list 1 2 0.5))
(set! test-outputs (list 8 15 4.5))
(eval (synthesize test-inputs test-outputs #:max-depth 6) 10)

(display "\nSynthesize f(X)=X**3 function: f(10)=")
(set! test-inputs (list 2 4 5))
(set! test-outputs (list 8 64 125))
(eval (synthesize test-inputs test-outputs #:max-depth 6) 10)



