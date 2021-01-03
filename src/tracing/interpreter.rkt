#lang rosette
; Interpreter for language of tree traversal schedules

(require "../utility.rkt"
         "../grammar/syntax.rkt"
         "../grammar/tree.rkt"
         "../trace.rkt")

(provide denotation *denotation* concrete-denotation abstract-denotation
         interpret traverse)

(struct denotation (ops fns ite) #:transparent)

(define concrete-denotation
  (denotation (match-lambda ['+ +] ['- -] ['* *] ['/ /]
                            ['< <] ['<= <=] ['== =] ['>= >=] ['> >]
                            ['! not]
                            ['&& (λ (e1 e2) (and e1 e2))]
                            ['\|\| (λ (e1 e2) (or e1 e2))])
              (match-lambda ['max max] ['min min])
              (λ (e1 e2 e3) (if e1 e2 e3))))

(define abstract-denotation
  (denotation (const void)
              (const void)
              (const void)))

(define *denotation* (make-parameter concrete-denotation))

(define (denote-op op . xs)
  (apply ((denotation-ops (*denotation*)) op) xs))

(define (denote-fn fn . xs)
  (apply ((denotation-fns (*denotation*)) fn) xs))

(define (denote-ite if then else)
  ((denotation-ops (*denotation*)) if then else))

(define (accumulator self)
  (for/list ([attr (ag:class-counters (tree-class self))])
    (cons attr (box #f))))

(define (interpret schedule tree)
  (traverse schedule tree))

(define (traverse trav self)
  (define class (tree-class self))
  (define visitor (ag:traversal-ref/visitor trav class))
  (for*/permuted ([command (ag:visitor-commands visitor)])
    (match command
      [(ag:recur child)
       (define subtree (tree-ref/child self child))
       ;((distribute (curry traverse trav)) subtree)
       (if (list? subtree)
           (for ([node subtree])
             (traverse trav node))
           (traverse trav subtree))]
      [(ag:iter/left child commands)
       (iterate self child identity commands trav)]
      [(ag:iter/right child commands)
       (iterate self child reverse commands trav)]
      [(ag:eval attr)
       (define rule (ag:class-ref*/rule class attr))
       ; (define tmp0 (tree-select self attr))
       ; (define tmp1 (evaluate self (ag:rule-formula rule)))
       ; (printf ">>> rule is:~a, tmp0 is:~a, which will be set as:~a\n" rule tmp0 tmp1)
       ; (printf ">>> set-box! is:~a\n" set-box!)
       (set-box! (tree-select self attr)
                 (evaluate self (ag:rule-formula rule)))
       ; (define tmp2 (tree-select self attr))
       ; (printf ">>> now tmp0 is:~a\n" tmp2)
       ; (printf ">>> now tmp0 is:~a\n" tmp0)
       ]
      [(ag:skip)
       (void)])))

(define (iterate self child order commands trav)
  (define class (tree-class self))
  (define state0 (accumulator self))

  (for*/permuted ([command commands])
    (match command
      [(ag:recur _)
       (void)]
      [(ag:eval attr)
       (define rule (ag:class-ref*/rule class attr))
       (when (ag:rule-folds? rule)
         (set-box! (dict-ref state0 attr)
                   (evaluate self (ag:rule-fold-init rule))))]
      [(ag:skip)
       (void)]))

  (define state#
    (for/fold ([state- state0])
              ([node (order (tree-ref/child self child))])
      (define state+ (accumulator self))
      (for*/permuted ([command commands])
                     (match command
                       [(ag:recur (== child))
                        (traverse trav node)]
                       [(ag:eval attr)
                        (define rule (ag:class-ref*/rule class attr))
                        (define eval
                          (curry evaluate self #:iterator child #:cursor node #:accumulator state-))
                        (if (ag:rule-folds? rule)
                            (set-box! (dict-ref state+ attr)
                                      (eval (ag:rule-fold-next rule)))
                            (set-box! (tree-select self attr #:iterator child #:cursor node)
                                      (eval (ag:rule-formula rule))))]
                       [(ag:skip)
                        (void)]))

      state+))

  (for ([(attr value) (in-dict state#)])
    (set-box! (tree-select self attr) (unbox value))))

(define (evaluate self term #:iterator [iter #f] #:cursor [cur #f] #:accumulator [acc #f])
  (define/match (recur term)
    [((ag:const val))
     val]
    [((ag:field attr))
     (unbox (tree-select self attr #:iterator iter #:cursor cur))]
    [((ag:accum attr))
     (unbox (dict-ref acc attr))]
    [((ag:index/first (cons child field) default))
     (define nodes (tree-ref/child self child))
     (if (null? nodes)
         (recur default)
         (unbox (tree-ref/field (first nodes) field)))]
    [((ag:index/last (cons child field) default))
     (define nodes (tree-ref/child self child))
     (if (null? nodes)
         (recur default)
         (unbox (tree-ref/field (last nodes) field)))]
    [((ag:ite if then else))
     (denote-ite (recur if) (recur then) (recur else))]
    [((ag:expr operator operands))
     (denote-op operator (map recur operands))]
    [((ag:call function arguments))
     (denote-fn function (map recur arguments))])
  (recur term))
