#lang rosette

; A symbolic data structure for static scheduling of statements with varying
; static and dynamic contexts.


(require
	rosette/solver/mip/cplex
	rosette/solver/smt/z3
	rosette/lib/synthax
	rosette/lib/angelic
	"./grammar/syntax.rkt"
	"./grammar/tree.rkt"
	"./utility.rkt"
)
(provide
	interpret
	idict
)

; Activate an ILP solver (IBM CPLEX if available, otherwise Z3 in ILP mode)
; (current-solver
;   (if (cplex-available?) ; FIXME: Rosette encodes for CPLEX very slowly...
; 	  (cplex)
; 	  (z3 #:logic 'QF_LIA)))

; =============================================
; ======== interpreter and necessities ========
; =============================================
; includes:
;         | interpret
;         | traverse
;         | evaluate

(define idict (make-hash)) ; stores indication matrix
; indication matrix
(define (get-imat nth clist)
	; (printf ">> get-imat gets: ~a\n" clist)
	(if (hash-has-key? idict nth)
		(hash-ref idict nth)
		(let ([n (length clist)])
			(define m
				(for/list ([row (range n)])
					(apply choose* clist)
				)
			)
			(hash-set! idict nth m)
			(hash-ref idict nth)
		)
	)
)
(define (list-wrapper p)
	(if (list? p)
		p
		(list p)
	)
)

(struct denotation (ops fns ite) #:transparent)
; lifted
; (define (^match-op arg-op)
; 	(cond
; 		[(symbol? arg-op)
; 			(cond
; 				[(equal? arg-op '+) +]
; 				[(equal? arg-op '-) -]
; 				[(equal? arg-op '*) *]
; 				[(equal? arg-op '/) /]
; 				[(equal? arg-op '<) <]
; 				[(equal? arg-op '<=) <=]
; 				[(equal? arg-op '==) =]
; 				[(equal? arg-op '>=) >=]
; 				[(equal? arg-op '>) >]
; 				[(equal? arg-op '!) not]
; 				[(equal? arg-op '&&) (λ (e1 e2) (and e1 e2))]
; 				[(equal? arg-op '\|\|) (λ (e1 e2) (or e1 e2))]
; 				[else (println-and-exit "# exception/^match-op: unsupported operator ~a\n" arg-op)]
; 			)
; 		]
; 		[(union? arg-op)
; 			(for/all ([pp arg-op])
; 				(^match-op pp)
; 			)
; 		]
; 		[else (println-and-exit "# exception/^match-op: unsupported key ~a\n" arg-op)]
; 	)
; )

; (define (^match-fn arg-fn)
; 	(cond
; 		[(symbol? arg-fn)
; 			(cond
; 				[(equal? arg-fn 'max) max]
; 				[(equal? arg-fn 'min) min]
; 				; [else (println-and-exit "# exception/^match-fn: unsupported operator ~a\n" arg-fn)]
; 				; FIXME: need to address the uninterpreted function
; 				[else panacea]
; 			)
; 		]
; 		[(union? arg-fn)
; 			(for/all ([pp arg-fn])
; 				(^match-fn pp)
; 			)
; 		]
; 		[else (println-and-exit "# exception/^match-fn: unsupported key ~a\n" arg-fn)]
; 	)
; )
; (define concrete-denotation
; 	(denotation 
; 		^match-op
; 		^match-fn
; 		(λ (e1 e2 e3) (if e1 e2 e3))
; 	)
; )

(define (denote-op op xs)
	(void)
	; (apply 
	; 	((denotation-ops concrete-denotation) op) 
	; 	xs
	; )
)

(define (denote-fn fn xs)
	(void)
	; (apply 
	; 	((denotation-fns concrete-denotation) fn)
	; 	xs
	; )
)

(define (denote-ite if then else)
	; ((denotation-ite concrete-denotation) if then else)
	(void)
)

; returns an associated list:
(define (accumulator self)
	(for/list ([attr (ag:class-counters (tree-class self))])
		; (pending) is it box or slot?
		(cons attr (ag:slot #f))
	)
)

(define (interpret schedule tree)
	; (fixme) currently this only supports sequential schedule
	(ex:traverse schedule tree)
)

(define (ex:traverse trav self)
	(define class (tree-class self))
	(define visitor (ag:traversal-ref/visitor trav class))

	(for ([command (ag:visitor-commands visitor)])
		(match command
			[(ag:recur child)
				(define subtree (tree-ref/child self child))
				(if (list? subtree)
					(for ([node subtree])
						(ex:traverse trav node)
					)
					(ex:traverse trav subtree)
				)
			]
			[(ag:iter/left child commands)
				(iterate self child identity commands trav)
			]
			[(ag:iter/right child commands)
				(iterate self child reverse commands trav)
			]
			[(ag:skip)
				(void)
			]
			[(list 'multichoose nth vs ...)
				; (printf "<traverse> multichoose nth=~a\n" nth)
				; obtain a |vs| * |vs| matrix of boolean symbolic variables
				(define imat (get-imat nth vs))
				(for ([ev imat])
					; for every row, which is every choose, which is one rule or null
					(if (null? ev)
						(void) ; do nothing
						; else this is a rule
						(begin
							(define ev-attr (ag:eval-attribute ev))
							(define ev-rule (ag:class-ref*/rule class ev-attr))
							(define ev-ready (^tree-select/ready self ev-attr))
							(assert (! (ag:slot-v ev-ready)) "before:write-to-0sym")
							(define ev-field (^tree-select/field self ev-attr))
							(define ev-res (evaluate self (ag:rule-formula ev-rule)))
							(ag:set-slot-v! ev-field ev-res)
							(ag:set-slot-v! ev-ready #t)
							(assert (ag:slot-v ev-ready) "after:write-to")
						)
					)
				)
			]
			[(ag:eval _)
				(let ([ev command])
					(define ev-attr (ag:eval-attribute ev))
					(define ev-rule (ag:class-ref*/rule class ev-attr))
					(define ev-ready (^tree-select/ready self ev-attr))
					(assert (! (ag:slot-v ev-ready)) "before:write-to-0con")
					(define ev-field (^tree-select/field self ev-attr))
					(define ev-res (evaluate self (ag:rule-formula ev-rule)))
					(ag:set-slot-v! ev-field ev-res)
					(ag:set-slot-v! ev-ready #t)
					(assert (ag:slot-v ev-ready) "after:write-to")
				)
			]
			[_ (println-and-exit "# exception/traverse: unknown command ~a\n" command)]
		)
	)
)

(define (iterate self child order commands trav)
	(define class (tree-class self))
	(define state0 (accumulator self))

	(for ([command commands])
		(match command
			[(ag:recur _)
				(void)
			]
			[(ag:skip)
				(void)
			]
			[(list 'multichoose nth vs ...)
				; (printf "<iterate> multichoose nth=~a\n" nth)
				(define imat (get-imat nth vs))
				(for ([ev imat])
					(if (null? ev)
						(void) ; do nothing
						; else this is a rule
						(begin
							(define ev-attr (ag:eval-attribute ev))
							(define ev-rule (ag:class-ref*/rule class ev-attr))
							(when (ag:rule-folds? ev-rule)
								(ag:set-slot-v! (^ass-ref state0 ev-attr) (evaluate self (ag:rule-fold-init ev-rule)))
							)
						)
					)
				)
			]
			[(ag:eval _)
				(let ([ev command])
					(define ev-attr (ag:eval-attribute ev))
					(define ev-rule (ag:class-ref*/rule class ev-attr))
					(when (ag:rule-folds? ev-rule)
						(ag:set-slot-v! (^ass-ref state0 ev-attr) (evaluate self (ag:rule-fold-init ev-rule)))
					)
				)
			]
			[_ (println-and-exit "# exception/iterate: unknown command ~a\n" command)]
		)
	)

	(define state#
		(for/fold ([state- state0]) ([node (order (list-wrapper (tree-ref/child self child)))])
			(define state+ (accumulator self))
			(for ([command commands])
				(match command
					[(ag:recur (== child))
						(ex:traverse trav node)
					]
					[(ag:skip)
						(void)
					]
					[(list 'multichoose nth vs ...)
						; (printf "<state> multichoose nth=~a\n" nth)
						(define imat (get-imat nth vs))
						(for ([ev imat])
							(if (null? ev)
								(void) ; do nothing
								; else this is a rule
								(begin
									(define ev-attr (ag:eval-attribute ev))
									(define ev-rule (ag:class-ref*/rule class ev-attr))
									(define ev-eval
										(curry evaluate self #:iterator child #:cursor node #:accumulator state-)
									)
									(if (ag:rule-folds? ev-rule)
										(begin
											(define ev-res (ev-eval (ag:rule-fold-next ev-rule)))
											(ag:set-slot-v! (^ass-ref state+ ev-attr))
										)
										(begin
											(define ev-ready (^tree-select/ready self ev-attr #:iterator child #:cursor node))
											(assert (! (ag:slot-v ev-ready)) "before:write-to-1sym")
											(define ev-field (^tree-select/field self ev-attr #:iterator child #:cursor node))
											(define ev-res (ev-eval (ag:rule-formula ev-rule)))
											(ag:set-slot-v! ev-field ev-res)
											(ag:set-slot-v! ev-ready #t)
											(assert (ag:slot-v ev-ready) "after:write-to")
										)
									)
								)
							)
						)
					]
					[(ag:eval _)
						(let ([ev command])
							(define ev-attr (ag:eval-attribute ev))
							(define ev-rule (ag:class-ref*/rule class ev-attr))
							(define ev-eval
								(curry evaluate self #:iterator child #:cursor node #:accumulator state-)
							)
							(if (ag:rule-folds? ev-rule)
								(begin
									(define ev-res (ev-eval (ag:rule-fold-next ev-rule)))
									(ag:set-slot-v! (^ass-ref state+ ev-attr))
								)
								(begin
									(define ev-ready (^tree-select/ready self ev-attr #:iterator child #:cursor node))
									(assert (! (ag:slot-v ev-ready)) "before:write-to-1con")
									(define ev-field (^tree-select/field self ev-attr #:iterator child #:cursor node))
									(define ev-res (ev-eval (ag:rule-formula ev-rule)))
									(ag:set-slot-v! ev-field ev-res)
									(ag:set-slot-v! ev-ready #t)
									(assert (ag:slot-v ev-ready) "after:write-to")
								)
							)
						)
					]
					[_ (println-and-exit "# exception/iterate/state#: unknown command ~a\n" command)]
				)
			)
			state+
		)
	)

	; (for ([(attr value) (^in-ass? state#)])
	(for ([(attr value) state#])
		; (printf "> on4 attr: ~a\n" attr)
		(define ev-ready (^tree-select/ready self attr))
		(assert (! (ag:slot-v ev-ready)) "before:write-to-2??")
		(define ev-field (^tree-select/field self attr))
		(define ev-res (ag:slot-v value))
		(ag:set-slot-v! ev-field ev-res)
		(ag:set-slot-v! ev-ready #t)
		(assert (ag:slot-v ev-ready) "after:write-to")
	)
)

(define (evaluate self term #:iterator [iter #f] #:cursor [cur #f] #:accumulator [acc #f])
	(define (recur term)
		(cond
			[(union? term)
				(for/all ([t term])
					(recur t)
				)
			]
			[else
				(match term
					[(ag:const val)
						val
					]
					[(ag:field attr)
						(define ev-ready (^tree-select/ready self attr #:iterator iter #:cursor cur))
						(assert (ag:slot-v ev-ready) "before:read-from/field")
						(define ev-field (^tree-select/field self attr #:iterator iter #:cursor cur))
						(ag:slot-v ev-field)
					]
					[(ag:accum attr)
						(ag:slot-v (^ass-ref acc attr))
					]
					[(ag:index/first (cons child field) default)
						(define nodes (tree-ref/child self child))
						(if (null? nodes)
							(recur default)
							(begin
								(define ev-ready (tree-ref/ready (first nodes) field))
								(assert (ag:slot-v ev-ready) "before:read-from/index/first")
								(define ev-field (tree-ref/field (first nodes) field))
								(ag:slot-v ev-field)
							)
						)
					]
					[(ag:index/last (cons child field) default)
						(define nodes (tree-ref/child self child))
						(if (null? nodes)
							(recur default)
							(begin
								(define ev-ready (tree-ref/ready (last nodes) field))
								(assert (ag:slot-v ev-ready) "before:read-from/index/last")
								(define ev-field (tree-ref/field (last nodes) field))
								(ag:slot-v ev-field)
							)
						)
					]
					[(ag:ite if then else)
						; (note) recur happens first, which means both branches should be ready
						; (fixme) if the user introduces redundant branches, the synthesizer will fail
						(denote-ite (recur if) (recur then) (recur else))
					]
					[(ag:expr operator operands)
						(define tmp0 (map recur operands))
						(define tmp1 (denote-op operator tmp0))
						tmp1
					]
					[(ag:call function arguments)
						(denote-fn function (map recur arguments))
					]
					[_ (println-and-exit "# exception/evaluate: unknown term ~a\n" term)]
				)
			]
		)
	)
	(define tmp0 (recur term))
	tmp0
)




