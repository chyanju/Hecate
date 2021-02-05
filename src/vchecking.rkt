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
	(rename-out 
		[interpret v:interpret]
		[idict v:idict]
		[tree-validate v:tree-validate]
	)
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
	(for*/all ([tmpself self] [self0 (unbox tmpself)])
		; (printf ">> traverse starts\n")
		(define class (tree-class self0))
		(define visitor (ag:traversal-ref/visitor trav class))
		; (printf ">> stub0\n")
		(for ([command (ag:visitor-commands visitor)])
			; (printf ">> traverse command starts: ~a\n" command)
			(match command
				[(ag:recur child)
					; (printf ">> [start] recur, command is: ~a\n" command)
					(define subtree (tree-ref/child self0 child))
					; (printf ">> stub2\n")
					(for*/all ([tmpsubtree subtree] [subtree0 (unbox tmpsubtree)])
						; (printf ">> stub3, subtree0: ~a\n" subtree0)
						(if (list? subtree0)
							(for ([node subtree0])
								(ex:traverse trav (box node))
							)
							(begin
								; (printf ">> stub4\n")
								(define rrr (ex:traverse trav (box subtree0)))
								; (printf ">> stub4 end\n")
								rrr
							)
						)
					)
					; (printf ">> [end] recur, command is: ~a\n" command)
				]
				[(ag:iter/left child commands)
					; (printf ">> stub1b\n")
					(iterate self0 child identity commands trav)
				]
				[(ag:iter/right child commands)
					; (printf ">> stub1c\n")
					(iterate self0 child reverse commands trav)
				]
				[(ag:skip)
					; (printf ">> stub1d\n")
					(void)
				]
				[(ag:eval _)
					; (printf ">> stub1e\n")
					(let ([ev command])
						(define ev-attr (ag:eval-attribute ev))
						(define ev-rule (ag:class-ref*/rule class ev-attr))
						(define ev-ready (^tree-select/ready self0 ev-attr))
						(for/all ([q ev-ready])
							(assert (! (ag:slot-v q)) "before:write-to-0con")
						)
						(define ev-field (^tree-select/field self0 ev-attr))
						(define ev-res (evaluate self0 (ag:rule-formula ev-rule)))
						(for/all ([q ev-field])
							(ag:set-slot-v! q ev-res)
						)
						(for/all ([q ev-ready])
							(ag:set-slot-v! q #t)
							(assert (ag:slot-v q) "after:write-to")
						)
					)
					; (printf ">> stub1e end\n")
				]
				[_ (println-and-exit "# exception/traverse: unknown command ~a\n" command)]
			)
			; (printf ">> traverse command ends: ~a\n" command)
		)
		; (printf ">> traverse ends\n")
	)
)

(define (iterate self child order commands trav)
	(define class (tree-class self))
	(define state0 (accumulator self))
	; (printf ">> stub0\n")
	(for ([command commands])
		; (printf ">> [start] iterate command: ~a\n" command)
		(match command
			[(ag:recur _)
				(void)
			]
			[(ag:skip)
				(void)
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
		; (printf ">> [end] iterate command ends: ~a\n" command)
	)

	
	; (printf ">> self is: ~v\n" self)
	; (define tod (tree-ref/child self child))
	(define qqq (tree-ref/child self child))
	(define tod 
		(if (null? qqq) 
			qqq 
			(if (list? qqq)
				(list-ref qqq 0)
				qqq
			)
		)
	) ; (fixme) don't know why, hack
	; (printf ">> tod is: ~v\n" tod)
	; (printf "... peek in\n")
	; (printf "... tod is: ~v\n" tod)
	; (printf "... qqq is: ~v\n" qqq)
	(if (null? tod)
		(void)
		; else
		(for*/all ([tmptt tod] [tod0 (unbox tmptt)])
			(define state#
				(for/fold ([state- state0]) ([node (order (list-wrapper tod0))])
					(define state+ (accumulator self))
					; (printf ">>> all commands to go: ~a\n" commands)
					(for ([command commands])
						; (printf ">>> matching: ~a\n" command)
						(match command
							[(ag:recur (== child))
								; (printf ">>> right?\n")
								(ex:traverse trav node)
							]
							[(ag:skip)
								(void)
							]
							[(ag:eval _)
								(let ([ev command])
									(define ev-attr (ag:eval-attribute ev))
									(define ev-rule (ag:class-ref*/rule class ev-attr))
									(define ev-eval
										(curry evaluate self #:iterator child #:cursor (unbox node) #:accumulator state-)
									)
									(if (ag:rule-folds? ev-rule)
										(begin
											; (printf ">>> here1?\n")
											(define ev-res (ev-eval (ag:rule-fold-next ev-rule)))
											(ag:set-slot-v! (^ass-ref state+ ev-attr))
										)
										(begin
											; (fixme) cursor is not used ???
											; (printf ">>> here2?\n")
											; (printf ">>> self is: ~v\n" self)
											; (printf ">>> node is: ~v\n" node)
											(define ev-ready (^tree-select/ready self ev-attr #:iterator child #:cursor (unbox node)))
											(for/all ([q ev-ready])
												(assert (! (ag:slot-v q)) "before:write-to-1con")
											)
											(define ev-field (^tree-select/field self ev-attr #:iterator child #:cursor (unbox node)))
											(define ev-res (ev-eval (ag:rule-formula ev-rule)))
											(for/all ([q ev-field])
												(ag:set-slot-v! q ev-res)
											)
											(for/all ([q ev-ready])
												(ag:set-slot-v! q #t)
												(assert (ag:slot-v q) "after:write-to")
											)
											; (printf ">>> here5?\n")
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
				(for/all ([q ev-ready])
					(assert (! (ag:slot-v q)) "before:write-to-2??")
				)
				(define ev-field (^tree-select/field self attr))
				(define ev-res (ag:slot-v value))
				(for/all ([q ev-field])
					(ag:set-slot-v! q ev-res)
				)
				(for/all ([q ev-ready])
					(ag:set-slot-v! q #t)
					(assert (ag:slot-v q) "after:write-to")
				)
			)
		)
	)
	; (printf "... peek out\n")
	; (printf ">> self is: ~v, child is: ~v, tod is: ~v\n" self child tod)
	
	; (printf ">> stub0, ends\n")
	
)

(define (evaluate self term #:iterator [iter #f] #:cursor [cur #f] #:accumulator [acc #f])
	(define (recur term)
		; (printf "** [start] term is: ~a\n" term )
		(match term
			[(ag:const val)
				val
			]
			[(ag:field attr)
				; (printf ">>>>> peek:0\n")
				(define ev-ready (^tree-select/ready self attr #:iterator iter #:cursor cur))
				; (printf ">>>>> peek:1\n")
				(for/all ([q ev-ready])
					(assert (ag:slot-v q) "before:read-from/field")
				)
				(define ev-field (^tree-select/field self attr #:iterator iter #:cursor cur))
				(for/all ([q ev-field])
					(ag:slot-v q)
				)
			]
			[(ag:accum attr)
				(ag:slot-v (^ass-ref acc attr))
			]
			[(ag:index/first (cons child field) default)
				; (printf ">> peek\n")
				; (printf ">> nodes-pre is: ~v\n " (tree-ref/child self child))
				; (define nodes (tree-ref/child self child))
				(define qqq (tree-ref/child self child)); (fixme) don't know why, hack
				(define nodes 
					(if (null? qqq)
						qqq
						(if (list? qqq)
							(list-ref qqq 0)
							qqq
						)
					)
				)
				; (printf ">> nodes is: ~v\n" nodes)
				(if (null? nodes)
					(void)
					; else
					(for*/all ([tmpnodes nodes] [nodes0 (unbox tmpnodes)])
						(if (null? nodes0)
							(recur default)
							(begin
								(define ev-ready (tree-ref/ready (unbox (first nodes0)) field))
								(assert (ag:slot-v ev-ready) "before:read-from/index/first")
								(define ev-field (tree-ref/field (unbox (first nodes0)) field))
								(ag:slot-v ev-field)
							)
						)
					)
				)
			]
			[(ag:index/last (cons child field) default)
				; (define nodes (tree-ref/child self child))
				(define qqq (tree-ref/child self child)); (fixme) don't know why, hack
				(define nodes 
					(if (null? qqq)
						qqq
						(if (list? qqq)
							(list-ref qqq 0)
							qqq
						)
					)
				)
				(if (null? nodes)
					(void)
					; else
					(for*/all ([tmpnodes nodes] [nodes0 (unbox tmpnodes)])
						(if (null? nodes0)
							(recur default)
							(begin
								(define ev-ready (tree-ref/ready (unbox (last nodes0)) field))
								(assert (ag:slot-v ev-ready) "before:read-from/index/last")
								(define ev-field (tree-ref/field (unbox (last nodes0)) field))
								(ag:slot-v ev-field)
							)
						)
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
		; (printf "** [done] term is: ~a\n" term )
	)
	(define tmp0 (recur term))
	tmp0
)

; Validate some property of every output attribute value.
(define (tree-validate tree check)
	(for*/all ([tmp tree] [tree0 (unbox tmp)])
		; (printf ">>> peekrrr\n")
		; (printf ">>> tree0 is: ~v\n" tree0)
		(define treeq (if (box? tree0) (unbox tree0) tree0)) ; (fixme) don't know why, hack
		(for ([p (tree-readys treeq)])
			; (printf ">>> peek0\n")
			; (printf ">>> p is: ~v\n" p)
			(define label (car p))
			(define value (cdr p))
			; (printf ">>> peek\n")
			; (displayln `(verifying ,(ag:class-name (tree-class treeq)) ,label))
			(check value)
		)
		
		(for ([p (tree-children treeq)])
			(define name (car p))
			; (define subtree (cdr p))
			(define qqq (cdr p)) ; (fixme) don't know why, hack
			(define subtree
				(if (null? qqq)
					qqq
					(if (list? qqq)
						(list-ref qqq 0)
						qqq
					)
				)
			)
			(if (null? subtree)
				(void)
				; else
				(for*/all ([tmpsubtree subtree] [subtree0 (unbox tmpsubtree)])
					(if (list? subtree0)
						(for ([node subtree0])
							(tree-validate (box node) check)
						)
						(tree-validate (box subtree0) check)
					)
				)
			)
		)
	)
	
)


