#lang rosette
(require 
	rosette/lib/angelic
	"../../src/grammar/tree.rkt"
	"../../src/grammar/syntax.rkt"
	"../../src/utility.rkt"
)
(provide (all-defined-out))

; g: grammar, i: interface
; find interface i in grammar g and return
(define (find-interface g i)
	(findf 
		(lambda (x) (equal? (ag:interface-name x) i))
		(ag:grammar-interfaces g)
	)
)

; i: interface, c: class
; find class c in interface i and return
(define (find-class i c)
	(findf
		(lambda (x) (equal? (ag:class-name x) c))
		(ag:interface-classes i)
	)
)

; c: class
; create brand new fields list for class c
(define (fresh-fields-list c)
	(for/list ([label0 (ag:class-labels* c)])
		(cons
			(ag:label-name label0)
			(ag:slot (ag:label/in? label0))
		)
	)
)

; c: class
; create brand new fields list for class c
(define (fresh-readys-list c)
	(for/list ([label0 (ag:class-labels* c)])
		(cons
			(ag:label-name label0)
			(ag:slot
				(if (ag:label/in? label0)
					(void)
					#f
				)
			)
		)
	)
)

(define (unroll-symbolic-example t)
	(cond
		[(box? t) (unroll-symbolic-example (unbox t))]
		[(union? t)
			; union
			(for/fold ([accu (list)]) ([t0 (union-contents t)])
				(append accu (unroll-symbolic-example (cdr t0)))
			)
		]
		[(list? t)
			; list
			; if you reach here, you get a fix-sized list
			(define cl
				(for/list ([q0 t]) 
					(unroll-symbolic-example q0)
				)
			)
			; get all potential combinations of every fixed position
			(define cp (apply cartesian-product cl))
			cp
		]
		[(tree? t)
			; box, means you hit:
			; - cdr of a pair in a child
			; - root
			; children is a tree
			(define cl
				(for/list ([p0 (tree-children t)])
					; note here:
					; - if child is a list, then (cdr p0) gets a list
					; - if child is a single object, then (cdr p0) get a box
					; (important) use list, because a list is also a pair
					(if (list? p0)
						; list: child list
						(if (equal? (length p0) 1)
							; no children, just return
							(list (list (car p0)))
							; has children
							(let ([tmp0 (unroll-symbolic-example (list-ref p0 1))])
								(for/list ([j0 tmp0])
									(cons (car p0) j0)
								)
							)
						)
						; pair: single object
						(let ([tmp0 (unroll-symbolic-example (cdr p0))])
							(for/list ([j0 tmp0])
								(cons (car p0) j0)
							)
						)
					)
				)
			)
			; cp: list of all potential tree-children
			(define cp (apply cartesian-product cl))
			; duplicate t for different children probabilities
			(for/list ([c cp])
				; (tree
				; 	(ag:class-name (tree-class t))
				; 	null
				; 	null
				; 	c
				; )
				(tree
					(tree-class t)
					(tree-fields t)
					(tree-readys t)
					c
				)
			)
		]
		[else (println-and-exit "error, got: ~a\n" t)]
	)
)

; g: grammar, w: width, h: height
(define (build-symbolic-example g w h)
	; h >= 1
	(new-cprogram g w (- h 1))
)

(define (new-cprogram g w h)
	(let ([class0 (find-class (find-interface g 'Program) 'CProgram)])
		(define fields-list (fresh-fields-list class0))
		(define readys-list (fresh-readys-list class0))
		(define children-list
			(if (<= h 0)
				; suggest shrinking
				(list
					(list
						'first_function
					)
				)
				; not shrinking
				(list
					(list
						'first_function
						(new-choose-list 
							g w (- h 1) 
							(list new-cfunction)
						)
					)
				)
			)
		)
		
		; construct and return
		(box (tree
			class0 fields-list readys-list children-list
		))
	)
)

(define (new-cfunction g w h)
	(let ([class0 (find-class (find-interface g 'Function) 'CFunction)])
		(define fields-list (fresh-fields-list class0))
		(define readys-list (fresh-readys-list class0))
		(define children-list
			(if (<= h 0)
				; suggest shrinking
				(list
					(list
						'first_statement
					)
					(list
						'next
					)
				)
				; not shrinking
				(list
					(list
						'first_statement
						(new-choose-list 
							g w (- h 1) 
							(list new-cstatement)
						)
					)
					(list
						'next
						(new-choose-list 
							g w (- h 1) 
							(list new-cfunction)
						)
					)
				)
			)
		)
		
		; construct and return
		(box (tree
			class0 fields-list readys-list children-list
		))
	)
)

(define (new-cstatement g w h)
	(let ([class0 (find-class (find-interface g 'Statement) 'CStatement)])
		(define fields-list (fresh-fields-list class0))
		(define readys-list (fresh-readys-list class0))
		(define children-list
			(if (<= h 0)
				; suggest shrinking
				(list
					(list
						'id
					)
					(list
						'assigned_expr
					)
					(list
						'next
					)
					(list
						'condition
					)
					(list
						'first_then_part
					)
					(list
						'first_else_part
					)
				)
				; not shrinking
				(list
					(list
						'id
						(new-choose-list 
							g w (- h 1) 
							(list new-cexpression)
						)
					)
					(list
						'assigned_expr
						(new-choose-list 
							g w (- h 1) 
							(list new-cexpression)
						)
					)
					(list
						'next
						(new-choose-list 
							g w (- h 1) 
							(list new-cstatement)
						)
					)
					(list
						'condition
						(new-choose-list 
							g w (- h 1) 
							(list new-cexpression)
						)
					)
					(list
						'first_then_part
						(new-choose-list 
							g w (- h 1) 
							(list new-cstatement)
						)
					)
					(list
						'first_else_part
						(new-choose-list 
							g w (- h 1) 
							(list new-cstatement)
						)
					)
				)
			)
		)
		
		; construct and return
		(box (tree
			class0 fields-list readys-list children-list
		))
	)
)

(define (new-cexpression g w h)
	(let ([class0 (find-class (find-interface g 'Expression) 'CExpression)])
		(define fields-list (fresh-fields-list class0))
		(define readys-list (fresh-readys-list class0))
		(define children-list
			(if (<= h 0)
				; suggest shrinking
				(list
					(list
						'lhs
					)
					(list
						'rhs
					)
				)
				; not shrinking
				(list
					(list
						'lhs
						(new-choose-list 
							g w (- h 1) 
							(list new-cexpression)
						)
					)
					(list
						'rhs
						(new-choose-list 
							g w (- h 1) 
							(list new-cexpression)
						)
					)
				)
			)
		)
		
		; construct and return
		(box (tree
			class0 fields-list readys-list children-list
		))
	)
)


(define (new-choose-list g w h cstrs)
	(apply choose*
		(flatten
			(for/list ([cstr cstrs])
				(for/list ([i (range (+ 1 w))])
					(box (for/list ([j (range i)]) (cstr g w h)))
				)
			)
		)
	)
)

