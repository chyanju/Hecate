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
	(if (union? t)
		; union
		(for/fold ([accu (list)]) ([t0 (union-contents t)])
			(append accu (unroll-symbolic-example (cdr t0)))
		)
		; box
		(let ([t0 (unbox t)])
			(if (list? t0)
				; children is a list
				(println-and-exit "not implemented.")
				; children is a tree
				(begin
					(define cl
						(for/list ([p0 (tree-children t0)])
							(define tmp0 (unroll-symbolic-example (cdr p0)))
							(for/list ([j0 tmp0])
								(cons (car p0) j0)
							)
						)
					)
					; cp: list of all potential tree-children
					(define cp (apply cartesian-product cl))
					; duplicate t0 for different children probabilities
					(for/list ([c cp])
						(tree
							(ag:class-name (tree-class t0))
							null
							null
							c
						)
						; (tree
						; 	(tree-class t0)
						; 	(tree-fields t0)
						; 	(tree-readys t0)
						; 	c
						; )
					)
				)
			)
		)
	)
)

; g: grammar, w: width, h: height
(define (build-symbolic-example g w h)
	; h >= 1
	(choose*
		(new-leaf g w (- h 1))
		(new-hbox g w (- h 1))
		; don't consider empty example here
	)
)

(define (new-hbox g w h)
	(let ([class0 (find-class (find-interface g 'HVBox) 'HBox)])
		(define fields-list (fresh-fields-list class0))
		(define readys-list (fresh-readys-list class0))
		(define children-list
			(if (<= h 0)
				; suggest shrinking
				(list
					(cons 
						'l
						(new-leaf g w (- h 1))
					)
					(cons 
						'r
						(new-leaf g w (- h 1))
					)
				)
				; not shrinking
				(list
					(cons 
						'l
						(choose* 
							(new-leaf g w (- h 1))
							(new-hbox g w (- h 1))
						)
					)
					(cons 
						'r
						(choose* 
							(new-leaf g w (- h 1))
							(new-hbox g w (- h 1))
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

(define (new-leaf g w h)
	(let ([class0 (find-class (find-interface g 'HVBox) 'Leaf)])
		(define fields-list (fresh-fields-list class0))
		(define readys-list (fresh-readys-list class0))
		(define children-list null)
		; construct and return
		(box (tree
			class0 fields-list readys-list children-list
		))
	)
)

