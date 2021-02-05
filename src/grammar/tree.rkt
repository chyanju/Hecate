#lang rosette

; Tree Data Structure

(require 
	(prefix-in xml: xml)
	"../utility.rkt"
	"./syntax.rkt"
)

(provide 
	(struct-out tree)
	inspect-tree
	tree-ref/field
	tree-ref/child
	tree-ref/ready
	; note: don't provide the original tree-select/* methods
	^tree-select/field
	^tree-select/ready
	tree-copy
	tree-size
	tree-annotate
	tree-validate
	tree-examples
)

(struct tree (class fields readys children) #:mutable #:transparent)
; (fixme) only works with one child
(define (inspect-tree arg-tree)
	(format "#~a"
		(append
			(list
				(ag:class-name (tree-class arg-tree))
			)
			(for/list ([c (tree-children arg-tree)])
				(if (list? c)
					; a list of children for given symbol
					(format ".~a=~a"
						(car c)
						(for/list ([z (cdr c)])
							(inspect-tree z)
						)
					)
					; single child for given symbol
					(format ".~a=~a"
						(car c)
						(inspect-tree (cdr c))
					)
				)
			)
		)
	)
)

(define (make-node class children)
	(define fields
		(for/list ([label (ag:class-labels* class)])
			(cons 
				(ag:label-name label) 
				(ag:slot (ag:label/in? label))
			)
		)
	)
	(define readys
		(for/list ([label (ag:class-labels* class)])
			(cons 
				(ag:label-name label) 
				; (ag:slot #f)
				(ag:slot
					(if (ag:label/in? label)
						; input modifier, don't care
						(void)
						; output modifier
						#f
					)
				)
			)
		)
	)
	(tree class fields readys children)
)

(define (tree-ref/field tree label)
	(^ass-ref (tree-fields tree) label)
)

(define (tree-ref/child tree name)
	(^ass-ref (tree-children tree) name)
)

(define (tree-ref/ready tree b)
	(^ass-ref (tree-readys tree) b)
)

(define (tree-select/field self attr #:iterator [iter #f] #:cursor [cur #f])
	(match attr
		[(cons 'self field)
			(tree-ref/field self field)
		]
		[(cons (== iter) field)
			(tree-ref/field cur field)
		]
		[(cons child field)
			(for/all ([tmp0 (tree-ref/child self child)])
				(define tmp1 (if (box? tmp0) (unbox tmp0) tmp0))
				(tree-ref/field tmp1 field)
			)
			; (tree-ref/field (tree-ref/child self child) field)
		]
		[_ (println-and-exit "# exception/tree-select/field: unsupported pattern ~a\n" attr)]
	)
)
; lifted version of tree-select
; note: usually attr is a tuple, e.g., (lk . puff) from #(struct:eval (lk . puff))
;       and this tuple can be symbolic (both car and cdr)
(define (^tree-select/field self attr #:iterator [iter #f] #:cursor [cur #f])
	(cond
		[(list? attr) (println-and-exit "# exception/tree-select/field: attr can't be list, got ~a\n" attr)]
		[(pair? attr)
			; note: the 1st and 2nd members can also be symbolic/union
			;       so another lifting is necessary
			(define a0 (car attr))
			(define a1 (cdr attr))
			(cond
				[(and (symbol? a0) (symbol? a1))
					; directly call the original method
					(tree-select/field self attr #:iterator iter #:cursor cur)
				]
				[(and (symbol? a0) (union? a1))
					(for/all ([a a1])
						(tree-select/field self (cons a0 a) #:iterator iter #:cursor cur)
					)
				]
				[(and (union? a0) (symbol? a1))
					(for/all ([a a0])
						(tree-select/field self (cons a a1) #:iterator iter #:cursor cur)
					)
				]
				[(and (union? a0) (union? a1))
					(for*/all ([i a0] [j a1])
						(tree-select/field self (cons i j) #:iterator iter #:cursor cur)
					)
				]
				[else (println-and-exit "# exception/tree-select/field: unsupported (a0 . a1), got ~a\n" attr)]
			)
		]
		[(union? attr)
			(for/all ([a attr])
				(^tree-select/field self attr #:iterator iter #:cursor cur)
			)
		]
		[else (println-and-exit "# exception/tree-select/field: unsupported attr ~a\n" attr)]
	)
)


(define (tree-select/ready self attr #:iterator [iter #f] #:cursor [cur #f])
	(match attr
		[(cons 'self field)
			(tree-ref/ready self field)
		]
		[(cons (== iter) field)
			(tree-ref/ready cur field)
		]
		[(cons child field)
			(for/all ([tmp0 (tree-ref/child self child)])
				(define tmp1 (if (box? tmp0) (unbox tmp0) tmp0))
				(tree-ref/ready tmp1 field)
			)
			; (tree-ref/ready (tree-ref/child self child) field)
		]
		[_ (println-and-exit "# exception/tree-select/ready: unsupported pattern ~a\n" attr)]
	)
)
; lifted version of tree-select
; note: usually attr is a tuple, e.g., (lk . puff) from #(struct:eval (lk . puff))
;       and this tuple can be symbolic (both car and cdr)
(define (^tree-select/ready self attr #:iterator [iter #f] #:cursor [cur #f])
	(cond
		[(list? attr) (println-and-exit "# exception/tree-select/ready: attr can't be list, got ~a\n" attr)]
		[(pair? attr)
			; note: the 1st and 2nd members can also be symbolic/union
			;       so another lifting is necessary
			(define a0 (car attr))
			(define a1 (cdr attr))
			(cond
				[(and (symbol? a0) (symbol? a1))
					; directly call the original method
					(tree-select/ready self attr #:iterator iter #:cursor cur)
				]
				[(and (symbol? a0) (union? a1))
					(for/all ([a a1])
						(tree-select/ready self (cons a0 a) #:iterator iter #:cursor cur)
					)
				]
				[(and (union? a0) (symbol? a1))
					(for/all ([a a0])
						(tree-select/ready self (cons a a1) #:iterator iter #:cursor cur)
					)
				]
				[(and (union? a0) (union? a1))
					(for*/all ([i a0] [j a1])
						(tree-select/ready self (cons i j) #:iterator iter #:cursor cur)
					)
				]
				[else (println-and-exit "# exception/tree-select/ready: unsupported (a0 . a1), got ~a\n" attr)]
			)
		]
		[(union? attr)
			(for/all ([a attr])
				(^tree-select/ready self attr #:iterator iter #:cursor cur)
			)
		]
		[else (println-and-exit "# exception/tree-select/ready: unsupported attr ~a\n" attr)]
	)
)

(define (tree-copy node)
	(define fields
		(for/list ([(label value) (^in-ass? (tree-fields node))])
			(cons 
				label 
				(ag:slot (ag:slot-v value))
			)
		)
	)
	(define children
		(for/list ([(name subtree) (^in-ass? (tree-children node))])
			(cons 
				name
				(if (list? subtree)
					(map tree-copy subtree)
					(tree-copy subtree)
				)
			)
		)
	)
	(define readys
		(for/list ([(label value) (^in-ass? (tree-readys node))])
			(cons 
				label 
				(ag:slot #f)
			)
		)
	)
	(tree (tree-class node) fields readys children)
)

(define (node-attributes node)
	(map ag:label-name (ag:class-labels* (tree-class node)))
)

; Annotate the tree with abstract attribute information.
(define (tree-annotate node)
	(define tmp0
		(make-node 
			(tree-class node)
			(dict-map-value 
				(tree-children node)
				(distribute tree-annotate)
			)
		)
	)
	tmp0
)

; Validate some property of every output attribute value.
(define (tree-validate tree check [verbose #t])
	(for ([p (tree-readys tree)])
		(define label (car p))
		(define value (cdr p))
		(when verbose
			(displayln `(check ,(ag:class-name (tree-class tree)) ,label))
		)
		(check value)
	)
	(for ([p (tree-children tree)])
		(define name (car p))
		(define subtree (cdr p))
		(if (list? subtree)
			(for ([node subtree])
				(tree-validate node check verbose)
			)
			(tree-validate subtree check verbose)
		)
	)
)

; Count the nodes in the tree.
(define (tree-size tree)
	(+ 
		(for*/sum ([child (tree-children tree)] [node (listify (cdr child))])
			(tree-size node)
		)
		1
	)
)

(define (build-leaves G)
	(for/list ([interface (ag:grammar-interfaces G)])
		(cons 
			(ag:interface-name interface)
			; TODO: What if every class of an interface is interior-only?
			(for/fold ([leaf-nodes null]) ([class (ag:interface-classes interface)])
				(match (ag:class-children* class)
					[(list (ag:child/seq names _) ...)
						(cons (tree class null null (map list names)) leaf-nodes)
					]
					[_ leaf-nodes]
				)
			)
		)
	)
)

(define (build-children G)
	(define leaf-variants (build-leaves G))
	(for/list ([interface (ag:grammar-interfaces G)])
		(define class-variants
			(for/list ([class (ag:interface-classes interface)])
				(define child-variants
					(for/list ([child (ag:class-children* class)])
						(match child
							[(ag:child/one name (ag:interface kind _ _))
								(map (curry cons name) (lookup leaf-variants kind))
							]
							[(ag:child/seq name (ag:interface kind _ _))
								(list (cons name (lookup leaf-variants kind)))
							]
						)
					)
				)
				(map (curry tree class null null)
				(apply cartesian-product child-variants))
			)
		)
		(cons (ag:interface-name interface) (append* class-variants))
	)
)


; Return a set of example tree skeletons that include every parent-child class
; pairing permitted by the grammar.
; (define (tree-examples G root)
;   (define variants (build-children G))
;   (define queue (list->mutable-seteq (ag:grammar-classes G)))

;   (define (construct class)
;     (define generate
;       ; (if (set-member? queue class)
;       ; (compose (curry append-map construct) ag:interface-classes)
;       ; (compose (curry lookup variants) ag:interface-name)))
;       (compose (curry lookup variants) ag:interface-name))
;     (set-remove! queue class)
;     (define children
;       (for/list ([child (ag:class-children* class)])
;         (match child
;           [(ag:child/one name interface)
;            (map (curry cons name) (generate interface))]
;           [(ag:child/seq name interface)
;            (let ([subtrees (generate interface)])
;              (list (cons name
;                          (append (list) subtrees))))])))
;     ;;; (pretty-display children)
;     ;;; (assert #f)
;     (map (curry tree class null) (apply cartesian-product children)))

;   (append-map construct (ag:interface-classes (ag:grammar-ref/interface G root))))

; quick hack version
(define (tree-examples G root)
  (define variants (build-children G))
  (define queue (list->mutable-seteq (ag:grammar-classes G)))

  (define (construct k class)
    (define generate
      (if (equal? k 0)
        (compose (curry lookup variants) ag:interface-name)
        (if (set-member? queue class)
          (compose (curry append-map (curry construct (- k 1))) ag:interface-classes)
          (compose (curry lookup variants) ag:interface-name)
        )
      )
    )
    (set-remove! queue class)
    (define children
      (for/list ([child (ag:class-children* class)])
        (match child
          [(ag:child/one name interface)
           (map (curry cons name) (generate interface))]
          [(ag:child/seq name interface)
           (let ([subtrees (generate interface)])
             (list (cons name
                         (append (list) subtrees)))
           )])))

    (map (curry tree class null null) (apply cartesian-product children)))

  (append-map (curry construct 2) (ag:interface-classes (ag:grammar-ref/interface G root))))