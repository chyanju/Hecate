#lang rosette
(require rosette/lib/angelic)
(struct container (content children) #:transparent)
(struct node (class) #:transparent)

(define (build-indent i) (build-string i (lambda (i) #\ )))
(define (format-cprogram p [i 0] [ap "root"])
	(format "~a.~a=<~a>\n~a"
		(build-indent i)
		ap
		(node-class (container-content p))
		(string-join
			(for/list ([q (container-children p)])
				(format-cprogram (cdr q) (+ i 2) (car q))
			)
			""
		)
	)
)

(define (new-end)
	(container
		(node "cend")
		(list)
	)
)

(define (new-cprogram width height)
	(if (<= height 0)
		(new-end)
		(container
			(node "cprogram")
			(list 
				(cons "first_function" (new-clist width (- height 1) new-cfunction))
			)
		)
	)
)

(define (new-clist width height gen)
	(if (<= height 0)
		(new-end)
		(container
			(node "clist")
			(for/list ([i (range width)])
				(cons i (gen width height))
			)
		)
	)
)

(define (new-cfunction width height)
	(if (<= height 0)
		(new-end)
		(container
			(node "cfunction")
			(list
				(cons "next" (new-clist width (- height 1) new-cfunction))
				(cons "first_statement" (new-clist width (- height 1) new-cstatement))
			)
		)
	)
)

(define (new-cstatement width height)
	(if (<= height 0)
		(new-end)
		(container
			(node "cstatement")
			(list
				(cons "id" (new-clist width (- height 1) new-cexpression))
				(cons "assigned_expr" (new-clist width (- height 1) new-cexpression))
			)
		)
	)
)

(define (new-cexpression width height)
	(if (<= height 0)
		(new-end)
		(container
			(node "cexpression")
			(list
				(cons "lhs" (new-clist width (- height 1) new-cexpression))
				(cons "rhs" (new-clist width (- height 1) new-cexpression))
			)
		)
	)
)

(define tree0 (new-cprogram 2 3))
(printf "~a\n" (format-cprogram tree0))