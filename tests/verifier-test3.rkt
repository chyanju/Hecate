#lang rosette
(require rosette/lib/angelic)
(struct container (content children) #:transparent)
(struct node (class) #:transparent)

(define (build-indent i) (build-string i (lambda (i) #\ )))
(define (format-cprogram p [i 0] [ap "root"])
	(format "~a.~a=<~a>\n~a"
		(build-indent i)
		ap
		(format "~v" (node-class (container-content p)))
		(string-join
			(for/list ([q (container-children p)])
				(format-cprogram (cdr q) (+ i 2) (format "~v" (car q)))
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
				(choose*
					(cons "first_element" (new-clist width (- height 1) new-cfunction))
					(cons "first_element" (new-clist width (- height 1) new-cstatement))
				)
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
				(cons "nextA" (new-clist width (- height 1) new-cfunction))
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
				(cons "nextB" (new-clist width (- height 1) new-cstatement))
			)
		)
	)
)


(define tree0 (new-cprogram 2 3))
(printf "~a\n" (format-cprogram tree0))