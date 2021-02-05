#lang rosette
(require rosette/lib/angelic)
(struct container (content children) #:transparent)
(struct node (class) #:transparent)

(define (new-program width)
	(container
		(node "program")
		(for/list ([i (range width)])
			(choose* (new-function width) (new-procedure width))
		)
	)
)

(define (new-function width)
	(container
		(node "function")
		(for/list ([i (range width)])
			(new-statement)
		)
	)
)

(define (new-procedure width)
	(container
		(node "procedure")
		(for/list ([i (range width)])
			(new-statement)
		)
	)
)

(define (new-statement)
	(container
		(node "statement")
		(list)
	)
)

(define tree0 (new-program 2))