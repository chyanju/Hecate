#lang rosette
(require rosette/lib/angelic)
(struct container (content children) #:transparent)
(struct node (class) #:transparent)

(define tree0
	(container
		(node "Program")
		(list
			(container
				(node "Function")
				(list)
			)
			(container
				(node "Function")
				(list)
			)
		)
	)
)

(define tree1
	(container
		(choose* (node "Program") (node "Function"))
		(list
			(container
				(choose* (node "Program") (node "Function"))
				(list)
			)
			(container
				(choose* (node "Program") (node "Function"))
				(list)
			)
		)
	)
)

(define tree2
	(container
		(choose* (node 'Program) (node 'Function))
		(list
			(container
				(choose* (node 'Program) (node 'Function))
				(list)
			)
			(container
				(choose* (node 'Program) (node 'Function))
				(list)
			)
		)
	)
)