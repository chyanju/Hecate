#lang rosette

; Tracing synthesizer for language of tree traversal schedules

(require "../schedule/enumerate.rkt"
		 "../grammar/syntax.rkt"
		 "../grammar/tree.rkt"
		 "../utility.rkt"
		 "../trace.rkt"
		 "interpreter.rkt"
)
(provide complete-sketch)

(define substitute 0)

(define/match (concretize* S)
	[((ag:sequential sched-1 sched-2))
		(ag:sequential (concretize* sched-1) (concretize* sched-2))]
	[((ag:parallel sched-1 sched-2))
		(ag:parallel (concretize* sched-1) (concretize* sched-2))]
	[((ag:traversal order visitor-list))
		(ag:traversal order (map concretize* visitor-list))]
	[((ag:visitor class command-list))
		(ag:visitor class (substitute command-list))]
)

(define (permute* . xs) (permute (length xs) xs))

(define (complete-sketch G sketch examples)
  (parameterize ([*box* ref]
				 [*box?* ref?]
				 [*unbox* deref]
				 [*set-box!* set-ref!]
				 [*denotation* abstract-denotation]
				 [*multichoose* permute*])

	(define schedule (instantiate-sketch G sketch))
	; (printf ">>> schedule is:\n~a\n" schedule)

	(define initial-time (current-milliseconds))

	(for ([tree examples])
	  ; (printf "==========================\n")
	  (let ([tree (tree-annotate tree)])
	  	; (printf ">>> annotated tree is:\n~a\n" tree)
		(interpret schedule tree)
		; (printf ">>> interpreted tree is:\n~a\n" tree)
		; (printf ">>> interpreted schedule is:\n~a\n" schedule)
		(tree-validate tree deref)
		(break)))

	(match-let-values ([(running-time) (- (current-milliseconds) initial-time)]
					   [(nodes variables) (formula-size)]
					   [((list concretize) overhead-time solving-time _)
						(time-apply solve null)])
		; (printf ">>>>>> concretize is:~a\n" concretize)
	  (displayln (if concretize "SAT" "UNSAT"))
	  (printf "Symbolic Evaluation: ~ams\n" (+ running-time overhead-time))
	  (printf "Constraint Solving: ~ams\n" (- solving-time overhead-time))
	  (printf "Constraint Size: ~a nodes and ~a variables\n" nodes variables)
	  (when concretize
		(set! substitute concretize)
		(concretize* schedule))))
)
