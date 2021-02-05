#!/usr/bin/env racket
#lang rosette

; Script to run the synthesizer on a given attribute grammar.

(require 
	racket/cmdline
	racket/pretty
	"./src/checking.rkt"
	"./src/utility.rkt"
	"./src/grammar/parse.rkt"
	"./src/grammar/validate.rkt"
	"./src/grammar/syntax.rkt"
	"./src/grammar/tree.rkt"
	"./src/schedule/parse.rkt"
	"./src/schedule/enumerate.rkt"
	; "./benchmarks/grafter/hv-toy.rkt"
	; "./benchmarks/grafter/hv-toy-list.rkt"
	; "./benchmarks/grafter/piecewise-exp1.rkt"
	; "./benchmarks/grafter/binary-tree.rkt"
	; "./benchmarks/grafter/fmm.rkt"
	; "./benchmarks/grafter/render.rkt"
	"./benchmarks/grafter/ast.rkt"

	"./src/vchecking.rkt"
)

(define debug-solution null)

(define (parse-grammar filename)
	(define G (file->grammar filename))
	(validate-grammar G)
	G
)

; FIXME: This is a horrid hack and only supports sequential schedule sketches.
(define (parse-schedule-sketch G S0)
	(define traversals 
		(reverse
			(map 
				(compose ag:traverse string->symbol)
				(filter 
					non-empty-string? 
					(map 
						string-trim 
						(string-split S0 ";")
					)
				)
			)
		)
	)
	(foldr ag:sequential (first traversals) (rest traversals))
)

; validation function for tree-validate
; assert the current attribute is ready
(define (validate-fn arg-slot)
	; (printf "> validate-fn: ~a\n" arg-slot)
	(assert (ag:slot-v arg-slot))
)

(define (verify-fn arg-slot)
	; (printf "> validate-fn: ~a\n" arg-slot)
	; (printf "a\n")
	(define sss (verify (assert (ag:slot-v arg-slot))))
	; (printf "val: ~v\n" (ag:slot-v arg-slot))
	(when (unsat? sss) (set! debug-solution sss))
	; (printf "b\n")
	(printf "sss: ~a\n" sss)
)

; (define classname "HVBox")
; (define rootname (string->symbol classname))
; (define schedule-sketch "fuse")
; (define grammar-filename "./benchmarks/grafter/hv-toy.grammar")

; (define classname "HVBox")
; (define rootname (string->symbol classname))
; (define schedule-sketch "fuse")
; (define grammar-filename "./benchmarks/grafter/hv-toy-list.grammar")

; (define classname "VirtualRoot")
; (define rootname (string->symbol classname))
; (define schedule-sketch "fuse")
; (define grammar-filename "./benchmarks/grafter/piecewise-exp3.grammar")

; (define classname "Root")
; (define rootname (string->symbol classname))
; (define schedule-sketch "fuse")
; (define grammar-filename "./benchmarks/grafter/binary-tree.grammar")

; (define classname "VirtualRoot")
; (define rootname (string->symbol classname))
; (define schedule-sketch "fuse")
; (define grammar-filename "./benchmarks/grafter/fmm.grammar")

; (define classname "Document")
; (define rootname (string->symbol classname))
; (define schedule-sketch "fuse")
; (define grammar-filename "./benchmarks/grafter/render.grammar")

(define classname "Program")
(define rootname (string->symbol classname))
(define schedule-sketch "fuse")
(define grammar-filename "./benchmarks/grafter/ast.grammar")

; G: grammar
(define G (parse-grammar grammar-filename))
; (printf "> grammar is:\n~a\n" G)

; E: tree set
(define E (tree-examples G rootname))
(printf "> generated ~a tree examples\n" (length E))
; (for ([e E])
	; e: (struct tree (class fields readys children) #:mutable #:transparent)
	;  | "fields" "readys" "children" are currently null
	;  | e.g., #(struct:tree #<class> () () ())
	; (printf "> tree is:\n~a\n" (inspect-tree e))
; )
; (printf "> last tree is:\n~a\n" (list-ref (reverse E) 0))

; S: #(struct:traverse fusion)
;  | this is just a invocation
;  | e.g., #(struct:traverse fusion)
(define S (parse-schedule-sketch G schedule-sketch))
; (printf "> S is:\n~a\n" S)

; schedule: (struct traversal (name visitors) #:transparent)
;         | now it becomes a definition
;         | e.g., #(struct:traversal 
;                   fusion
;                   (
;                     #(struct:visitor 
;                       #<class> 
;                       (
;                         #(struct:recur lk) 
;                         #(struct:recur rk) 
;                         (choose 
;                           #(struct:eval (self . puff)) 
;                           #(struct:eval (self . pie))
;                         )
;                       )
;                     ) 
;                     #(struct:visitor 
;                       #<class> 
;                       (
;                         (choose 
;                           #(struct:eval (self . puff)) 
;                           #(struct:eval (self . pie))
;                         )
;                       )
;                     )
;                   )
;                 )
(define schedule (instantiate-sketch G S))
; (printf "> schedule is:\n~a\n" schedule)

(for ([e (reverse E)])
; (for ([e (cdr (reverse E))])
	; ae: (struct tree (class fields readys children) #:mutable #:transparent)
	;   | all struct members are currently filled
	;   | e.g., #(struct:tree 
	;             #<class> 
	;             (
	;               (puff . #(struct:slot #f)) 
	;               (pie . #(struct:slot #f))
	;             ) 
	;             (
	;               (puff . #(struct:slot #f)) 
	;               (pie . #(struct:slot #f))
	;             )
	;             () --> no children
	;           )
	; (printf "> tree is:\n~a\n" (inspect-tree e))
	
	(define ae (tree-annotate e))
	; (printf "> annotated tree is:\n~a\n" ae)

	; then start the interpretation
	(interpret schedule ae)
	; (printf "> interpreted tree is:\n~a\n" ae)

	; validate is reading all attributes
	(tree-validate ae validate-fn)


	; (pause)
)

(define sol (solve (assert #t)))
(if (sat? sol)
	(begin
		(printf "> SAT\n")
		(printf (schedule->string schedule idict sol))
		(printf "\n")
	)
	(begin
		(printf "> UNSAT\n")
	)
)

; =====================================
; ===== verifier-specific helpers =====
; =====================================

(define (concretize-schedule sched sdict sol)
	(match sched
		[(ag:traversal order visitors)
			(ag:traversal order (for/list ([v visitors]) (concretize-visitor v sdict sol)))
		]
	)
)

(define (concretize-visitor visitor sdict sol)
	(match visitor
		[(ag:visitor class commands)
			(ag:visitor class (flatten (for/list ([c commands]) (concretize-command c sdict sol))))
		]
	)
)

(define (concretize-command command sdict sol)
	(match command
		[(ag:iter/left child commands)
			(ag:iter/left child (flatten (for/list ([c commands]) (concretize-command c sdict sol))))
		]
		[(ag:iter/right child commands)
			(ag:iter/right child (flatten (for/list ([c commands]) (concretize-command c sdict sol))))
		]
		[(ag:recur child)
			(ag:recur child)
		]
		[(ag:eval (cons node label))
			(ag:eval (cons node label))
		]
		[(ag:hole) (ag:hole)]
		[(ag:skip) (ag:skip)]
		[(list 'multichoose nth vs ...)
			(if (hash-has-key? sdict nth)
				; this evaluates to a list, need to be flattened from parent receiver
				; (list-ref (evaluate (hash-ref sdict nth) sol) 0)
				(evaluate (hash-ref sdict nth) sol)
				; can't find key --> didn't traverse to this hole
				; --> the solution could be wrong
				(ag:skip)
			)
		]
	)
)

(define (tree-reset tree)
	(define fields-list (fresh-fields-list (tree-class tree)))
	(define readys-list (fresh-readys-list (tree-class tree)))
	(set-tree-readys! tree readys-list)
	(set-tree-fields! tree fields-list)
	(for ([p (tree-children tree)])
		(define name (car p))
		(define subtree (cdr p))
		(if (list? subtree)
			(for ([node subtree])
				(tree-reset node)
			)
			(tree-reset subtree)
		)
	)
)

; =====================================
; testing: assuming solution is sat
(clear-asserts!)
(define cs (concretize-schedule schedule idict sol))
(define st (build-symbolic-example G 1 6))

(v:interpret cs st)
(v:tree-validate st verify-fn)

; (define vtrees (unroll-symbolic-example st))
; (printf "> verifying on ~a trees\n" (length vtrees))
; ; (for ([t0 vtrees])
; (for ([i (range (length vtrees))])
; ; (for ([i (range 2)])
; 	(printf "> verifying tree ~a\n" i)
; 	(define t0 (list-ref vtrees i))
; 	; (printf "  > tree-reset\n")
; 	(tree-reset t0)
; 	; (printf "  > interpret\n")
; 	(interpret cs t0)
; 	; (printf "  > validate\n")
; 	(tree-validate t0 validate-fn #f)
; )

