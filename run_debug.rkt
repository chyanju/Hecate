#!/usr/bin/env racket
#lang rosette

; Script to run the synthesizer on a given attribute grammar.

(require racket/cmdline
         racket/pretty
         "src/grammar/parse.rkt"
         "src/grammar/validate.rkt"
         "src/grammar/syntax.rkt"
         "src/grammar/tree.rkt"
         "src/schedule/parse.rkt"
         ; "src/tracing/synthesizer.rkt"
         "src/checking/synthesizer.rkt"
         "src/backend/generate.rkt"
         "src/backend/printer.rkt"
)

(define *root* (make-parameter 'Root))
(define *output* (make-parameter "browser/src/layout.rs"))

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

(define classname "Node")
(*root* (string->symbol classname))
(define schedule-sketch "fusion")
(define grammar-filename "./benchmarks/molly/molly0.grammar")

; (printf "> schedule sketch is:\n~a\n" schedule-sketch)
(define G (parse-grammar grammar-filename))
; (printf "> Grammar is:\n~a\n" G)
(define E (tree-examples G (*root*)))
; (for ([e E])
; 	(printf "> Tree is:\n~a\n" (inspect-tree e))
; )
(define S (parse-schedule-sketch G schedule-sketch))
; (printf "> Schedule is:\n~a\n" S)
(define S* (complete-sketch G S E))
; (printf "> Done synthesis.\n")
; (printf "> Complete schedule is:\n~a\n" S*)
(when S*
	(define DS* (schedule->string S*))
	(displayln DS*)
	(define P (generate-program G S*))
	(define file (open-output-file (*output*) #:mode 'text #:exists 'replace))
	(parameterize ([current-output-port file]) (print-program P))
)

