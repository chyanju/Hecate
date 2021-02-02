#lang rosette

; Tree Data Structure

(require (prefix-in xml: xml)
         "../utility.rkt"
         "syntax.rkt")

(provide (struct-out tree)
         (rename-out [*ref?* *box?*]
                     [*ref* *box*]
                     [*deref* *unbox*]
                     [*set-ref!* *set-box!*]
                     [ref? box?]
                     [ref box]
                     [deref unbox]
                     [set-ref! set-box!])
         inspect-tree
         tree-ref/field
         tree-ref/child
         tree-select
         tree-copy
         xml->tree
         xexpr->tree
         file->tree
         tree-size
         tree-annotate
         tree-validate
         tree-examples)

(define *ref?* (make-parameter box?))
(define *ref* (make-parameter box))
(define *deref* (make-parameter unbox))
(define *set-ref!* (make-parameter set-box!))

(define (ref? b) ((*ref?*) b))
(define (ref v) ((*ref*) v))
(define (deref b) ((*deref*) b))
(define (set-ref! b v) ((*set-ref!*) b v))

; TODO: Maybe represent fields with a prefix tree.
(struct tree (class fields children) #:transparent)
(define (inspect-tree arg-tree)
  (if (list? arg-tree)
    ; then
    (format "~a"
      (for/list ([t arg-tree]) (inspect-tree))
    )
    ; else
    (format "~a"
      (append
        (list
          (ag:class-name (tree-class arg-tree))
        )
        (for/list ([c (tree-children arg-tree)]) 
          (format "~a=~a"
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
      (cons (ag:label-name label) (ref (ag:label/in? label)))))

  (tree class fields children))

(define (tree-ref/field tree label)
  (dict-ref (tree-fields tree) label))

(define (tree-ref/child tree name)
  (dict-ref (tree-children tree) name))

(define (tree-select self attr #:iterator [iter #f] #:cursor [cur #f])
  (match attr
    [(cons 'self field)
     (tree-ref/field self field)]
    [(cons (== iter) field)
     (tree-ref/field cur field)]
    [(cons child field)
     (tree-ref/field (tree-ref/child self child) field)]))

(define (tree-copy node)
  (define fields
    (for/list ([(label value) (in-dict (tree-fields node))])
      (cons label (ref (deref value)))))

  (define children
    (for/list ([(name subtree) (in-dict (tree-children node))])
      (cons name
            (if (list? subtree)
                (map tree-copy subtree)
                (tree-copy subtree)))))

  (tree (tree-class node) fields children))

; derive a quoted instance of an FTL tree structure for a grammar in IR form
; given an XML string
(define (xml->tree G xml-string)
  (xexpr->tree G (xml:string->xexpr xml-string)))

; Convert the X-expression tree into the internal tree representation. Tags are
; interpreted as classnames, and attribute values are parsed as Racket-serialized
; values (i.e., parsed with Racket's read). Children are assigned to the child
; labels in the order given by the grammar, with a sequence child consuming all
; remaining child X-expressions. Therefore, this procedure only supports
; attribute grammars whose classes have at most one sequence child as their last-
; declared child.
(define (xexpr->tree G xexpr)
  (let ([recur (curry xexpr->tree G)])
    (match xexpr
      [(list-rest class-name
                  (and xattributes (list (list (? symbol?) (? string?)) ...))
                  xcontent)

       (define class (ag:grammar-ref/class G class-name))

       (define children
         (let ([subtrees (map recur (filter list? xcontent))])
           (match (ag:class-children* class)
             [(list (ag:child/one names _) ...)
              (map cons names subtrees)]
             [(list (ag:child/seq name _))
              (cons name subtrees)])))

       (define fields
         (for/list ([label (ag:class-labels* class)])
           (cons (ag:label-name label) (ref #f))))

       (for ([(label value) (in-dict xattributes)])
         (let ([value (read (open-input-string (first value)))])
           (set-ref! (dict-ref fields label) value)))

       (tree class fields children)]

      [(list-rest class-name children)
       (recur (list class-name null children))]

      [_
       (raise-arguments-error 'xexpr->tree
                              "could not convert X-expression to tree"
                              'xexpr xexpr)])))

(define (file->tree G filename)
  (xml->tree G (file->string filename #:mode 'text)))

(define (node-attributes node)
  (map ag:label-name (ag:class-labels* (tree-class node))))

; Annotate the tree with abstract attribute information.
(define (tree-annotate node)
  (make-node (tree-class node)
             (dict-map-value (tree-children node)
                             (distribute tree-annotate)
             )
  )
)

; Validate some property of every output attribute value.
(define (tree-validate tree check)
  (for ([(label value) (in-dict (tree-fields tree))])
    (displayln `(check ,(ag:class-name (tree-class tree)) ,label))
    (check value))
  (for ([(name subtree) (in-dict (tree-children tree))])
    (if (list? subtree)
        (for ([node subtree])
          (tree-validate node check))
        (tree-validate subtree check))))

; Count the nodes in the tree.
(define (tree-size tree)
  (+ (for*/sum ([child (tree-children tree)]
                [node (listify (cdr child))])
       (tree-size node))
     1))

(define (build-leaves G)
  (for/list ([interface (ag:grammar-interfaces G)])
    (cons (ag:interface-name interface)
          ; TODO: What if every class of an interface is interior-only?
          (for/fold ([leaf-nodes null])
                    ([class (ag:interface-classes interface)])
            (match (ag:class-children* class)
              [(list (ag:child/seq names _) ...)
               (cons (tree class null (map list names)) leaf-nodes)]
              [_
               leaf-nodes])))))

(define (build-children G)
  (define leaf-variants (build-leaves G))
  (for/list ([interface (ag:grammar-interfaces G)])
    (define class-variants
      (for/list ([class (ag:interface-classes interface)])
        (define child-variants
          (for/list ([child (ag:class-children* class)])
            (match child
              [(ag:child/one name (ag:interface kind _ _))
               (map (curry cons name) (lookup leaf-variants kind))]
              [(ag:child/seq name (ag:interface kind _ _))
               (list (cons name (lookup leaf-variants kind)))])))
        (map (curry tree class null)
             (apply cartesian-product child-variants))))
    (cons (ag:interface-name interface) (append* class-variants))))

; TODO: Complete this cleaner version of tree-examples
(define (tree-examples* G root)
  (define queue
    (for/hasheq ([interface (ag:grammar-interfaces G)])
      (values interface
              (list->mutable-seteq (ag:interface-classes interface)))))
  (define (dequeue! class)
    (set-remove! (hash-ref queue (ag:class-interface class)) class))

  (define forest (make-hasheq))
  (define (plant! node)
    (hash-update! forest (tree-class node) (curry cons node) null))

  (for ([class (ag:grammar-classes G)]
        #:when (andmap ag:child/seq? (ag:class-children* class)))
    (let ([children (ag:class-children* class)])
      (plant! (tree class null (map (compose list ag:child-name) children)))
      (when (null? children)
        (dequeue! class))))

  ; until queue empty do:
  ;   find class in queue s.t. children class-saturated in forest:
  ;   construct nodes of class for cartesian product of child classes
  ;   // always pick shortest or tallest?
  ;   // once a node is adopted, there's no sense in duplicating it elsewhere.
  ;   // how can we optimize for many short trees rather than a few very tall trees?
  ;   insert nodes into forest
  ;   remove class from queue
  #f)

; Return a set of example tree skeletons that include every parent-child class
; pairing permitted by the grammar.
(define (tree-examples G root)
  (define variants (build-children G))
  (define queue (list->mutable-seteq (ag:grammar-classes G)))

  (define (construct class)
    (define generate
      ;;; (if (set-member? queue class)
          ;;; (compose (curry append-map construct) ag:interface-classes))
          (compose (curry lookup variants) ag:interface-name))
    (set-remove! queue class)
    (define children
      (for/list ([child (ag:class-children* class)])
        (match child
          [(ag:child/one name interface)
           (map (curry cons name) (generate interface))]
          [(ag:child/seq name interface)
           (let ([subtrees (generate interface)])
             (list (cons name
                         (append (list) subtrees))))])))
    ;;; (pretty-display children)
    ;;; (assert #f)
    (map (curry tree class null) (apply cartesian-product children)))

  (append-map construct (ag:interface-classes (ag:grammar-ref/interface G root))))
