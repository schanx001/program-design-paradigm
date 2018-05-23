;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Simplified Garter Snake program to determine infinite loop in list of definitions.
;; Referred reachability.rkt example.

(require "sets.rkt")
(require "extras.rkt")
(require rackunit)

(provide make-def
         make-varexp
         make-appexp
         any-loops?)

(check-location "08" "q1.rkt")

;; DATA DEFINITIONS:

;; A Program is a ListOfDefinition.

(define-struct def (name args body))
;; A Definition is a (make-def Variable ListOfVariable Exp)
;; INTERPRETATION:
;; name is the name of the function being defined
;; args is the list of arguments of the function
;; body is the body of the function.

;; TEMPLATE:
;; def-fn : Definition -> ?
;; (define (def-fn d)
;;  (...(def-name d)
;;      (def-args d)
;;      (def-body d)))

;; A ListOfDefinition(LOD) is one of:
;; --empty
;; --(cons Definition ListOfDefinition)

;; template:
;; lod-fn: LOD -> ??
;; (define (lod-fn lod)
;;   (cond
;;     [(empty? lod) lod]
;;     [else (...(first lod)
;;              (lod-fn (rest lod)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct varexp (name))

;; A Varexp is a (make-varexp Variable)
;; INTERPRETATION:
;; name is the name of the variable

;; TEMPLATE:
;; varexp-fn : Varexp -> ??
;; (define (varexp-fn v)
;;  (...(varexp-name v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct appexp (fn args))

;; An Appexp is a (make-appexp Variable ListOfExp)
;; INTERPRETATION:
;; fn is the name of the variable
;; args is the ListOfExp

;; TEMPLATE:
;; appexp-fn : Appexp -> ??
;; (define (appexp-fn e)
;;  (...(appexp-fn e)
;;       (appexp-args e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en

;; A ListOfExp(LOE) is one of:
;; -- empty
;; -- (cons Exp ListOfExp)

;; INTERP:
;; empty             -- represents the sequence with no Exp
;; (cons exp loe)   -- represents the sequence whose first element is exp
;;                      and the rest of sequence is represented by loe
;;
;; TEMPLATE:
;; loe-fn : LOE-> ??
;; (define (loe-fn loe
;;   (cond
;;     [(empty? loe) ...]
;;     [else ... (first loe)
;;                   (loe-fn (rest loe))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Variable is a Symbol.

;; A ListOfVariable is one of:
;; --empty
;; (cons Variable LOV)
;;
;; template:
;; lov-fn: LOV -> ??
;; (define (lov-fn lov)
;;	(cond
;; 	[(empty? lov) empty]
;;	        [else (... (first lov)
;;	             	(lov-fn (rest lov)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Node is a Symbol

;; ListOfNode is one of:
;; --empty
;; --(cons Node ListOfNode)

;; template:

;; lon-fn: LON -> ??
;; (define (lon-fn lon)
;;  (cond
;;    [(empty? lon) lon]
;;    [else (...(first lon)
;;              (lon-fn (rest lon)))]))

;; SetOfNode is a ListOfNode with no repeats.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct edge (from to))

;; An Edge is a (make-edge Node Node)
;; Interpretation:
;; from is the source node in the graph
;; to is the destination node in the graph

;; template
;; (define (edge-fn e)
;;  (... (edge-from e)
;;       (edge-to e)))

;; ListOfEdge(LOE) is one of:
;; --empty
;; --(cons Edge ListOfEdge)

;; template:
;; loe-fn: LOE -> ??
;; (define (loe-fn loe)
;;	(cond
;; 	[(empty? loe) empty]
;;	        [else (... (first loe)
;;	             	(loe-fn (rest loe)))]))

;; A Graph is ListOfEdge with no repeats.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END OF DATA DEFINITIONS ;;;;;;;;;;;;;;;;;;;;;;

;; any-loops? : Program -> Boolean
;; GIVEN: a valid SGS program p (that is, a GS program that obeys the
;; restrictions listed above).
;; RETURNS: true iff there is some function f in p that calls itself
;; either directly or indirectly, as in the example below.
;; EXAMPLES:
;; (any-loops?
;; (list (make-def 'f1 (list 'x) (make-appexp 'f1 empty)))) = true
;; STRATEGY: Using HOF ormap on program
;; HALTING MEASURE: (length program)

(define (any-loops? program)
  (ormap
   ;; Definition -> Boolean
   ;; Returns: true iff only any one of def leads to itself using the graph for
   ;; the program
   (lambda (df) (check-loops? (def-name df) (create-graph program)))
   program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-loops? : Definition ListOfEdge -> Boolean
;; GIVEN: a definition and a list of edge 
;; RETURNS: true iff the given definition leads to itself
;; while traversing the list of edges (i.e graph)
;; EXAMPLES: (check-loops? 'f1 (create-graph some-loops)) -> #false
;; DESIGN STRATEGY: Initiating invariants (recent and reached) for
;;                  reaching-itself? function

(define (check-loops? src g)
  (reaching-itself? empty (list src) src g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create-graph: Program -> Graph
;; GIVEN: a program
;; RETURNS: a graph with list of edge (of only appexps, no varexp required) 
;; EXAMPLES: (create-graph
;; (list (make-def 'f1 (list 'x) (make-appexp 'f1 empty)))) =
;;  (list (make-edge 'f1 'f1))
;; DESIGN STRATEGY: Using HOF foldr on program
;; HALTING MEASURE: (length program)
 
(define (create-graph program)
  (foldr
   ;; Definition ListOfEdge -> ListOfEdge
   ;; Returns: a list of edge 
   (lambda (df loe) (if (varexp? (def-body df))
                       loe
                       (append (create-an-edge (def-name df) (def-body df)) loe)))
   empty
  program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create-an-edge: Variable Exp -> ListOfEdge
;; GIVEN: a variable and an expression
;; RETURNS: a list of edges with all of its children appexps included
;; EXAMPLES:(create-an-edge 'f1 (make-appexp 'no-loop (list (make-varexp 'x))))
;; = (list (make-edge 'f1 'no-loop))
;; (create-an-edge 'f1 (make-appexp 'no-loop (list (make-appexp 'f2 empty))))
;; = (list (make-edge 'f1 'no-loop) (make-edge 'f1 'f2))
;; DESIGN STRATEGY: Using template for Appexp on exp

(define (create-an-edge name exp)
  (append (list (make-edge name (appexp-fn exp)))
   (get-a-loe name (appexp-args exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-a-loe: Variable ListOfExp -> ListOfEdge
;; GIVEN: a variable name and a list of expresssion
;; WHERE: a list of expression is the args of the outer appexp within the body
;;        of a def
;; RETURNS: a list of expressions consisting of appexps
;;          within the definition's appexp
;; EXAMPLES: Refer tests
;; DESIGN STRATEGY: using HOF foldr on (get-appexp argslist)
;; HALTING MEASURE: (length argslist)

(define (get-a-loe name argslist)
  (foldr
   ;; Exp ListOfEdge -> ListOfEdge
   ;; Returns: a list of edges
   (lambda (x y) (cons (make-edge name (appexp-fn x)) y))
   empty
   (get-appexp argslist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-appexp: ListOfExp -> ListOfExp
;; GIVEN: a list of exp
;; WHERE: a list of exp consists of both appexp and varexp of the outer appexp
;; RETURNS: a list of exp which consists of appexp only
;; EXAMPLES: Refer tests
;; DESIGN STRATEGY: Using HOF foldr on argslist
;; HALTING MEASURE: (length loe)

(define (get-appexp argslist)
  (foldr
   ;; Exp ListOfExp -> ListOfExp
   ;; Returns: List of exp with appexps only
   (lambda (x y) (if (varexp? x)
                     y
                     (append (cons x y)
                             (get-args (appexp-args x)))))
   empty
   argslist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; get-args: ListOfExp -> ListOfExp 
;; GIVEN: a list of exp 
;; WHERE: a list of exp consists of both appexp and varexp of the
;; inner list of arguments of an appexp
;; RETURNS: a list of exp which consists of appexp only
;; EXAMPLES: Refer tests
;; DESIGN STRATEGY: Using HOF foldr on argslist
;; HALTING MEASURE: (length loe)

(define (get-args loe)
  (foldr
   ;; Exp ListOfExp -> ListOfExp
   ;; Returns: a list of exp consisting of appexp only 
   (lambda (x y) (if (varexp? x)
                    y
                    (cons x
                          (get-appexp (appexp-args x)))))
   empty
   loe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; node: Node Node -> Boolean
;; GIVEN: a two nodes
;; RETURNS: true iff the two nodes are equal
;; EXAMPLES: (node=? 'f1 'f1) = #true
;; DESIGN STRATEGY: Combine using simpler functions

(define (node=? n1 n2) (symbol=? n1 n2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-immediate-nodes: Node Graph -> ListofNode
;; GIVEN:a node and a graph 
;; RETURNS: list of node
;; EXAMPLES: Refer tests
;; DESIGN STRATEGY: using HOF map on loe

(define (get-immediate-nodes n1 loe)
  (map 
   edge-to  
   (filter
    ;; Edge -> Boolean
    ;; Returns: true iff the edge's from value matches the given Node.
    (lambda (e) (node=? (edge-from e) n1))
    loe)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; all-successors: SetOfNode Graph -> SetOfNode
;; GIVEN: A set of nodes
;; RETURNS: the set of all their immediate successors
;; EXAMPLES: (all-successors (list 'f1)
;                            (list (make-edge 'f1 'no-loop)
;                                  (make-edge 'f2 'f1)
;                                  (make-edge 'f3 'f1)
;                                  (make-edge 'f3 'f4)
;                                  (make-edge 'f4 'f5)
;                                  (make-edge 'f5 'f2)
;                                  (make-edge 'f5 'f3)))
;; =(list 'no-loop)
;; DESIGN STRATEGY: use HOF foldr on nodes

(define (all-successors nodes graph)
  (foldr
   ;; Node Graph -> SetOfNode
   ;; Returns: a set of node
    (lambda (node s)
      (set-union
        (get-immediate-nodes node graph)
        s))
    empty
    nodes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; reaching-itself? : SetOfNode SetOfNode Node Graph -> Boolean
;; GIVEN: two sets of nodes, a node, and a graph
;; WHERE:
;;  reached is the set of nodes reachable in graph g in fewer than n steps
;;        from some starting node 'src', for some n
;;  recent is the set of nodes reachable from src in n steps but
;;         not in n-1 steps.
;; AND tgt is not in its all successors
;; RETURNS: true iff tgt is present in its (all succesors) itself in g.
;; EXAMPLES: (reaching-itself? empty (list 'f1) 'f1 (create-graph some-loops))
;; = #false
;; DESIGN STRATEGY: Recur on recent
;; HALTING MEASURE: (length recent) or (tgt is in its all-successors set)
;; 
;; TERMINATION ARGUMENT: (recent consists of the current nodes and reached will
;; contain the nodes already traversed, so it will terminate when recent becomes
;; empty or if the target is member of all successors of recent)

(define (reaching-itself? reached recent tgt g)
   (cond
     [(member tgt (all-successors recent g)) true]
     [(empty? recent) false]
     [else
      ;; SetOfNode SetOfNode Node Graph -> Boolean
      ;; Returns: true iff tgt is present in its (all succesors) itself in g.
      (local
        ((define next-reached (append recent reached))
         (define next-recent 
           (set-diff (all-successors recent g)
                     next-reached)))
        (reaching-itself? next-reached next-recent tgt g))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sample for test

(define some-loops
  (list
     (make-def 'f1 (list 'x) (make-appexp 'no-loop (list (make-varexp 'x))))
     (make-def 'f2 (list 'u 'y) (make-appexp 'f1 (list (make-varexp 'y))))
     (make-def 'f3 (list 'x 'u)
               (make-appexp 'f1 (list (make-appexp 'f4
                                             (list (make-varexp 'u)
                                                   (make-varexp 'w)))
                                      (make-varexp 'z))))
     (make-def 'f4 (list 'x 'y)
               (make-appexp 'f5
                            (list (make-varexp 'y)
                                  (make-varexp 'u))))
     (make-def 'f5 (list 'u)
               (make-appexp 'f2
                            (list (make-appexp 'f3 empty))))
     (make-def 'no-loop (list 'x) (make-varexp 'x))))

(define loop1
  (list
     (make-def 'f1 (list 'x) (make-appexp 'f1 (list (make-varexp 'x))))))

(define loop2
  (list
     (make-def 'f1 (list 'x)
               (make-appexp 'f2
                            (list (make-appexp 'f3
                                               (list (make-appexp 'f4 empty))))))))

;; TESTS:

(begin-for-test
  (check-equal? (any-loops? some-loops)
                true
                "the output should be true for some-loops")
  (check-equal? (any-loops? loop1)
                true
                "the output should be true for loop1")
  (check-equal? (any-loops? loop2)
                false
                "the output should be false for loop2"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; make-stress-input-without-loops : PosInt -> Program
;;;; GIVEN: an integer n
;;;; RETURNS: an SGS program with no loops that defines n functions
;;;; EXAMPLES:
;;;;     (make-stress-input-without-loops 1)
;;;;  => (list
;;;;      (make-def 'f1 (list 'x 'y) (make-varexp 'x)))
;;;;
;;;;     (make-stress-input-without-loops 3)
;;;;  => (list
;;;;      (make-def 'f1 (list 'x 'y) (make-varexp 'x))
;;;;      (make-def 'f2 (list 'x 'y)
;;;;        (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'x))))
;;;;      (make-def 'f3 (list 'x 'y)
;;;;        (make-appexp
;;;;         'f2
;;;;         (make-appexp 'f2 (list (make-varexp 'x) (make-varexp 'x)))
;;;;         (make-appexp 'f2 (list (make-varexp 'x) (make-varexp 'x))))))
;
;(define (make-stress-input-without-loops n)
;  (local (
;          ;;; Returns a list of 1 through k.
;          (define (iota k)
;            (reverse (reverse-iota k)))
;          (define (reverse-iota k)
;            (if (= k 0)
;                empty
;                (cons k (reverse-iota (- k 1)))))
;
;          ;;; Given the function names in reverse order,
;          ;;; returns their bodies in reverse order.
;          (define (make-bodies names)
;            (if (empty? (rest names))
;                (list (make-varexp 'x))
;                (let* ((bodies (make-bodies (rest names)))
;                       (body (first bodies))
;                       (name (first (rest names))))
;                  (cons (make-appexp name (list body body))
;                        bodies)))))
;    (let* ((nums (iota n))
;           (syms (map (lambda (k)
;                        (string->symbol (string-append "f"
;                                                       (number->string k))))
;                      nums))
;           (bodies (reverse (make-bodies (reverse syms)))))
;      (map (lambda (sym body)
;             (make-def sym (list 'x 'y) body))
;           syms
;           bodies))))
;
;;;; stress-benchmark1 : PosInt -> Boolean
;;;; GIVEN: a positive integer n
;;;; RETURNS: false
;;;; EFFECT: reports how many milliseconds it takes to determine
;;;;     (make-stress-input-without-loops n) has no loops
;
;(define (stress-benchmark1 n)
;  (time (any-loops? (make-stress-input-without-loops n))))