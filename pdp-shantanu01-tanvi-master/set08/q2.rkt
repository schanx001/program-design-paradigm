;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; q2.rkt
;; PURPOSE: find whether a given list of clauses is null derivable or not

(require "extras.rkt")
(require rackunit)
(require "sets.rkt")
(check-location "08" "q2.rkt")

(provide
 is-null-derivable?
 make-pos
 make-neg
 make-clause)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Literal is one of
;; -- (make-pos Variable)  Interp: a literal containing the variable
;; -- (make-neg Variable)  Interp: a literal containing the negation of
;;                                the variable
;;
;; A Clause is a SetOfLiteral

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct pos (var))

;; A Pos is a (make-pos Variable)
;; INTERPRETATION:
;; var is the variable symbol which is positive

;; TEMPLATE:
;; pos-fn : Pos -> ?
;; (define (pos-fn v)
;;  (...(pos-var v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct neg (var))

;; A Neg is a (make-neg Variable)
;; INTERPRETATION:
;; var is the variable symbol which is negative

;; TEMPLATE:
;; neg-fn : Neg -> ?
;; (define (neg-fn v)
;;  (...(neg-var v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct claus (sol))

;; A Clause is a (make-claus SetOfLiteral)
;; INTERPRETATION:
;; sol is the set of literals without any duplicates

;; TEMPLATE:
;; sol-fn : Sol -> ?
;; (define (sol-fn s)
;;  (...(claus-sol s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ListOfLiteral (LOL) is
;; empty
;; (cons Literal LOL)

;; lol-fn: LOL -> ??
;; (define (lol-fn lol)
;;	(cond
;; 	[(empty? lol) empty]
;;	        [else (... (first lol)
;;	             	(lol-fn (rest lol)))]))

;; A SetOfLiteral(SOL) is a ListOfLiteral without any duplicates

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ListOfClause (LOC) is
;; empty
;; (cons Clause LOC)

;; loc-fn: LOC -> ??
;; (define (loc-fn loc)
;;	(cond
;; 	[(empty? loc) empty]
;;	        [else (... (first loc)
;;	             	(loc-fn (rest loc)))]))

;; A SetOfClause(SOC) is a ListOfClause without any duplicates


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-clause: ListOfLiteral -> Clause
;; GIVEN: a list of literals, possibly with duplications
;; RETURNS: a clause containing exactly those literals
;; EXAMPLE: (make-clause (list (make-pos 'a)(make-neg 'b)(make-pos 'a))) ->
;;           (make-claus (list (make-neg 'b) (make-pos 'a)))
;; STRATEGY: Combine simpler functions

(define (make-clause lol)
  (make-claus (get-set-sol lol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; END OF DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-set-sol: ListOfLiteral -> SetOfLiteral
;; GIVEN: a list of literals, possibly with duplications
;; RETURNS: a clause containing exactly those literals
;; EXAMPLE: (get-set-sol (list (make-pos 'a)(make-neg 'b)(make-pos 'a))) ->
;;           (list (make-neg 'b) (make-pos 'a))
;; STRATEGY: Use HOF foldr on lol

(define (get-set-sol lol)
  (foldr
   #|
     Literal ListOfLiteral -> SetOfLiteral
     GIVEN: A literal and a list of literal
     RETURNS: A set of literals after removing all the duplicates from the list
    |#
   (lambda (elt lol) (if (not (my-member? elt lol))
                         (cons elt lol)
                         lol))
   empty
   lol))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; is-null-derivable? : ListofClause -> Boolean
;; GIVEN: A list of clauses which are to be checked if derivable
;; RETURNS: true iff any of the subset of the list of clauses becomes empty
;;          after resolution
;; EXAMPLES:
;; (is-null-derivable (list (make-clause (list (make-pos 'a)
;;                                      (make-pos 'b)))
;;                    (make-clause (list (make-neg 'a)
;;                                      (make-neg 'b))))) -> false
;; STRATEGY: Cases on loc being empty

(define (is-null-derivable? loc)
  (if (empty? loc)
      false
      (process-loc? loc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; process-loc?: ListOfClause -> Boolean
;; GIVEN: A list of clauses
;; WHERE loc is the original list of clauses to which the resolved clauses are
;; appended
;; RETURNS: true if the ListOfClause can return an empty clause after resolving
;;          else false
;; EXAMPLES:
;; (process-loc? (list (make-clause (list (make-pos 'a)
;;                                      (make-pos 'b)))
;;                    (make-clause (list (make-neg 'a)
;;                                      (make-neg 'b))))) -> false
;; STRATEGY: Cases on whether loc is empty, or there is only one clause in loc,
;;           or if any clause is empty.
;; HALTING MEASURE: length of loc or an empty clause in loc
;; TERMINATING ARGUMENT: after every recursive call,loc will increase in length
;;                       if new resolved clauses can be added to loc, untill
;;                       there are no more clauses that can be resolved, and
;;                       then decrease in length till only one clause remains or  
;;                       an empty clause is encountered, in which case we will
;;                       break out of the loop


(define (process-loc? loc)
  (cond
    [(empty? loc) false]
    [(my-member? (make-clause '()) loc) true]
    [(empty? (rest loc)) false]
    [else
     (process-loc? (append
                   (rest loc)
                   (apply-resolution (first loc) (rest loc) empty)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; apply-resolution: Clause ListOfClause ListOfClause -> ListOfClause
;; GIVEN: A clause, a list of clause which is the original list to be resolved
;;        with the given clause, and a list of clause which contains the 
;;        resolution combinations and is initially empty
;; WHERE flist is the list of clauses with all the combinations  after applying
;;       resolution on the clause
;; EXAMPLES: See test cases
;; STRATEGY: Using HOF foldr on loc. If two clauses can be resolved with only
;;           one set of complimentary literals, resolve them, else take the
;;           next clause in the list to combine with the given clause
;;           
;; HALTING MEASURE: length of loc


(define (apply-resolution c loc flist)
  (foldr
   (lambda (cl flist) (if (should-be-resolved c cl)
                          (append (get-resolved-clause (remove-complimentary
                                                        (get-set-union c cl))) flist)
                          flist))
   flist
   loc))
                        
;  (cond
;    [(empty? loc) flist]
;    [else
;     (if (should-be-resolved c (first loc))
;         (apply-resolution c (rest loc) (append
;                                         (get-resolved-clause
;                                          (remove-complimentary
;                                           (get-set-union c (first loc)))) 
;                                         flist))
;         (apply-resolution c (rest loc) flist))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; should-be-resolved: Clause Clause -> Boolean
;; GIVEN: Two clauses
;; RETURNS: true iff the two clauses contain exactly one pair of complimentary
;;          literals, else false
;; EXAMPLES:
;; (should-be-resolved (make-clause (list (make-pos 'a) (make-pos 'b)))
;;                     (make-clause (list (make-neg 'a) (make-pos 'c)))) -> true
;; (should-be-resolved (make-clause (list (make-pos 'a) (make-pos 'b)))
;;                     (make-clause (list (make-neg 'a) (make-neg 'b))))-> false
;; STRATEGY: Combining simpler functions


(define (should-be-resolved c c1)
  (= (length (remove-complimentary (get-set-union c c1)))
     (- (length (get-set-union c c1)) 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-set-union: Clause Clause -> SetOfLiteral
;; GIVEN: Two clauses which are to be combined
;; RETURNS: A set of literals of the two clauses after combining all the
;;          literals in both the clauses
;; EXAMPLES: 
;; (get-set-union (make-clause (list (make-pos 'a) (make-neg 'b)))
;;                (make-clause (list (make-neg 'a) (make-neg 'c)))) ->
;; (list (make-pos 'a) (make-neg 'b) (make-neg 'a) (make-neg 'c))
;; STRATEGY: Combining simpler functions


(define (get-set-union c1 c2)
  (set-union (claus-sol c1) (claus-sol c2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove-complimentary: SetOfLiterals -> SetOfLiterals
;; GIVEN: A set of literals which is the union of the literals of the two
;;       clauses which are to be resolved
;; RETURNS: A set of literals after removing all the complimentary pairs from
;;          the given union set
;; EXAMPLES:
;; (remove-complimentary (list (make-pos 'a) (make-pos 'b) (make-pos 'c)
;;                            (make-neg 'a) (make-neg 'b) (make-pos 'd))) ->
;; (list (make-pos 'c) (make-pos 'd))
;; STRATEGY: Use HOF foldr on unionc1c2

(define (remove-complimentary unionc1c2)
  (foldr
   #|
     Literal SetOfLiteral -> SetOfLiteral
     GIVEN: A literal and a SetOfLiteral
     RETURNS: A setofLiteral after removing all the complimentary pairs from it
   |#
   (lambda (x y) (if (and (my-member? (get-compliment x) unionc1c2)
                          (my-member? x unionc1c2))
                     y
                     (cons x y)))
   empty
   unionc1c2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-resolved-clause: SetOfLiteral -> ListOfClause
;; GIVEN: A set of literals from which complimentary pairs have been removed
;; RETURNS: A list of the clause after making a clause from the given set of
;;          literals
;; EXAMPLES: (get-resolved-clause (list (make-pos 'a) (make-neg 'b))) ->
;;            (list (make-claus (list (make-pos 'a) (make-neg 'b))))
;; STRATEGY: Combine simpler functions

(define (get-resolved-clause sol)
  (list (make-clause sol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-compliment: Literal -> Literal
;; GIVEN: A literal whose compliment is needed
;; RETURNS: The compliment of the given literal
;; EXAMPLES: (get-compliment (make-pos 'a)) -> (make-neg 'a)
;; STRATEGY: Cases on whether literal is pos or neg


(define (get-compliment literal)
  (if (pos? literal)
      (make-neg (pos-var literal))
      (make-pos (neg-var literal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-pos : Symbol -> Literal
;; make-neg : Symbol -> Literal
;; GIVEN: A Variable v
;; RETURNS: The literal v or ¬v


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define loc (list (make-clause (list (make-pos 'a)
                                     (make-neg 'b)
                                     (make-pos 'c)))
                  (make-clause (list (make-pos 'd)
                                     (make-pos 'b)))
                  (make-clause (list (make-neg 'a)
                                     (make-pos 'c)))
                  (make-clause (list (make-pos 'b)))
                  (make-clause (list (make-neg 'c)))))

(define loc1 (list (make-clause (list (make-pos 'a)
                                      (make-pos 'b)))
                   (make-clause (list (make-neg 'a)
                                      (make-neg 'b)))))


(define loc2 (list (make-clause (list (make-neg 'c)))
                   (make-clause (list (make-neg 'b)))
                   (make-clause (list (make-pos 'a)
                                      (make-pos 'c)
                                      (make-pos 'b)))
                   (make-clause (list (make-neg 'a)))
                   (make-clause (list (make-pos 'c)
                                      (make-pos 'b)))))
(define loc3 (list (make-clause (list (make-pos 'a)
                                      (make-pos 'b)
                                      (make-pos 'c)))
                   (make-clause (list (make-neg 'a)
                                      (make-neg 'b)
                                      (make-pos 'd)))
                   (make-clause (list (make-neg 'd)))))
(define loc11
  (list
   (make-clause (list (make-pos 'a) (make-pos 'b) (make-pos 'c)))
   (make-clause (list (make-neg 'a) (make-neg 'b) (make-pos 'd)))
   (make-clause (list (make-neg 'd)))))

(define loc12
  (list
   (make-clause (list (make-neg 'b) (make-pos 'c)))
   (make-clause (list (make-pos 'a) (make-pos 'c)))
   (make-clause (list (make-neg 'a) (make-pos 'b)))
   (make-clause (list (make-neg 'a) (make-neg 'b)))
   (make-clause (list (make-pos 'a) (make-neg 'b)))
   (make-clause (list (make-pos 'b) (make-neg 'c)))))

(define loc13
  (list
   (make-clause  (list (make-pos 'a) (make-pos 'b) (make-pos 'c)))
   (make-clause (list (make-neg 'a) (make-neg 'b)))
   (make-clause (list (make-pos 'b) (make-neg 'c)))
   (make-clause (list (make-neg 'b) (make-neg 'c)))
   (make-clause (list (make-neg 'a) (make-pos 'c)))
   (make-clause (list (make-neg 'b) (make-pos 'c)))))

(define loc14
  (list (make-claus (list (make-neg 'a)
                          (make-pos 'b)
                          (make-neg 'c)))
        (make-claus (list (make-neg 'd)
                          (make-neg 'c)))
        (make-claus (list (make-neg 'd)
                          (make-pos 'c)))
        (make-claus (list (make-pos 'a)
                          (make-neg 'b)
                          (make-pos 'c)
                          (make-pos 'd)))))

(define loc16
  (list
   (make-clause (list (make-pos 'a) (make-neg 'b)))
   (make-clause  (list (make-neg 'a) (make-pos 'b)))
   (make-clause (list (make-neg 'b)))
   (make-clause (list (make-pos 'b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-test
  (check-equal? (is-null-derivable? loc)
                true
                "Function should return true as it is null derivable")
  (check-equal? (is-null-derivable? loc1)
                false
                "Function should return false as it is not null derivable")
  (check-equal? (is-null-derivable? loc2)
                true
                "Function should return true as it is null derivable")
  (check-equal? (is-null-derivable? loc3)
                false
                "Function should return false as it is not null derivable")
  (check-equal? (is-null-derivable? loc11)
                false
                "Function should return false as it is null derivable")
  (check-equal? (is-null-derivable? loc13)
                true
                "Function should return true as it is null derivable")
  (check-equal? (is-null-derivable? loc14)
                false
                "Function should return false as it is not null derivable")
  (check-equal? (is-null-derivable? empty)
                false
                "Function should return false as it is not null derivable")
  (check-equal? (process-loc? empty)
                false
                "Function should return false as it is not null derivable")
  (check-equal? (get-set-sol (list (make-pos 'a) (make-neg 'b) (make-pos 'a)))
                (list (make-neg 'b) (make-pos 'a))
                "Function should return list with a,b"))

;;;; Note: The original version of this benchmark defined a function
;;;; that didn't satisfy its contract.  That function was not needed
;;;; by the benchmark and has been removed.  I have also added a call
;;;; to make-clause.
;
;;; make-stress-input-sat : NonNegInt -> ListOfClause
;;; GIVEN: an integer n
;;; RETURNS: a satisfiable set of clauses of length n
;;; EXAMPLES:
;;;     (make-stress-input-sat 0) => empty
;;;     (make-stress-input-sat 3)
;;;  => (list (make-clause (list (make-pos 'p1)
;;;                              (make-neg 'p2)
;;;                              (make-neg 'p3)))
;;;           (make-clause (list (make-neg 'p1)
;;;                              (make-pos 'p2)
;;;                              (make-neg 'p3)))
;;;           (make-clause (list (make-neg 'p1)
;;;                              (make-neg 'p2)
;;;                              (make-pos 'p3))))
;
;(define (make-stress-input-sat n)
;  (local ((define (reverse-iota k)
;            (if (= k 0)
;                empty
;                (cons k (reverse-iota (- k 1)))))
;          (define (iota k)
;            (reverse (reverse-iota k))))
;    (let* ((nums (iota n))
;           (syms (map (lambda (k)
;                        (string->symbol (string-append "p"
;                                                       (number->string k))))
;                      nums)))
;      (map (lambda (k)
;             (make-clause   ; see note above
;              (map (lambda (i)
;                     ((if (= i k) make-pos make-neg)
;                      (list-ref syms (- i 1))))
;                   nums)))
;           nums))))
;
;
;
;;; make-stress-input-unsat : PosInt -> ListOfClause
;;; GIVEN: an integer n
;;; RETURNS: an unsatisfiable set of clauses of length 2n
;
;(define (make-stress-input-unsat n)
;  (local ((define (reverse-iota k)
;            (if (= k 0)
;                empty
;                (cons k (reverse-iota (- k 1)))))
;          (define (iota k)
;            (reverse (reverse-iota k))))
;    (let* ((nums (iota n))
;           (syms (map (lambda (k)
;                        (string->symbol (string-append "p"
;                                                       (number->string k))))
;                      nums)))
;      (cons (make-clause (list (make-neg (first syms))))
;            (append
;             (map (lambda (sym)
;                    (make-clause (list (make-pos sym))))
;                  (rest syms))
;             (map (lambda (k)
;                    (make-clause
;                     (map (lambda (i)
;                            ((if (= i k) make-pos make-neg)
;                             (list-ref syms (- i 1))))
;                          nums)))
;                  nums))))))
;
;; stress-benchmark2 : NonNegInt -> Boolean
;;; GIVEN: a non-negative integer n
;;; RETURNS: false
;;; EFFECT: reports how many milliseconds it takes to determine
;;;     (make-stress-input-sat n) is satisfiable
;
;(define (stress-benchmark1 n)
;  (time (is-null-derivable? (make-stress-input-unsat n))))
;
;(define (stress-benchmark2 n)
;  (time (is-null-derivable? (make-stress-input-sat n))))