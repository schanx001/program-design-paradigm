;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; q1.rkt
;; PURPOSE: add a pretty-printer which will convert the GarterSnake syntax-tree
;;          representation into a nicely indented format.

(require "extras.rkt")
(require rackunit)
(check-location "07" "q1.rkt")

(provide program-to-strings
         make-def
         make-varexp
         make-appexp)

;; DATA DEFINITION:

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

;; A ListOfDefinition (LOD) is
;; empty
;; (cons Definition LOD)

;; lod-fn: LOD -> ??
;; (define (lod-fn lod)
;;	(cond
;; 	[(empty? lod) empty]
;;	        [else (... (first lod)
;;	             	(lod-fn (rest lod)))]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct varexp (name))

;; A Varexp is a (make-varexp Variable)
;; INTERPRETATION:
;; name is the name of the variable

;; TEMPLATE:
;; varexp-fn : Varexp -> ??
;; (define (varexp-fn v)
;;  (...(varexp-name v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct appexp (fn args))

;; An Appexp is a (make-appexp Variable ListOfExp)
;; INTERPRETATION:
;; fn is the name of the variable
;; args is the ListOfExp

;; TEMPLATE:
;; appexp-fn : Appexp -> ??
;; (define (appexp-fn e)
;;  (...(appexp-fn e)
;;       (appexp-args e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en

;; TEMPLATE:
;; exp-fn : Exp -> ?
;; (define (exp-fn e)
;;  (cond
;;    [(varexp? e) ...]
;;    [(appexp? e) ...]))

;; A Variable is a Symbol.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END OF DATA DEFINITION;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; program-to-strings : Program PosInt -> ListOfString
;; GIVEN: GarterSnake program and a width
;; RETURNS: a representation of the program as a sequence of lines,
;;         following the formatting rules described below.
;; EXAMPLES: (program-to-strings sample-program 20) ->
;;   (list
;; "def a-very-long-function-name (x) :"
;; "    f1(x)"
;; "def f2 (x,"
;; "        a-very-long-variable-name,"
;; "        y) :"
;; "    f1(y)"
;; "def f3 (x,z,t,u) :"
;; "    f1(f2(z, y),"
;; "       z,"
;; "       f1(f2(z, y),"
;; "          z))")

;; STRATEGY: Use HOF foldl on program

(define (program-to-strings program width)
  (foldl
   #|
     String ListOfString -> ListOfString
     GIVEN: A string
     RETURNS: A list of strings after appending the given string
   |#
   (lambda (elt lst) (append lst (helper elt width)))
   empty
   program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; helper : Definition PosInt -> ListOfString
;; GIVEN: A definition and the width
;; RETURNS: A ListOfString representing the definition after fitting it into
;;          the given width
;; EXAMPLE: Refer test cases
;; STRATEGY: Use template for Definition on def

(define (helper def width)
  (append (get-header (def-name def) (def-args def) width)
          (reverse (get-body (def-body def) width 4 empty))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-header : Variable ListOfVariables -> ListOfString
;; GIVEN: A variable name and the list of arguments for the variable
;; RETURNS: A ListOfString representing the header after fitting it into
;;          the given width
;; EXAMPLE: Refer test cases
;; STRATEGY: Cases on length of args

(define (get-header name args width)
  (cond
    [(<= (string-length (print-header name args)) width) (list (print-header name args))]
    [(empty? args) (list (print-header name args))]
    [(empty? (rest args)) (list(print-header name args))]
    [else (append (list (string-append (get-len-def name)
                                       (symbol->string (first args))
                                       ","))
                  (get-rest-args (rest args) (string-length(get-len-def name))))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; get-rest-args : ListOfVariables PosInt -> ListOfStrings
;; GIVEN: A list of arguments and the length
;; WHERE length is for the spaces for indentation
;; RETURNS: A ListOfString for the given arguments of the definition
;; EXAMPLES: Refer test cases
;; STRATEGY: Using template for ListOfVariables on loa
;; HALTING MEASURE: length of loa


(define (get-rest-args loa length)
  (cond
    [(empty? loa) ""]
    [(equal? (rest loa) empty) (list (string-append (print-spaces "" length)
                                                    (symbol->string (first loa))
                                                    ") :"))]
    [else (append (list (string-append (print-spaces "" length)
                               (symbol->string (first loa)) ","))
                (get-rest-args (rest loa) length))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; print-spaces: String Length -> String
;; GIVEN: A string (an empty string in this case) which is to be replaced with
;;        the number of spaces as the given length
;; RETURNS: A string with the spaces as per the given length
;; EXAMPLES: (print-spaces "" 4) -> "    "
;; STRATEGY: Combine simpler functions

(define (print-spaces spc length)
  (if (>   length 0)
      (print-spaces (string-append " " spc) (- length 1))
      spc))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-len-def: Variable -> String
;; GIVEN: A Variable
;; RETURNS: The string representation of the definition with the given variable 
;;           as the name of the definition
;; EXAMPLES: (get-len-def "foo") -> "def foo ("
;; STRATEGY: Combine simpler functions

 (define (get-len-def name)
   (string-append "def "
                       (symbol->string name)
                       " ("))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; print-header: Variable ListofVariables -> ListOfString
 ;; GIVEN: A Variable name and the list of its arguments
 ;; RETURNS: A ListOfString representing the header part of the definition which
 ;;          includes the variable name and its arguments
 ;; EXAMPLES: Refer test cases
 ;; STRATEGY: Combine simpler functions
 
 (define (print-header name args)
   (string-append "def "
                  (symbol->string name)
                  " ("
                  (get-args args)
                  ") :"))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-args: ListOfVariables -> ListOfString
;; GIVEN: A list of arguments
;; RETURNS: A ListOfString representing the arguments
;; EXAMPLES: See test cases
;; STRATEGY: Using template for ListOfVariables on loa
;; HALTING MEASURE: length of loa
 
(define (get-args loa)
  (cond
    [(empty? loa) ""]
    [(equal? (rest loa) empty) (symbol->string (first loa))]
    [else (string-append (symbol->string (first loa)) "," (get-args (rest loa)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-body: Exp PosInt PosInt ListOfString -> ListOfString
;; GIVEN: An Expression, a width, a length and a ListOfString
;; WHERE length is the indendation and the ListOfString covered is the list of
;;       string which is to be displayed at the end with indentation
;; RETURNS: A list of string which represents the expression body with its
;;          indentation
;; EXAMPLES: See test cases
;; STRATEGY: Use template for Exp on exp

(define (get-body exp width len covered)
  (cond
    [(varexp? exp) (cons
                    (string-append (print-spaces "" len)
                                   (symbol->string (varexp-name exp)))
                    covered)]
    [(appexp? exp) (check-format-appexp exp width len covered)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-format-appexp: Exp PosInt PosInt ListOfString -> ListOfString
;; GIVEN: An expression, width and length and a ListOfString
;; WHERE length is for the indentation and covered is the ListOfString which
;;       has been formatted for display of the given exp
;; RETURNS: A list of string which represents the expression with its
;;          indentation
;; EXAMPLES: See test cases
;; STRATEGY: Cases on how much of the expression can fit on one line

(define (check-format-appexp exp width len covered)
  (cond
    [(<= (string-length (case1-string exp len)) width)
     (cons (case1-string exp len) covered)]
    [(<= (string-length (case2-with-first-arg exp len)) width)
     (case2-get-exps exp width len covered)]
    [else (case3-get-exps exp width len covered)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; case1-string: Exp PosInt -> String
;; GIVEN: An expression and length
;; RETURNS: A string representing the expression with indentation and its
;;         arguments in one line
;; EXAMPLES: See test cases
;; STRATEGY: Combine simpler functions


(define (case1-string exp len)
  (string-append (print-function-part exp len)
                                (get-app-elt-string (appexp-args exp) len)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; print-function-part: Exp PosInt -> String
;; GIVEN: An expression and length
;; RETURNS: A string representing the expression with its function name in one
;;          line
;; EXAMPLES: See test cases
;; STRATEGY: Combine simpler functions


(define (print-function-part exp len)
   (string-append (print-spaces "" len)
                  (symbol->string (appexp-fn exp)) "("))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-app-elt-string: Exp PosInt -> String
;; GIVEN: An expression and length
;; RETURNS: A string representing the expression with its arguments
;; EXAMPLES: See test cases
;; STRATEGY: Using template for ListOfExp on exp
;; HALTING MEASURE: length of exp


(define (get-app-elt-string exp len)
  (cond
    [(empty? exp) ")"]
    [(empty? (rest exp)) (string-append (get-exp (first exp) len) ")")]
    [else (string-append  (get-exp (first exp) len)
                          ", "
                          (get-app-elt-string (rest exp) len))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-exp: Exp PosInt -> String
;; GIVEN: An expression and length
;; RETURNS: A string representing each expression with its arguments
;; EXAMPLES: See test cases
;; STRATEGY: Cases for Exp on exp
;; HALTING MEASURE: length of args of exp

(define (get-exp exp len)
  (cond
    [(varexp? exp) (symbol->string (varexp-name exp))]
    [(appexp? exp) (string-append
                    (print-function-part exp 0)
                    (get-app-elt-string (appexp-args exp) len))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; case2-with-first-arg: AppExp PosInt -> String
;; GIVEN: An expression, and length
;; WHERE length is for the indentation 
;; RETURNS: A String after formatting the given exp 
;; EXAMPLES: See test cases
;; STRATEGY: Cases on length of args of the Appexp

(define (case2-with-first-arg exp len)
(if (empty? (appexp-args exp))
    (case1-string exp len)
    (string-append (print-function-part exp len)
                                (get-exp (first (appexp-args exp)) len) ",")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; case2-get-exps: AppExp PosInt PosInt ListOfString -> ListOfString
;; GIVEN: An expression, width, length and a list of string
;; WHERE length is for the indentation and covered is the ListOfString which
;;       has been formatted for display
;; RETURNS: A ListOfString after formatting the given exp depending on whether
;;          its a variable or an expression and appending it to
;;          the given ListOfString
;; EXAMPLES: See test cases
;; STRATEGY:Combine simpler functions

(define (case2-get-exps exp width len covered)
  (get-elt (rest (appexp-args exp))
                 width
                 (string-length (print-function-part exp len))
                 (cons  (case2-with-first-arg exp len) covered)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-elt: ListOfExp NonNegInt NonNegInt ListOfString -> ListOfString
;; GIVEN: An expression, width, length and a list of string
;; WHERE length is for the indentation and covered is the ListOfString which
;;       has been formatted for display
;; RETURNS: A ListOfString after formatting the given exp depending on whether
;;          its a variable or an expression and appending it to
;;          the given ListOfString
;; EXAMPLES: See test cases
;; STRATEGY: Using template for ListOfExp on loa
;; HALTING MEASURE: length of loa

(define (get-elt loa width len covered)
  (cond
    [(empty? loa) ")"]
    [(empty? (rest loa)) (check-varexp (first loa) width len covered)]
    [else (get-elt (rest loa)
                   width
                   len
                   (check-appexp (first loa) width len covered))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; case3-get-exps: AppExp PosInt PosInt ListOfString -> ListOfString
;; GIVEN: An expression, width, length and a list of string
;; WHERE: length is for the indentation and covered is the ListOfString which
;;       has been formatted for display
;; RETURNS: A ListOfString after adding the given expression to the already
;;          formatted list of string
;; EXAMPLES: See test cases
;; STRATEGY: Cases on length of args of the Appexp

(define (case3-get-exps exp width len covered)
  (cond
    [(empty? (appexp-args exp)) (cons (string-append (print-spaces "" (+ len 1))
                                                     "()")
                                      (cons (case3-string exp len) covered))]
    [(empty? (rest (appexp-args exp))) 
     (cons (check-case3-varexp (first (appexp-args exp)) width len covered)
           (cons (case3-string exp len) covered))]
    [else (get-elt (rest (appexp-args exp)) width (+ len 2)
                   (case3-with-first-arg
                    (check-case3-appexp (first (appexp-args exp))
                                        width
                                        len
                                        covered)
                    width
                    (+ len 1)
                    (cons (case3-string exp len) covered)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; case3-with-first-arg: Exp PosInt PosInt ListOfString -> ListOfString
;; GIVEN: An expression, width, length and a list of string
;; WHERE length is for the indentation and covered is the ListOfString which
;;       has been formatted for display
;; RETURNS: A ListOfString after formatting the given exp and appending it to
;;          the given ListOfString
;; EXAMPLES: See test cases
;; STRATEGY: Combine simpler functions

(define (case3-with-first-arg exp width len covered)
 (append
  (if (cons? exp)
      (cons (string-append (print-spaces "" len)
                        (list->string (remove-spc (first (re-lst exp len)))) ",")
            (rest (re-lst exp len)))
      (list (string-append (print-spaces "" len) "("  exp ","))) covered))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; re-lst: ListOfString PosInt -> ListOfString
;; GIVEN: A ListOfString and a length
;; WHERE len is for the indentation and lst is the ListOfString which
;;       has to be formatted for display
;; RETURNS: A ListOfString after formatting the given list for display
;; EXAMPLES: See test cases
;; STRATEGY: Combine simpler functions

(define (re-lst lst len) 
      (reverse (cons
       (string-append (print-spaces "" len) "("
                      (list->string (remove-spc (first (reverse lst)))))
       (rest (reverse lst)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove-spc: String -> String
;; GIVEN: A String
;; RETURNS: A String after removing the spaces in the string to format it for
;;          display
;; EXAMPLES: See test cases
;; STRATEGY: Use HOF foldr on character list of str

(define (remove-spc str)
  (foldr
   #|
    Character List -> ListOfCharacter
    GIVEN: A character
    RETURNS: A list of characters after removing the spaces
   |#
   (lambda (x y) (if (equal? x #\space)
                     y
                     (cons x y)))
   empty
   (string->list str)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-case3-varexp: Exp PosInt PosInt ListOfString -> ListOfString
;; GIVEN: An expression, width, length and a list of string
;; WHERE length is for the indentation and covered is the ListOfString which
;;       has been formatted for display
;; RETURNS: A ListOfString after formatting the given exp depending on whether
;;          its a variable or an expression and appending it to
;;          the given ListOfString
;; EXAMPLES: See test cases
;; STRATEGY: Cases on whether e is a varexp or appexp
;; HALTING MEASURE: length of (appexp-args e) or if e is a varexp


(define (check-case3-varexp e width len covered)
  (if (varexp? e)
     (string-append (print-spaces "" (+ len 1))
                    "("
                    (symbol->string (varexp-name e))")")
     (get-body e width (+ len 1) covered)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-case3-appexp: Exp PosInt PosInt ListOfString -> ListOfString
;; GIVEN: An expression, width, length and a list of string
;; WHERE length is for the indentation and covered is the ListOfString which
;;       has been formatted for display
;; RETURNS: A ListOfString after formatting the given exp depending on whether
;;          its a variable or an expression and appending it to
;;          the given ListOfString
;; EXAMPLES: See test cases
;; STRATEGY: Cases on whether e is a varexp or appexp
;; HALTING MEASURE: length of (appexp-args e) or if e is varexp

(define (check-case3-appexp e width len covered)
  (if (varexp? e)
     (symbol->string (varexp-name e))
     (get-body e width (+ len 1) covered)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; case3-string: AppExp PosInt -> String
;; GIVEN: An expression, and length
;; WHERE length is for the indentation 
;; RETURNS: A String after formatting the given exp for indentation
;; EXAMPLES: See test cases
;; STRATEGY: Combine simpler functions

(define (case3-string exp len)
  (string-append (print-spaces "" len) 
                  (symbol->string (appexp-fn exp))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-appexp: Exp PosInt PosInt ListOfString -> ListOfString
;; GIVEN: An expression, width, length and a list of string
;; WHERE length is for the indentation and covered is the ListOfString which
;;       has been formatted for display
;; RETURNS: A ListOfString after formatting the given exp depending on whether
;;          its a variable or an expression and appending it to
;;          the given ListOfString
;; EXAMPLES: See test cases
;; STRATEGY: Cases for Exp on e

(define (check-appexp e width len covered)
  (if (varexp? e)
      (cons (string-append (print-spaces "" len) (symbol->string (varexp-name e))",") covered)
      (reverse (handle-fn (reverse (get-body e width len covered))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; handle-fn: ListOfString -> ListOfString
;; GIVEN: A list of string
;; RETURNS: A list of string after appending "," for the last element in list
;; EXAMPLES: See test cases
;; STRATEGY: Cases on length of lst

(define (handle-fn lst)
  (if (empty? (rest lst))
      (cons (string-append (first lst) ",") (rest lst))
      (cons (first lst) (handle-fn (rest lst)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-varexp : Exp PosInt PosInt ListOfString -> ListOfString
;; GIVEN: An expression, width, length and a list of string
;; WHERE length is for the indentation and covered is the ListOfString which
;;       has been formatted for display
;; RETURNS: A ListOfString after formatting the given exp depending on whether
;;          its a variable or an expression and appending it to
;;          the given ListOfString
;; EXAMPLES: See test cases
;; STRATEGY: Cases for Exp on e


(define (check-varexp exp width len covered)
  (if (varexp? exp)
      (cons (string-append (print-spaces "" len)
                           (symbol->string (varexp-name exp))
                           ")")
            covered)
      (handle-fn-case2 (get-body exp width len covered))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; handel-fn-case2: ListOfString -> ListOfString
;; GIVEN: A list of string
;; RETURNS: The list of string after formatting the first element
;; EXAMPLES: See test cases
;; STRATEGY: Combine simpler functions

(define (handle-fn-case2 lst)
  (cons (string-append (first lst) ")") (rest lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SAMPLES FOR TESTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sample-program
  (list
   (make-def 'a-very-long-function-name
             (list 'x)
             (make-appexp 'f1 (list (make-varexp 'x))))
   (make-def 'f2 (list 'x 'a-very-long-variable-name 'y)
             (make-appexp 'f1 (list (make-varexp 'y))))
   (make-def 'f3 (list 'x 'z 't 'u)
             (make-appexp
              'f1
              (list (make-appexp
                     'f2 (list (make-varexp 'z)
                               (make-varexp 'y)))
                    (make-varexp 'z)
                    (make-appexp
                     'f1 (list (make-appexp
                                'f2 (list (make-varexp 'z)
                                          (make-varexp 'y)))
                               (make-varexp 'z))))))))
(define s-program
  (list
   (make-def 'a-very-long-function-name
             (list 'x)
             (make-appexp 'f1 (list (make-varexp 'x))))
   (make-def 'f2 (list 'x 'a-very-long-variable-name 'y)
             (make-appexp 'f1aaaaaaaaaaaaaaaaaaaaaaaaaaaa
                          (list (make-appexp 'f1aaaaaaaaaaaaaaaaaaa
                                             (list (make-varexp 'x)
                                                   (make-varexp 'x)))
                                (make-varexp 'x)
                                (make-varexp 'y)
                                (make-appexp
                                 'f2 (list (make-varexp 'z)
                                           (make-varexp 'y)))
                                (make-varexp 'y))))
   (make-def 'f3 (list 'x 'z 't 'u)
             (make-appexp
              'f1
              (list (make-appexp
                     'f2 (list (make-varexp 'z)
                               (make-varexp 'y)))
                    (make-appexp
                     'f1jjjjjjjjjjjjjjjjjjjjjjjjjjjj '())                    
                    (make-varexp 'z)
                    (make-appexp
                     'f1 (list (make-appexp
                                'f2 (list (make-varexp 'z)
                                          (make-varexp 'y)))
                               (make-varexp 'z)))
                    (make-varexp 'x))))))

(define s (list (make-def 'f2 (list 'x 'a-very-long-variable-name 'y)
                          (make-appexp 'f1aaaaaaaaaaaaaaaaaaaaaaaaaaaa
                                       (list (make-varexp 'xxxxxxxxxxx)
                                             (make-varexp 'y)
                                             (make-appexp
                                              'f2 (list (make-varexp 'z)
                                                        (make-varexp 'y)))
                                             (make-varexp 'y))))))
(define s1 (list (make-def 'f2222222222222222222222222222 '() (make-varexp 'y))))
(define s1-out (list "def f2222222222222222222222222222 () :" "    y"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES:

(define sample-program-output (list
 "def a-very-long-function-name (x) :"
 "    f1(x)"
 "def f2 (x,"
 "        a-very-long-variable-name,"
 "        y) :"
 "    f1(y)"
 "def f3 (x,z,t,u) :"
 "    f1(f2(z, y),"
 "       z,"
 "       f1(f2(z, y),"
 "          z))"))
(define s-output (list
 "def f2 (x,"
 "        a-very-long-variable-name,"
 "        y) :"
 "    f1aaaaaaaaaaaaaaaaaaaaaaaaaaaa"
 "     (xxxxxxxxxxx,"
 "      y,"
 "      f2(z, y),"
 "      y)"))


(begin-for-test
  (check-equal? (program-to-strings sample-program 20)
                sample-program-output
                "the pretty printer wasn't able to print the program as it should have")
  (check-equal? (program-to-strings s 20)
                s-output
                "the pretty printer wasn't able to print the program as it should have")
  (check-equal? (program-to-strings s1 20)
                s1-out
                "the printed output should be s1-out")
  (check-equal? (get-header 'd1 empty 20)
                (list "def d1 () :"))
  (check-equal? (get-rest-args empty 4)
                ""
                "the output should be ''")
  (check-equal? (case3-get-exps (make-appexp 'f1 empty) 20 4 empty)
                (list "     ()" "    f1")
                "the output should be '()' ")
  (check-equal? (get-app-elt-string empty 4)
                ")"
                "the output should be ')' ")
  (check-equal? (case3-get-exps (make-appexp 'f (list (make-varexp 'x)))
                  10 4 empty)
                (list "     (x)" "    f")
                "the output should be '     (x)' '    f' ")
  (check-equal? (case3-with-first-arg (list "   s") 20 4 empty)
                (list "    (s,")
                "the output should be '    (s,'")
  (check-equal? (case2-with-first-arg (make-appexp 's empty) 4)
                "    s()"
                "the output should be '    s()'")
  (check-equal? (check-case3-varexp (make-appexp 's empty) 20 4 empty)
                (list "     s()")
                "the output should be '    s()'")
  (check-equal? (check-case3-appexp (make-appexp 's empty) 20 4 empty)
                (list "     s()")
                "the output should be '     s()'")
  (check-equal? (get-elt empty 20 4 empty)
                ")"
                "the output should be ')'"))
