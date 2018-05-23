;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A program to show the working of lists and the use of HOFs wherever appropriate.

(require "extras.rkt")
(require rackunit)
(check-location "05" "q1.rkt")

(provide felleisen-roster
         shivers-roster
         possible-roster?
         acceptable-felleisen-answer?
         make-slip
         slip-color
         slip-name1
         slip-name2)

;; CONSTANTS

(define SLIP-COLOR-F "yellow")
(define SLIP-COLOR-S "blue")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS:

(define-struct slip (color name1 name2))

;;A Slip is a (make-slip Color String String)
;; Interpretations:
;; color is the color of the slips of the 2 professors
;; name1 is either first or last name of the student
;; name2 is either first or last name of the student

;; template 
;; slip-fn: Slip -> ??
;; (define (slip-fn s)
;;   (...(slip-color s)
;;       (slip-name1 s)
;;       (slip-name2 s)))

;; A ListOfSlip (LOS) is one of
;; --empty
;; --(cons Slip ListOfSlip)

;; template:
;; los-fn: ListOfSlip -> ??
;; (define (los-fn los)
;;   (cond
;;     [(empty? los)...]
;;     [else (...(first los)
;;               (los-fn (rest los)))]))
;; HALTING MEASURE: (length los)

;; A Color is one of
;; -- "yellow"
;; -- "blue"

;; template for Color
;;(define (color-fn c)
;; (cond
;;   [(equal=? c "yellow")...]
;;   [(equal=? c "blue")...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; felleisen-roster : ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;; Felleisen's class, without duplication.
;; EXAMPLE: refer test cases
;; DESIGN STRATEGY: Call a more general function.

(define (felleisen-roster los)
  (uniq-roster los SLIP-COLOR-F))

;; uniq-roster: ListOfSlip Color -> ListOfSlip
;; GIVEN: a list of slip and a color of slip
;; RETURNS: a list of slip like original, but without  duplicates
;;          and of same colored slips.
;; DESIGN STRATEGY: Using HOF foldr on los.

(define (uniq-roster los color)
  (foldr
   ;; Slip ListOfSlip -> ListOfSlip
   ;; Returns: a list of slip without duplicates
   (lambda (x y) (if (and (not (check-duplicate? x y))
                          (string=? (slip-color x) color))
                     (cons x y)
                     y))
   empty
   los))

;; TESTS
;; List of student samples

(define lst (list (make-slip "yellow" "A" "B")
      (make-slip "yellow" "C" "D")
      (make-slip "yellow" "A" "B")
      (make-slip "yellow" "B" "A")
      (make-slip "blue" "C" "D")
      (make-slip "blue" "C" "D")
      (make-slip "blue" "B" "A")))

(define lst-output (list (make-slip "yellow" "C" "D")
                         (make-slip "yellow" "B" "A")))


(begin-for-test
  (check-equal? (felleisen-roster lst)
                lst-output
                "the list contains duplicates and different colored slips"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; check-if-name-equal?: Slip Slip -> Boolean
;; GIVEN: two slips
;; RETURNS: true iff the names (i.e name1 and name2) of the two given slips match.
;; EXAMPLES: (check-if-name-equal? (make-slip "yellow" "A" "B")
;;       (make-slip "yellow" "B" "A")) = true 
;; DESIGN STRATEGY: Using template for Slip on s1 and s2

(define (check-if-name-equal? s1 s2)
   (and (or (string=? (slip-name1 s1) (slip-name1 s2))
            (string=? (slip-name1 s1) (slip-name2 s2)))
        (or (string=? (slip-name2 s1) (slip-name1 s2))
            (string=? (slip-name2 s1) (slip-name2 s2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; color-equal?: Slip Slip -> Boolean
;; GIVEN: two Slips
;; RETURNS: true iff the color of the slips match
;; DESIGN STRATEGY: Using template for Slip on s1 and s2

(define (color-equal? s1 s2)
   (string=? (slip-color s1) (slip-color s2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-duplicate?: Slip ListOfSlip -> Boolean
;; GIVEN: a slip and a list of slip
;; RETURNS: whether the given list contains duplicate slips
;; EXAMPLES:
;; (check-duplicate? (make-slip "yellow" "A" "B")
;; (list (make-slip "yellow" "C" "D")(make-slip "yellow" "A" "B"))) = true
;; DESIGN STRATEGY: Using HOF ormap on los

(define (check-duplicate? f los)
   (ormap
    ;; Slip -> Boolean
    ;; Returns: true iff the slips have same name and their color is same
   (lambda (s1) (and (check-if-name-equal? f s1)
                     (color-equal? f s1)))
   los))

(begin-for-test
  (check-equal? (check-duplicate? (make-slip "yellow" "h" "e")
                                  (list (make-slip "yellow" "d" "e")
                                        (make-slip "yellow" "e" "h")))
                true
                "the list contains duplicates but returned false where it
                 should have returned true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; color-blue?: Slip -> Boolean
;; GIVEN: a Slip
;; RETURNS: true iff the color of the slip is blue
;; DESIGN STRATEGY: Using template for Slip on s1

(define (color-blue? s1)
  (string=? SLIP-COLOR-S (slip-color s1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; shivers-roster: ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;;          Shivers' class, without duplication.
;; EXAMPLE: refer test cases
;; DESIGN STRATEGY: Call a more general function

(define (shivers-roster los)
  (uniq-roster los SLIP-COLOR-S))

;; sample
;; (define lst (list (make-slip "yellow" "A" "B")
;;      (make-slip "yellow" "C" "D")
;;      (make-slip "yellow" "A" "B")
;;      (make-slip "yellow" "B" "A")
;;      (make-slip "blue" "C" "D")
;;      (make-slip "blue" "C" "D")
;;      (make-slip "blue" "B" "A")))

(define lst-output-s (list
      (make-slip "blue" "C" "D")
      (make-slip "blue" "B" "A")))

;; TESTS

(begin-for-test
  (check-equal? (shivers-roster lst)
                lst-output-s
                "the list contains duplicates and different colored slips"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; same-color?: Slip ListOfSlip -> Boolean
;; GIVEN: a Slip and a LOS 
;; RETURNS: true iff the all the slips have the same color
;; DESIGN STRATEGY: Using HOF andmap on los

(define (same-color? y los)
  (andmap
   ;; Slip -> Boolean
   ;; Returns: true iff the color of the slips are equal
   (lambda (x) (color-equal? y x))
   los))

;; EXAMPLE and TEST:

(begin-for-test
  (check-equal? (same-color? (make-slip "blue" "D" "B")
                             (list (make-slip "blue" "A" "B")
                                   (make-slip "blue" "C" "D")
                                   (make-slip "blue" "A" "E")))
                true
                "the list contains same colored slips so it should have returned true"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; possible-roster? : ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff all the slips in the list are the same color,
;;          and no student is represented twice.
;; EXAMPLES: refer tests
;; DESIGN STRATEGY: Using HOF andmap on los

(define (possible-roster? los)
  (andmap
   ;; Slip -> Boolean
   ;; Returns: true iff the slips have same color and its not a duplicate slip 
   (lambda (x) (and (same-color? x los)
                    (not(check-duplicate? x (remove x los)))))             
   los))

;; Sample and example:

(define lst5 (list (make-slip "blue" "A" "B")
      (make-slip "blue" "C" "D")
      (make-slip "blue" "A" "E")))


(define lst-c (list (make-slip "blue" "A" "B")
      (make-slip "yellow" "C" "D")
      (make-slip "blue" "A" "E")))

(define lst-d (list (make-slip "blue" "A" "B")
      (make-slip "blue" "A" "B")))

(define l  '())

;; TESTS:
(begin-for-test
  (check-equal? (possible-roster? l)
                true
                "possible-roster? should have been true for list l")
  (check-equal? (possible-roster? lst-d)
                false
                "possible-roster? should have been false for lst-d")
  (check-equal? (possible-roster? lst-c)
                false
                "possible-roster? should have been false for lst-c"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; acceptable-felleisen-answer? : ListOfSlip ListOfSlip -> Boolean
;; GIVEN: two lists of slips, lst1 and lst2
;; RETURNS: true iff every student on a yellow slip in lst1 appears once
;; and only once in lst2.
;; EXAMPLES:
;; Let lst1 = (list
;;              (make-slip "yellow" "Wang" "Xi")
;;              (make-slip "blue" "Jones" "Tom")
;;              (make-slip "yellow" "Xi" "Wang")
;;              (make-slip "yellow" "Shriram" "K."))

;; This list contains two of Professor Felleisen's students: Wang Xi (or
;; maybe Xi Wang), and Shriram K.  Therefore the following are acceptable
;; answers and should return true when given to acceptable-felleisen-answer? 
;; (list
;; (make-slip "yellow" "Wang" "Xi")
;; (make-slip "yellow" "Shriram" "K."))

;; (list
;; (make-slip "yellow" "Shriram" "K.")
;; (make-slip "yellow" "Wang" "Xi"))

;; (list
;; (make-slip "yellow" "Shriram" "K.")
;; (make-slip "yellow" "Xi" "Wang"))

;; (list
;; (make-slip "yellow" "K." "Shriram")
;; (make-slip "yellow" "Xi" "Wang"))

;; DESIGN STRATEGY: Cases on lists being empty

(define (acceptable-felleisen-answer? los1 los2)
  (cond
    [(empty? los1) true]
    [(empty? los2) (handle-empty-list2 los1)]
    [else (handle-both-lists los1 los2)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; handle-both-lists: ListOfSlip ListOfSlip -> Boolean
;; GIVEN: two list of slips
;; RETURNS: true iff first list is a subset of the second list and
;;          second list doesn't contain duplicates and color blue slip.
;; DESIGN STRATEGY: Using HOF andmap on (felleisen-roster los1)

(define (handle-both-lists los1 los2)
  (andmap
   ;; Slip -> Boolean
   ;; Returns: true iff the slip is a subset of a given list and the given
   ;; list does not have duplicates and doesnot contain blue slips
   (lambda (x) (and (check-subset? x los2)
                    (not-duplicate-list2? los2)
                    (not(contains-blue? los2))))
   (felleisen-roster los1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; contains-blue? : ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: false iff all the slips in the list are not of blue color
;; DESIGN STRATEGY: Using HOF ormap on los

(define (contains-blue? los)
  (ormap
   ;; Slip -> Boolean
   ;; Returns: true iff the slip is of blue color
  (lambda (x) (color-blue? x))
  los))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Samplles:


(define lst-a1 (list (make-slip "yellow" "Wang" "Xi")
      (make-slip "blue" "Jones" "Tom")
      (make-slip "yellow" "Xi" "Wang")
      (make-slip "yellow" "Shriram" "K.")))

(define lst-a3 (list (make-slip "blue" "Wang" "Xi")
      (make-slip "blue" "Jones" "Tom")
      (make-slip "blue" "Xi" "Wang")))

(define lst-a7 (list (make-slip "yellow" "Wang" "Xi")
      (make-slip "yellow" "Jones" "Tom")
      (make-slip "yellow" "Xi" "Wang")))

(define lst-a2 (list
 (make-slip "yellow" "Wang" "Xi")
 (make-slip "yellow" "Shriram" "K.")
 (make-slip "yellow" "Kobe" "Bryant")
 (make-slip "yellow" "Bryant" "Kobe")))

(define lst-a4 (list
 (make-slip "yellow" "Wang" "Xi")
 (make-slip "yellow" "Shriram" "K.")))

(define lst-a8 (list
 (make-slip "blue" "Wang" "Xi")
 (make-slip "yellow" "Shriram" "K.")))

(define ls1 '())

(define ls2 '())

;; TESTS:

(begin-for-test
  (check-equal? (acceptable-felleisen-answer? lst-a1 lst-a4)
                true
                "the result should have been true")
  (check-equal? (acceptable-felleisen-answer? lst-a1 ls2)
                false
                "the result should be false when list2 is empty")
  
  (check-equal? (acceptable-felleisen-answer? ls1 ls2)
                true
                "the result should be true if both lists empty")
  
  (check-equal? (acceptable-felleisen-answer? lst-a3 ls2)
                true
                "the result should be true when list2 is empty but list1 contains all blue slips")
  
  (check-equal? (acceptable-felleisen-answer? lst-a1 lst-a2)
                false
                "the result should be false when list2 has duplicates")
  
  (check-equal? (acceptable-felleisen-answer? lst-a7 ls2)
                false
                "the result should be false when list2 is empty but list1 contains all yellow slips")

  (check-equal? (acceptable-felleisen-answer? ls1 lst-a4)
                true
                "the result should be true when list1 is empty")
  (check-equal? (acceptable-felleisen-answer? lst-a1 lst-a8)
                false
                "the result should have been false as list2 has one or more blue slips")
  (check-equal? (contains-blue? lst-a8)
                true
                "if the list has blue slips,it should return true but didn't"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; not-duplicate-list2? : ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff all the slips in the list are the same color,
;;          and no student is represented twice.
;; DESIGN STRATEGY: Using HOF andmap on los

(define (not-duplicate-list2? los)
  (andmap
   ;; Slip -> Boolean
   ;; Returns: true iff the slip is not a duplicate in the given list
   (lambda (x) (not(check-duplicate? x (remove x los))))             
   los))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-subset? : Slip ListOfSlip -> Boolean
;; GIVEN: a slip and a list of slips
;; RETURNS: true iff a slip is a subset of the given list of slip
;; DESIGN STRATEGY: Using HOF ormap on los

(define (check-subset? n1 los)
  (ormap
   ;; Slip -> Boolean
   ;; Returns: true iff the slip has a same name and color as the given slip
   (lambda (x) (and (check-if-name-equal? x n1)
                    (color-equal? x n1)))
   los))

;; TESTS:

(begin-for-test
  (check-equal? (check-subset? (make-slip "yellow" "n1" "n2") empty)
                false
                "the list given wasn't empty"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; handle-empty-list2 : ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff all the slips in the list are the same color,
;;          and not the blue color.
;; WHERE: los is a non empty list
;; DESIGN STRATEGY: Combine using simpler functions.

(define (handle-empty-list2 los)
  (and (same-color? (first los) los) (color-blue? (first los))))
                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
