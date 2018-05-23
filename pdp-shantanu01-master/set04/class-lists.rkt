;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A program to demonstrate working of a list.

(require "extras.rkt")
(require rackunit)
(check-location "04" "class-lists.rkt")

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
;; DESIGN STRATEGY: Using template for ListOfSlip on los
;; HALTING MEASURE: (length los)

(define (felleisen-roster los)
  (cond
    [(empty? los) los]
    [else (if (or (check-duplicate? (first los) (rest los))
                  (color-blue? (first los)))
              (felleisen-roster (rest los))
              (cons (first los) (felleisen-roster (rest los))))]))

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

;; check-duplicate?: Slip ListOfSlip -> Boolean
;; GIVEN: a slip and a list of slip
;; RETURNS: whether the given list contains duplicate slips
;; EXAMPLES:
;; (check-duplicate? (make-slip "yellow" "A" "B")
;; (list (make-slip "yellow" "C" "D")(make-slip "yellow" "A" "B"))) = true
;; DESIGN STRATEGY: Using template for ListOfSlip on los
;; HALTING MEASURE: (length los)

(define (check-duplicate? f los)
  (cond
    [(empty? los) false]
    [else (if (and (check-if-name-equal? f (first los))
                   (color-equal? f (first los)))   
              true
              (check-duplicate? f (rest los)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; DESIGN STRATEGY: Using template for ListOfSlip on los
;; HALTING MEASURE: (length los)

(define (shivers-roster los)
  (cond
    [(empty? los) los]
    [else (if (or (check-duplicate? (first los) (rest los))
                  (color-yellow? (first los)))
              (shivers-roster (rest los))
              (cons (first los) (shivers-roster (rest los))))]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; color-yellow?: Slip -> Boolean
;; GIVEN: a Slip
;; RETURNS: true iff the color of the slip is yellow
;; DESIGN STRATEGY: Using template for Slip on s1

(define (color-yellow? s1)
  (string=? SLIP-COLOR-F (slip-color s1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; color-equal?: Slip Slip -> Boolean
;; GIVEN: two Slips
;; RETURNS: true iff the color of the slips match
;; DESIGN STRATEGY: Using template for Slip on s1 and s2

(define (color-equal? s1 s2)
   (string=? (slip-color s1) (slip-color s2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; same-color?: Slip ListOfSlip -> Boolean
;; GIVEN: a Slip and a LOS 
;; RETURNS: true iff the all the slips have the same color
;; DESIGN STRATEGY: Using template for ListOfSlip on lst
;; HALTING MEASURE: (length lst)

(define (same-color? f lst)
  (cond
    [(empty? lst) true]
    [else (if (color-equal? f (first lst))
              (same-color? f (rest lst))
              false)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; possible-roster? : ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff all the slips in the list are the same color,
;;          and no student is represented twice.
;; EXAMPLES: refer tests
;; DESIGN STRATEGY: Using template for ListOfSlip on los

(define (possible-roster? los)
  (cond
    [(empty? los) true]
    [else (if (same-color? (first los)(rest los))
              (if (check-duplicate? (first los) (rest los))
                  false
                  (possible-roster? (rest los)))
              false)]))

;; Sample and example:

(define lst5 (list (make-slip "blue" "A" "B")
      (make-slip "blue" "C" "D")
      (make-slip "blue" "A" "E")))


(define lst-c (list (make-slip "blue" "A" "B")
      (make-slip "yellow" "C" "D")
      (make-slip "blue" "A" "E")))

(define lst-d (list (make-slip "blue" "A" "B")
      (make-slip "blue" "A" "B")))

;; TESTS:
(begin-for-test
  (check-equal? (possible-roster? lst5)
                true
                "possible-roster? should have been true")
  (check-equal? (possible-roster? lst-d)
                false
                "possible-roster? should have been false")
  (check-equal? (possible-roster? lst-c)
                false
                "possible-roster? should have been false"))

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

;; DESIGN STRATEGY: Using template for ListOfSlip on los1

(define (acceptable-felleisen-answer? los1 los2)
  (cond
    [(empty? los1) true]
    [(empty? los2) (handle-empty-list2 los1)]
    [else (if (and (check-subset? (first (felleisen-roster los1)) los2)
                   (not (duplicate-list2? los2))
                   (not (contains-blue? los2)))
              
              (acceptable-felleisen-answer? (rest (felleisen-roster los1))
                                            los2)
              
              false)]))



;; contains-blue? : ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: false iff all the slips in the list are not of blue color
;; DESIGN STRATEGY: Using template for ListOfSlip on los
;; HALTING MEASURE: (length los)

(define (contains-blue? los)
  (cond
    [(empty? los) false]
    [else (if(color-blue? (first los))
             true
             (contains-blue? (rest los)))]))

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

;; duplicate-list2? : ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff all the slips in the list are the same color,
;;          and no student is represented twice.
;; DESIGN STRATEGY: Using template for ListOfSlip on los
;; HALTING MEASURE: (length los)

(define (duplicate-list2? los)
  (cond
    [(empty? los) false]
    [else (if (check-duplicate? (first los) (rest los))
              true
              (duplicate-list2? (rest los)))]))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-subset? : Slip ListOfSlip -> Boolean
;; GIVEN: a slip and a list of slips
;; RETURNS: true iff a slip is a subset of the given list of slip
;; DESIGN STRATEGY: Using template for ListOfSlip on los
;; HALTING MEASURE: (length los)

(define (check-subset? n1 los)
  (cond
    [(empty? los) false]
    [else (if (and (check-if-name-equal? n1 (first los))
                   (color-equal? n1 (first los)))
              true
              (check-subset? n1 (rest los)))]))

;; TESTS:

(begin-for-test
  (check-equal? (check-subset? (make-slip "yellow" "n1" "n2") empty)
                false
                "the list given wasn't empty"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; duplicate-list2? : ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff all the slips in the list are the same color,
;;          and no student is represented twice.
;; DESIGN STRATEGY: Using template for ListOfSlip on los

(define (handle-empty-list2 los)
  (if (same-color? (first los) (rest los))
      (if (color-blue? (first los))
                       true
                       false)
      false))

