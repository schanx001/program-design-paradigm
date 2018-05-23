;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A program to show working of Higher Order Functions while processing lists.

(require "extras.rkt")
(require rackunit)
(check-location "05" "q2.rkt")
(provide make-enrollment
         enrollment-student
         enrollment-class
         make-roster
         roster-classname
         roster-students
         behavior-correct?
         enrollments-to-rosters
         enrollments-to-rosters-bad-1
         enrollments-to-rosters-bad-2
         enrollments-to-rosters-bad-3)

;; DATA DEFINITIONS:

(define-struct enrollment (student class))

;; An EnrollmentAssertion is a (make-enrollment Student Class).
;; (make-enrollment s c) represents the assertion that student s is
;; enrolled in class c.
;; Interpretations:
;; student is the identity of the student in any type.
;; class is the name/description of the class in any type.

;; template:
;; enrollment-fn: EnrollmentAssertion -> ??
;; (define (enrollment-fn e)
;;   (...(enrollment-student)
;;       (enrollment-class)))

(define-struct roster (classname students))

;; A ClassRosterAssertion is a (make-roster Class SetOfStudent).
;; (make-roster c ss) represents the assertion that the students in class
;; c are exactly the students in set ss.
;;

;; template:
;; roster-fn: ClassRosterAssertion -> ??
;; (define (roster-fn r)
;;  (...(roster-classname)
;;      (roster-students)))

;; A ProposedSolution is a function with contract
;; SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; that is, it is a function that takes a SetOfEnrollmentAssertion and
;; produces a SetOfClassRosterAssertion

;; A SetOfX is a list of X's without duplication.  Two SetOfX's are
;; considered equal if they have the same members.

;; Example: (list (list 1 2) (list 2 1)) is NOT a SetOfSetOfNumber,
;; because (list 1 2) and (list 2 1) represent the same set of numbers.

;; A ListOfStudent (LOS) is one of:
;; --empty
;; --(cons Student ListOfStudent)

;; ;; template:
;; ls-fn: ListOfStudent -> ??
;; (define (ls-fn los)
;;   (cond
;;     [(empty? los)...]
;;     [else (...(first los)
;;               (los-fn (rest los)))]))
;; HALTING MEASURE: (length los)

;; A SetOfStudent is a ListOfStudent with no duplicates.

;; A ListOfEnrollmentAssertion (LEA) is one of:
;; --empty
;; --(cons EnrollmentAssertion LEA)

;; template:
;; lea-fn:  ListOfEnrollmentAssertion -> ??
;; (define (lea-fn lea)
;;   (cond
;;     [(empty? lea)...]
;;     [else (...(first lea)
;;               (lea-fn (rest lea)))]))
;; HALTING MEASURE: (length lea)

;; A SetOfEnrollmentAssertion is a ListOfEnrollmentAssertion with no duplicates.

;; A ListOfClassRosterAssertion (LCRA) is one of:
;; --empty
;; --(cons ClassRosterAssertion LCRA)

;; template:
;; lcra-fn:  ListOfClassRosterAssertion -> ??
;; (define (lcra-fn lcra)
;;   (cond
;;     [(empty? lcra)...]
;;     [else (...(first lcra)
;;               (los-fn (rest lcra)))]))
;; HALTING MEASURE: (length lcra)

;; A SetOfClassRosterAssertion is a ListOfClassRosterAssertion with no duplicates.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; End Of Data Definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enrollments-to-rosters: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; GIVEN: a set of enrollments
;; RETURNS: a correct set of class rosters for the given enrollments
;; EXAMPLE:
;; If soln1 is a ProposedSolution, we might have
;;  (soln1
;;    (list (make-enrollment "John" "PDP")
;;          (make-enrollment "Kathryn" "Networks")
;;          (make-enrollment "Feng" "PDP")
;;          (make-enrollment "Amy" "PDP")
;;          (make-enrollment "Amy" "Networks")))
;; =>
;; (list
;;   (make-roster "PDP" (list "John" "Feng" "Amy"))
;;   (make-roster "Networks" (list "Kathryn" "Amy")))
;;
;; This is an example of correct behavior by a ProposedSolution.
;; DESIGN STRATEGY: Using HOF foldr on (get-list-of-class-roster se)

(define (enrollments-to-rosters se)
    (foldr
     ;; ClassRosterAssertion SetOfClassRosterAssertion -> SetOfClassRosterAssertion
     ;; Returns: a set of class roster assertion with no duplicates
     (lambda (y z) (if (not (my-member? y z))
                       (cons y z)
                       z))
     empty
     (get-list-of-class-roster se)))


(begin-for-test
  (check-equal? (enrollments-to-rosters lst2)
                (list
                 (make-roster "IR" (list "Sansa"))
                 (make-roster "PDP" (list "Jon" "Sansa"))
                 (make-roster "Networks" (list "Arya")))
                "the list obtained is not a correct list from enrollments-to-rosters"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-list-of-class-roster: SetOfEnrollmentAssertion -> ListOfClassRosterAssertion
;; GIVEN: a set of enrollment assertion
;; RETURNS: a set of class roster assertion with classes and corresponding
;; students in the class consisting of duplicate rosters.
;; DESIGN STRATEGY: Combine simpler function.

(define (get-list-of-class-roster se)
  (list-of-class-roster-fn get-list-of-students se))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; StudentSet (ss-fn) is a function with contract
;; EnrollmentAssertion SetOfEnrollmentAssertion -> SetOfStudent

;; list-of-class-roster-fn: StudentSet SetOfEnrollmentAssertion -> ListOfClassRosterAssertion
;; GIVEN: a studentset function and a set of enrollment assertion
;; RETURNS: a set of class roster assertion with classes and corresponding
;; students in the class consisting of duplicate rosters.
;; DESIGN STRATEGY: Using HOF map on se

(define (list-of-class-roster-fn ss-fn se)
  (map
   ;; EnrollmentAssertion -> ClassRosterAssertion
   ;; Returns: a class roster with class and student list
   (lambda (x) (make-roster (enrollment-class x) (ss-fn x se))) 
   se))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-list-of-students: EnrollmentAssertion SetOfEnrollmentAssertion -> SetOfStudent 
;; GIVEN: an enrollment assertion and a set of enrollment assertion
;; RETURNS: a set of students corresponding to the class they attend 
;; DESIGN STRATEGY: Using HOF foldr on se

(define (get-list-of-students e1 se)
  (foldr
   ;; EnrollmentAssertion SetOfStudent -> SetOfStudent
   ;; Returns: a set of students with no duplicates corresponding to their classes
   (lambda (x y) (if (and (equal? (enrollment-class e1) (enrollment-class x))
                          (not(my-member? (enrollment-student x) y)))
                   (cons (enrollment-student x) y)
                   y))
   empty
   se))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; my-member? : X SetOfX -> Boolean
;; GIVEN: an X and a set of X's
;; RETURNS: true iff the X is an element of the set
;; STRATEGY: Use HOF ormap on set1

(define (my-member? x set1)
  (ormap
   ;; X -> Boolean
   ;; Returns: true iff X is a member of given set
   (lambda (elt) (equal? x elt))
   set1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; subset? : SetOfX SetOfX -> Boolean
;; GIVEN: two set of Xs
;; RETURNS: true iff a set is a subset of the other set
;; STRATEGY: Use HOF andmap on set1

(define (subset? set1 set2)
  (andmap
   ;; X -> Boolean
   ;; Returns: true iff X is present in the given set
   (lambda (elt) (my-member? elt set2))
   set1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set-equal? : SetOfX SetOfX -> Boolean
;; GIVEN: an X and a set of X's
;; RETURNS: true iff both the sets are equal
;; EXAMPLES: 
;; (list 1 2) and (list 2 1) represent the same set of numbers.
;; STRATEGY: Call simpler functions

(define (set-equal? set1 set2)
  (and
   (subset? set1 set2)
   (subset? set2 set1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; enrollments-to-rosters-bad-1: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; enrollments-to-rosters-bad-2: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; enrollments-to-rosters-bad-3: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; GIVEN: a set of enrollment assertions
;; RETURN: an incorrect set of class rosters for the given enrollments.
;; The three functions should return DIFFERENT incorrect sets of class
;; rosters.
;; EXAMPLES: Refer test cases

;--------------------------------------------------------------------------------------------
;; This function produces class roster set with exchanging the students between the classes.
;; DESIGN STRATEGY: Using HOF foldr on (get-list-of-class-roster-bad1 se)

(define (enrollments-to-rosters-bad-1 se)
  (foldr
   ;; ClassRosterAssertion SetOfClassRosterAssertion -> SetOfClassRosterAssertion
   ;; Returns: a set of class roster assertion without duplicates
   (lambda (y z) (if (not (my-member? y z))
                     (cons y z)
                     z))
   empty
   (get-list-of-class-roster-bad1 se)))

(begin-for-test
  (check-equal? (enrollments-to-rosters-bad-1 lst2)
                (list
                 (make-roster "IR" (list "Jon" "Sansa" "Arya"))
                 (make-roster "PDP" (list "Sansa" "Arya"))
                 (make-roster "Networks" (list "Jon" "Sansa")))
                "the list obtained from enrollments-to-rosters-bad-1
                is not a bad list of roster"))

;; get-list-of-class-roster-bad1: SetOfEnrollmentAssertion -> ListOfClassRosterAssertion
;; GIVEN: a set of enrollment assertion
;; RETURNS: a set of class roster assertion with classes and corresponding
;; students in the class consisting of duplicate rosters.
;; DESIGN STRATEGY: Combine simpler function.

(define (get-list-of-class-roster-bad1 se)
  (list-of-class-roster-fn get-list-of-students-bad1 se))

;; get-list-of-students-bad1: EnrollmentAssertion SetOfEnrollmentAssertion -> SetOfStudent 
;; GIVEN: an enrollment assertion and a set of enrollment assertion
;; RETURNS: a set of students not corresponding to the class they attend 
;; DESIGN STRATEGY: Using HOF foldr on se

(define (get-list-of-students-bad1 e1 se)
  (foldr
   ;; EnrollmentAssertion SetOfStudent -> SetOfStudent
   ;; Returns: a set of student without duplicates but with exchanged classes
   (lambda (x y) (if (and (not (equal? (enrollment-class e1) (enrollment-class x)))
                          (not (my-member? (enrollment-student x) y)))
                   (cons (enrollment-student x) y)
                   y))
   empty
   se))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;-------------------------------------------------------------------------------------------
;; This function produces class roster set with only the first student present
;; within the corresponding class
;; DESIGN STRATEGY: Using HOF foldr on (get-list-of-class-roster-bad2 se)

(define (enrollments-to-rosters-bad-2 se)
  (foldr
   ;; ClassRosterAssertion SetOfClassRosterAssertion -> SetOfClassRosterAssertion
   ;; Returns: a set of class roster assertion without duplicates 
   (lambda (y z) (if (not (my-member? y z))
                     (cons y z)
                     z))
   empty
   (get-list-of-class-roster-bad2 se)))

(begin-for-test
  (check-equal? (enrollments-to-rosters-bad-2 lst2)
                (list
                 (make-roster "IR" (list "Sansa"))
                 (make-roster "PDP" (list "Jon"))
                 (make-roster "Networks" (list "Arya")))
                "the list obtained from enrollments-to-rosters-bad-2
                is not a bad list of roster"))

;; get-list-of-class-roster-bad2: SetOfEnrollmentAssertion -> ListOfClassRosterAssertion
;; GIVEN: a set of enrollment assertion
;; RETURNS: a set of class roster assertion with classes and corresponding
;; students in the class consisting of duplicate rosters.
;; DESIGN STRATEGY: Combine simpler function.

(define (get-list-of-class-roster-bad2 se)
  (list-of-class-roster-fn get-list-of-students-bad2 se))

;; get-list-of-students-bad2: EnrollmentAssertion SetOfEnrollmentAssertion -> SetOfStudent 
;; GIVEN: an enrollment assertion and a set of enrollment assertion
;; RETURNS: a set of students corresponding to the class they attend but
;; consists of only one element 
;; DESIGN STRATEGY: Using HOF foldl on se

(define (get-list-of-students-bad2 e1 se)
  (foldl
   ;; EnrollmentAssertion SetOfStudent -> SetOfStudent
   ;; Returns: a set of student without duplicates but with only the
   ;; first student in each class
   (lambda (x y) (if  (and (equal? (enrollment-class e1) (enrollment-class x))
                           (not (my-member? (enrollment-student x) y))
                           (empty? y))
                      (cons (enrollment-student x) y)
                      y))
   empty
   se))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;-------------------------------------------------------------------------------------------
;; This function produces a class roster set with an incomplete list as input
;; so it generates incomplete set
;; EXAMPLE: refer test
;; DESIGN STRATEGY: Using HOF foldr on (get-list-of-class-roster-bad3 se)

(define (enrollments-to-rosters-bad-3 se)
  (foldr
   ;; ClassRosterAssertion SetOfClassRosterAssertion -> SetOfClassRosterAssertion
   ;; Returns: a set of class roster assertion without duplicates
     (lambda (y z) (if (not (my-member? y z))
                       (cons y z)
                       z))
     empty
    (get-list-of-class-roster-bad3 se)))

;; TESTS:
(define lst2 (list (make-enrollment "Jon" "PDP")
          (make-enrollment "Sansa" "IR")
          (make-enrollment "Sansa" "PDP")
          (make-enrollment "Arya" "Networks")))

(begin-for-test
  (check-equal? (enrollments-to-rosters-bad-3 lst2)
                (list
                 (make-roster "IR" (list "Sansa"))
                 (make-roster "PDP" (list "Sansa"))
                 (make-roster "Networks" (list "Arya")))
                "the list obtained from enrollments-to-rosters-bad-3
                is not a bad list of roster"))


;; get-list-of-class-roster-bad3: SetOfEnrollmentAssertion -> ListOfClassRosterAssertion
;; GIVEN: a set of enrollment assertion
;; RETURNS: a set of class roster assertion with classes and corresponding
;; students in the class consisting of duplicate rosters.
;; DESIGN STRATEGY: Combine simpler function.

(define (get-list-of-class-roster-bad3 se)
  (list-of-class-roster-fn get-list-of-students-bad3 se))

;; get-list-of-students-bad3: EnrollmentAssertion SetOfEnrollmentAssertion -> SetOfStudent 
;; GIVEN: an enrollment assertion and a set of enrollment assertion
;; RETURNS: a set of students corresponding to the class they attend but
;; takes incomplete list as an input
;; DESIGN STRATEGY: Using HOF foldl on se

(define (get-list-of-students-bad3 e1 se)
  (foldr
   ;; EnrollmentAssertion SetOfStudent -> SetOfStudent
   ;; Returns: a set of student without duplicates
   (lambda (x y) (if (and (equal? (enrollment-class e1) (enrollment-class x))
                          (not (my-member? (enrollment-student x) y)))
                   (cons (enrollment-student x) y)
                   y))
   empty
   (rest se)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; behavior-correct? : ProposedSolution SetOfEnrollmentAssertion -> Boolean
;; GIVEN: a ProposedSolution soln-fn and a SetOfEnrollmentAssertion se
;; RETURNS: true iff the output of soln-fn on se is an example of correct
;; behavior by a ProposedSolution.
;; EXAMPLE: See example above
;; DESIGN STRATEGY: Combine simpler function

(define (behavior-correct? sol-fn se)
   (and (check-set-equal? compare-students-roster se sol-fn)     
        (check-set-equal? compare-class-roster se sol-fn)))

;; CompareFn (compare-fn?) is a function which has the following contract:
;; ClassRosterAssertion SetOfClassRoster -> Boolean

;; check-set-equal?: CompareFn SetOfEnrollmentAssertion ProposedSolution -> Boolean
;; RETURNS: true iff the two sets in any order, are equal 
;; DESIGN STRATEGY: Using HOF andmap on (enrollments-to-rosters se)

(define (check-set-equal? compare-fn? se sol-fn)
  (andmap
   ;; EnrollmentAssertion -> Boolean
   ;; Returns: true iff the enrollment assertion is present in the given set
   (lambda (x) (compare-fn? x (sol-fn se)))
   (enrollments-to-rosters se)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compare-students-roster: ClassRosterAssertion SetOfClassRosterAssertion -> Boolean
;; GIVEN: a ClassRosterAssertion and a set of class roster assertion
;; RETURNS: true iff the set of students of the ClassRosterAssertion
;; is equal to a set of students in the given SetOfClassRosterAssertion

;Example:

;(compare-students-roster (make-roster "pdp" (list "j" "n"))
;                                      (list (make-roster "ir" (list "j" "e"))
;                                            (make-roster "pdp" (list "n" "j"))
;                                            (make-roster "net" (list "c" "d"))))= true

;; DESIGN STRATEGY: Using HOF ormap on scr.

(define (compare-students-roster r1 scr)
  (ormap
   ;; ClassRosterAssertion -> Boolean
   ;; Returns: true iff student sets are equal and classname are equal of the given
   ;; class roster assertion
   (lambda (x) (and (set-equal? (roster-students r1) (roster-students x))
                    (equal? (roster-classname r1) (roster-classname x))))
   scr))

;; compare-class-roster: ClassRosterAssertion SetOfClassRosterAssertion -> Boolean
;; GIVEN: a ClassRosterAssertion
;; RETURNS: true iff the classname is present in the set of class roster.
;; DESIGN STRATEGY: Using HOF ormap on scr.

(define (compare-class-roster r1 scr)
  (ormap
   ;; ClassRosterAssertion -> Boolean
   ;; Returns: true iff classname is equal for the class roster asssertion
   (lambda (x) (equal? (roster-classname r1) (roster-classname x)))
   scr))

;; Sample for tests
(define-struct student (firstname lastname))
(define-struct class (id classname))
(define stud1 (make-student "Jon" "Snow"))
(define stud2 (make-student "Jon" "Targaryen"))
(define stud3 (make-student "Ed" "Stark"))
(define stud4 (make-student "Sansa" "Stark"))
(define stud5 (make-student "Tyrion" "Lannister"))
(define class1 (make-class 001 "PDP"))
(define class2 (make-class 002 "IR"))

(define lst-of-struct (list (make-enrollment stud1 class1)
          (make-enrollment stud1 class2)
          (make-enrollment stud2 class1)
          (make-enrollment stud3 class1)
          (make-enrollment stud4 class2)
          (make-enrollment stud5 class1)))

(define lst (list (make-enrollment "John" "PDP")
          (make-enrollment "Kathryn" "Networks")
          (make-enrollment "Feng" "PDP")
          (make-enrollment "Amy" "PDP")
          (make-enrollment "Amy" "Networks")
          (make-enrollment "Amy" "Networks")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS:

(begin-for-test
  (check-equal? (behavior-correct? enrollments-to-rosters lst)
                true
                "the result should be true for the correct behavior
                 by the sol-fn 'enrollments-to-rosters'")
  (check-equal? (behavior-correct? enrollments-to-rosters lst-of-struct)
                true
                "the result should be true for the correct behavior by the sol-fn
                'enrollments-to-rosters1'")
  (check-equal? (behavior-correct? enrollments-to-rosters-bad-1 lst)
                false
                "the result should be false for the incorrect behavior by the sol-fn
                'enrollments-to-rosters-bad-1'")
  (check-equal? (behavior-correct? enrollments-to-rosters-bad-1 lst-of-struct)
                false
                "the result should be false for the incorrect behavior by the sol-fn
                'enrollments-to-rosters-bad-1'")
  (check-equal? (behavior-correct? enrollments-to-rosters-bad-2 lst)
                false
                "the result should be false for the incorrect behavior by the sol-fn
                'enrollments-to-rosters-bad-2'")
  (check-equal? (behavior-correct? enrollments-to-rosters-bad-2 lst-of-struct)
                false
                "the result should be false for the incorrect behavior by the sol-fn
                'enrollments-to-rosters-bad-2'")
  (check-equal? (behavior-correct? enrollments-to-rosters-bad-3 lst-of-struct)
                false
                "the result should be false for the incorrect behavior by the sol-fn
                'enrollments-to-rosters-bad-3'")
  (check-equal? (behavior-correct? enrollments-to-rosters-bad-3 lst)
                false
                "the result should be false for the incorrect behavior by the sol-fn
                'enrollments-to-rosters-bad-3'"))
