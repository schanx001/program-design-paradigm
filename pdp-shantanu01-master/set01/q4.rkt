;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Question4 of problem set01 to insert "_" in the
;; ith position of a given string which can be empty.

(require rackunit)
(require "extras.rkt")
(check-location "01" "q4.rkt")

(provide string-insert)
;; DATA DEFINITIONS: NONE
;; string-insert: String NonNegInt -> String
;; GIVEN: a string and a number i which is between 0 and the length of
;;        the given string (inclusive)
;; RETURNS: a string with "_" inserted in the ith position of the given string
;; EXAMPLES:
;; (string-insert "helloworld" 5) -> "hello_world"
;; (string-insert "helloworld" 0) -> "_helloworld"
;; (string-insert "helloworld" 10) -> "helloworld_"
;; (string-insert "" 0) -> "_"
;; DESIGN STRATEGY: Combine simpler functions
(define (string-insert str i)
  (string-append (substring str 0 i) "_" (substring str i)))
;; TESTS
(begin-for-test (check-equal? (string-insert "helloworld" 5) "hello_world" "The string should be 'hello_world' ")
                (check-equal? (string-insert "helloworld" 0) "_helloworld" "The string should be '_helloworld' ")
                (check-equal? (string-insert "helloworld" 10) "helloworld_" "The string should be 'helloworld_' ")
                (check-equal? (string-insert "" 0) "_" "The string should be '_' "))

