;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Question2 of problem set01 to get the last 1String of a non-empty string.

(require rackunit)
(require "extras.rkt")
(check-location "01" "q2.rkt")

(provide string-last)

;; DATA DEFINITIONS: NONE
;; string-last: String -> String
;; GIVEN: a non-empty string
;; RETURNS: the last 1String of the given string
;; EXAMPLES:
;; (string-last "Hello Universe") = "e"
;; (string-last "Jello") = "o"
;; DESIGN STRATEGY: Combine simpler functions
(define (string-last str)
  (string-ith str (- (string-length str) 1)))
;; TESTS
(begin-for-test
  (check-equal? (string-last "Hello Universe") "e" "The last 1String should be e ")
  (check-equal? (string-last "Jello") "o" "The last 1String should be o "))