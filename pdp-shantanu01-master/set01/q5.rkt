;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Question5 of problem set01 to delete the ith position from a given string.

(require rackunit)
(require "extras.rkt")
(check-location "01" "q5.rkt")
(provide string-delete)

;; DATA DEFINITIONS: NONE
;; string-delete: String NonNegInt -> String
;; GIVEN: a string and a number i which is between 0(inclusive)
;;        and the length of the string(exclusive)
;; RETURNS: a string with the ith position letter deleted from the string
;; EXAMPLES:
;; (string-delete "helloworld" 5) -> "helloorld"
;; (string-delete " " 0) -> ""
;; (string-delete "" 0) -> "Can't delete from an empty string as 'i' should be less than the length of the string"
;; DESIGN STRATEGY: Combine simpler functions
(define (string-delete str i)
  (string-append (substring str 0 i) (substring str (+ i 1))))
;; TESTS:
(begin-for-test (check-equal? (string-delete "helloworld" 0) "elloworld" "The string should be 'helloorld' ")
                (check-equal? (string-delete " " 0) "" "The string should be '' "))