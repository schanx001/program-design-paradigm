;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Question1 of problem set01 to compute distance of point (x,y) to the origin. 

(require rackunit)
(require "extras.rkt")
(check-location "01" "q1.rkt")

(provide distance-to-origin)
;; DATA DEFINITIONS: NONE
;; distance-to-origin: Real Real -> NonNegReal
;; GIVEN: a point with x and y coordinates
;; RETURNS: the distance of point(x,y) to the origin
;; EXAMPLES:
;; (distance-to-origin 10 5) = #i11.180339887498949
;; (distance-to-origin -3 4) = 5
;; (distance-to-origin -11 -13) = #i17.029386365926403
;; (distance-to-origin 12 -5) = 13
;; DESIGN STRATEGY: Combine simpler functions
(define (distance-to-origin x y)
	(sqrt (+ (sqr x) (sqr y))))
;; TESTS
(begin-for-test
  (check-equal? (distance-to-origin 10 5) #i11.180339887498949 "The distance to origin should be #i11.180339887498949 units")
  (check-equal? (distance-to-origin -3 4) 5 "The distance to origin should be 5 units")
  (check-equal? (distance-to-origin -11 -13) #i17.029386365926403 "The distance to origin should be #i17.029386365926403 units")
  (check-equal? (distance-to-origin 12 -5) 13 "The distance to origin should be 13 units"))

