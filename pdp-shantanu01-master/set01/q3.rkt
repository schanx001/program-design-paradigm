;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Question3 of problem set01 to count the number of pixels in a given image.

(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(check-location "01" "q3.rkt")

(provide image-area)

(define cat (bitmap "cat1.png"))
(define star-shape (star 12 "outline" "blue"))
(define rectangle-shape (rectangle 5 5 "solid" "red"))

;; DATA DEFINITIONS: NONE
;; image-area: Image -> NonNegReal
;; GIVEN: an image with some width and height
;; RETURNS: the number of pixels in the image
;; EXAMPLES:
;; (image-area cat) = 8775
;; (image-area star-shape) = 342
;; (image-area rectangle-shape) = 25
;; DESIGN STRATEGY: Combine simpler functions
(define (image-area img)
  (* (image-width img) (image-height img)))
;; TESTS
(begin-for-test
  (check-equal? (image-area cat) 8775 "The number of pixels in the image should be 8775")
  (check-equal? (image-area star-shape) 342 "The number of pixels in the image should be 342")
  (check-equal? (image-area rectangle-shape) 25 "The number of pixels in the image should be 25"))

 
