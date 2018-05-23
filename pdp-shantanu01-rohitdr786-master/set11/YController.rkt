#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "XYSuperController.rkt")

(provide make-y-controller)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A YController is an object whose class inherits the XYController class and
;; implements the Controller<%>, CommonControllerHooks and XYSuperControllerHooks
;; interface.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YController Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-y-controller : Model -> Controller
;; GIVEN: a model m
;; RETURNS: A y controller for m

(define (make-y-controller m)
  (new YController% [model m][width 50][height 140]))

;; Constructor template for YController%
;; (new YController% [model Model] [width NonNegInt] [height NonNegInt]
;;  [saved-my Real] [particle-y Real] [inner-rect-width NonNegInt] [inner-rect-height])
;; Interpretation: An object of class YController% represents a YController
;; which shows a representation of the particle bouncing in the rectangle.
;; A Y controller, which is like the X controller except that it works in the y direction.

(define YController%
  (class* XYSuperController% ;; inherit method implementations from XYSuperController%
    
    ;; must implement the interface(s) of XYSuperController% + the open
    ;; hooks from the superclass
    
    (Controller<%> CommonControllerHooks<%> XYSuperController<%>)
    
    ;; inherit these fields from the superclass
    
    (inherit-field model width height saved-my particle-y
                   inner-rect-width inner-rect-height)
    
    (super-new)
    
    ;; setting the width and height according to this y controller
    
    (set! inner-rect-width 50)
    (set! inner-rect-height 100)
    
    ;----------------------------------------------------------------------------------
    ;; data-image-xy: -> Scene
    ;; Returns: a scene like the previous one, but with the particle object drawn on it.
    
    (define/override (data-image-xy)
      (super data-image (/ width 2) particle-y))
    
    ;; data-image: NonNegInteger NonNegInteger -> Scene
    ;; Returns: a scene like the previous one, but with the particle object drawn on it.
    
    (define/override (data-image x1 y1)
      this)
    
    ;-----------------------------------------------------------------------------------
    ;; button-down-sub-xy: Integer Integer -> Void
    ;; EFFECT: sets the saved my value only considering the particle y for this controller
    
    (define/override (button-down-sub-xy mx my)
      (set! saved-my (- my particle-y)))
    
    ;------------------------------------------------------------------------------------
    ;; drag-event-sub-xy: Integer Integer -> Void
    ;; EFFECT: updates the particle's y coordinate only following the drag event
    ;; for this controller
    
    (define/override (drag-event-sub-xy mx my)
      (set! particle-y (super within-limits 1 (- my saved-my) (- inner-rect-height 1)))
      )
    
    ;; within-limits: Real Real Real -> Real 
    ;; Returns: a value if it lies within the given range or else returns the min or max value
    
    (define/override (within-limits lo value high)
      this)
    
    ;; For test:
    (define/public (for-test:particle-y)
      particle-y)
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTING:

;;; for testing, we'll create a model and a controller for it.
;;; We'll send some messages to the controller, and see if the 
;;; model gets updated appropriately.

(require rackunit)
(require "extras.rkt")
(require "Model.rkt")

(begin-for-test
  
  (let*
      ((m (make-model))
       (c (make-y-controller m)))
    (begin
      (check-equal? (send c for-test:particle-y) 50)
      (check-equal? (send m for-test:get-x) 75)
      (check-equal? (send m for-test:get-y) 50)
      
      ;; send m a make-incr-velocity command directly, and see if it
      ;; responds. 
      (send m execute-command (make-incr-velocity 5 5))
      (send m after-tick)
      (check-equal? (send c for-test:particle-y) 55)
      (check-equal? (send m for-test:get-x) 80)
      (check-equal? (send m for-test:get-y) 55)
      
      
      ;; now do a button-down on the controller and drag it and check if the
      ;; particle moves to right position
      (send c button-down-sub-xy 300 250)
      (send c drag-event-sub-xy 310 260)
      (check-equal? (send c for-test:particle-y) 65)
      (check-equal? (send c data-image-xy)
                    (place-image (circle 2 "solid" "black")
                                 25
                                 (send c for-test:particle-y)
                                 (place-image (circle 10 "solid" "red")
                                              25
                                              (send c for-test:particle-y)
                                              (empty-scene 50 100))))
      
      )))