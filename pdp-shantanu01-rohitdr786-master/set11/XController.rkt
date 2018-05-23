#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "XYSuperController.rkt")

(provide make-x-controller)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A XController is an object whose class inherits the XYController class and
;; implements the Controller<%>, CommonControllerHooks and XYSuperControllerHooks
;; interface.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XController Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-x-controller : Model -> Controller
;; GIVEN: a model m
;; RETURNS: A x controller for m

(define (make-x-controller m)
  (new XController% [model m][width 190][height 50]))

;; Constructor template for XController%
;; (new XController% [model Model] [width NonNegInt] [height NonNegInt]
;;  [saved-mx Real] [particle-x Real] [inner-rect-width NonNegInt] [inner-rect-height])
;; Interpretation: An object of class XController% represents a XController
;; which shows a representation of the particle bouncing in the rectangle.
;; Which is like the XY controller, except that it displays only the x coordinate
;; of the particle's motion. Dragging the mouse in the X controller alters the
;; particle's position in the x direction.

(define XController%
  (class* XYSuperController% ;; inherit method implementations from XYSuperController%
    
    ;; must implement the interface(s) of XYSuperController% + the open
    ;; hooks from the superclass
    
    (Controller<%> CommonControllerHooks<%> XYSuperController<%>)
    
    ;; inherit these fields from the superclass
    
    (inherit-field width height saved-mx particle-x
                   inner-rect-width inner-rect-height)
    
    (super-new)
    
    ;; setting the width and height according to this x controller
    
    (set! inner-rect-width 150)
    (set! inner-rect-height 50)
    
    ;----------------------------------------------------------------------------------
    ;; data-image-xy: -> Scene
    ;; Returns: a scene like the previous one, but with the particle object drawn on it.
    
    (define/override (data-image-xy)
      (super data-image particle-x (/ height 2)))
    
    ;; data-image: NonNegInteger NonNegInteger -> Scene
    ;; Returns: a scene like the previous one, but with the particle object drawn on it.
    
    (define/override (data-image x1 y1)
      this)
    
    ;-----------------------------------------------------------------------------------
    ;; button-down-sub-xy: Integer Integer -> Void
    ;; EFFECT: sets the saved mx value only considering the particle x for this controller
    
    (define/override (button-down-sub-xy mx my)
      (set! saved-mx (- mx particle-x)))
    
    ;------------------------------------------------------------------------------------
    ;; drag-event-sub-xy: Integer Integer -> Void
    ;; EFFECT: updates the particle's x coordinate only following the drag event
    ;; for this controller
    
    (define/override (drag-event-sub-xy mx my)
      (set! particle-x (super within-limits 1 (- mx saved-mx) (- inner-rect-width 1))))
    
    ;; within-limits: Real Real Real -> Real 
    ;; Returns: a value if it lies within the given range or else returns the min or max value
    
    (define/override (within-limits lo value high)
      this)
    
    ;; For test:
    (define/public (for-test:particle-x)
      particle-x)
    
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
       (c (make-x-controller m)))
    (begin
      (check-equal? (send c for-test:particle-x) 75)
      (check-equal? (send m for-test:get-x) 75)
      (check-equal? (send m for-test:get-y) 50)
      
      ;; send m a make-incr-velocity command directly, and see if it
      ;; responds. 
      (send m execute-command (make-incr-velocity 5 5))
      (send m after-tick)
      (check-equal? (send c for-test:particle-x) 80)
      (check-equal? (send m for-test:get-x) 80)
      (check-equal? (send m for-test:get-y) 55)
      
      
      ;; now do a button-down on the controller and drag it and check if the
      ;; particle moves to right position
      (send c button-down-sub-xy 300 250)
      (send c drag-event-sub-xy 310 260)
      (check-equal? (send c for-test:particle-x) 90)
      (check-equal? (send c data-image-xy)
                    (place-image (circle 2 "solid" "black")
                                 (send c for-test:particle-x)
                                 25
                                 (place-image (circle 10 "solid" "red")
                                              (send c for-test:particle-x)
                                              25
                                              (empty-scene 150 50))))
      
      )))