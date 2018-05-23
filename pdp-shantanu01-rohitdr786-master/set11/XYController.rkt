#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "XYSuperController.rkt")

(provide make-xy-controller)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A XYController is an object whose class inherits the XYController class and
;; implements the Controller<%>, CommonControllerHooks and XYSuperControllerHooks
;; interface.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XYController Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-xy-controller : Model -> Controller
;; GIVEN: a model m
;; RETURNS: A xy controller for m

(define (make-xy-controller m)
  (new XYController% [model m][width 190][height 140]))

;; Constructor template for XYController%
;; (new XYController% [model Model][width NonNegInt][height NonNegInt]
;;                          [saved-mx Real][saved-my Real][particle-x Real][particle-y Real]
;;                          [inner-rect-width NonNegInt][inner-rect-height NonNegInt])
;; Interpretation: An object of class XYController% represents a XYController
;; which shows a representation of the particle bouncing in the rectangle.
;; With this controller, the user can drag the particle using the mouse.
;; Dragging the mouse causes the particle to follow the mouse pointer via a Smooth Drag.

(define XYController%
  (class* XYSuperController% ;; inherit method implementations from XYSuperController%
    
    ;; must implement the interface(s) of XYSuperController% + the open
    ;; hooks from the superclass
    
    (Controller<%> CommonControllerHooks<%> XYSuperController<%>)
    
    ;; inherit all these fields from the superclass:
    ;; saved mouse position in x and y
    ;; center coordinates of particle in x and y
    ;; width height of the controller
    ;; inner rectangle's width and height
    
    (inherit-field width height saved-mx saved-my particle-x particle-y
                   inner-rect-width inner-rect-height)
    
    (super-new)
    
    ;; setting the width and height according to this xy controller
    
    (set! inner-rect-width 150)
    (set! inner-rect-height 100)
    
    ;----------------------------------------------------------------------------------
    ;; data-image-xy: -> Scene
    ;; Returns: a scene like the previous one, but with the particle object drawn on it.
    
    (define/override (data-image-xy)
      (super data-image particle-x particle-y))
    
    ;; data-image: NonNegInteger NonNegInteger -> Scene
    ;; Returns: a scene like the previous one, but with the particle object drawn on it.
    
    (define/override (data-image x1 y1)
      this)
    
    ;-----------------------------------------------------------------------------------
    ;; button-down-sub-xy: Integer Integer -> Void
    ;; EFFECT: sets the saved mx and my values considering the  particle x and y
    
    (define/override (button-down-sub-xy mx my)
      (set! saved-mx (- mx particle-x))
      (set! saved-my (- my particle-y)))
    
    ;------------------------------------------------------------------------------------
    ;; drag-event-sub-xy: Integer Integer -> Void
    ;; EFFECT: updates the particle's x and y center coordinates following the drag event
    
    (define/override (drag-event-sub-xy mx my)
      (set! particle-x (super within-limits 1 (- mx saved-mx) (- inner-rect-width 1)))
      (set! particle-y (super within-limits 1 (- my saved-my) (- inner-rect-height 1))))
    
    ;; within-limits: Real Real Real -> Real 
    ;; Returns: a value if it lies within the given range or else returns the min or max value
    
    (define/override (within-limits lo value high)
      this)
    
    ;; For test:
    (define/public (for-test:particle-x)
      particle-x)
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
       (c (make-xy-controller m)))
    (begin
      (check-equal? (send c for-test:particle-x) 75)
      (check-equal? (send c for-test:particle-y) 50)
      (check-equal? (send m for-test:get-x) 75)
      (check-equal? (send m for-test:get-y) 50)
      
      ;; send m a make-incr-velocity command directly, and see if it
      ;; responds. 
      (send m execute-command (make-incr-velocity 5 5))
      (send m after-tick)
      (check-equal? (send c for-test:particle-x) 80)
      (check-equal? (send c for-test:particle-y) 55)
      (check-equal? (send m for-test:get-x) 80)
      (check-equal? (send m for-test:get-y) 55)
      
      
      ;; now do a button-down on the controller and drag it and check if the
      ;; particle moves to right position
      (send c button-down-sub-xy 300 250)
      (send c drag-event-sub-xy 310 260)
      (check-equal? (send c for-test:particle-x) 90)
      (check-equal? (send c for-test:particle-y) 65)
      (check-equal? (send c data-image-xy)
                    (place-image (circle 2 "solid" "black")
                                 (send c for-test:particle-x)
                                 (send c for-test:particle-y)
                                 (place-image (circle 10 "solid" "red")
                                              (send c for-test:particle-x)
                                              (send c for-test:particle-y)
                                              (empty-scene 150 100))))
      
      )))