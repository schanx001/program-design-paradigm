#lang racket

;; arrow keys increments or decrements location of the particle by 5

(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "PosVelSuperController.rkt")

(provide make-position-controller)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A PositionController is an object whose class inherits the PosVelSuperController class and
;; implements the Controller<%>, CommonControllerHooks and PosVelControllerHooks
;; interface.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PositionController Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-position-controller : Model -> Controller
;; GIVEN: a model m
;; RETURNS: A position controller for m

(define (make-position-controller m)
  (new PositionController% [model m]))

;; Constructor template for PositionController%
;; (new PositionController% [model Model][posveltext String][particle-x Real]
;; [particle-y Real][width NonNegInt] [height NonNegInt])
;; Interpretation:An object of class PositionController% represents a PositionController
;; which shows the position of the particle within the rectangle.
;; A position controller which enables us to change the position of the particle,
;; by using the arrow keys to move the particle in the x or y direction.

(define PositionController%
  (class* PosVelSuperController% ;; inherit method implementations from PosVelSuperController%

    ;; must implement the interface(s) of XYSuperController% + the open
    ;; hooks from the superclass
    
    (Controller<%> CommonControllerHooks<%> PosVelControllerHooks<%>)

    ;; inherit these fields from the superclass

    (inherit-field posveltext particle-x particle-y width height)
    
    (super-new)

    ;; updates the posveltext to "position"
     
    (set! posveltext "position")

    ;-------------------------------------------------------------------------------
    ;; after-up-event: -> Command
    ;; Returns: a command to set the position for the particle
    ;; DETAILS: decreases the y value of the position by 5
    
    (define/override (after-up-event)
      (make-set-position particle-x (max (- particle-y 5) 1)))

    ;-------------------------------------------------------------------------------
    ;; after-down-event: -> Command
    ;; Returns: a command to set the position for the particle
    ;; DETAILS: increases the y value of the position by 5
    
    (define/override (after-down-event)
      (make-set-position particle-x (min (+ particle-y 5) (- height 1))))

    ;-------------------------------------------------------------------------------
    ;; after-left-event: -> Command
    ;; Returns: a command to set the position for the particle
    ;; DETAILS: decreases the x value of the position by 5
    
    (define/override (after-left-event)
      (make-set-position (max (- particle-x 5) 1) particle-y))

    ;-------------------------------------------------------------------------------
    ;; after-right-event: -> Command
    ;; Returns: a command to set the position for the particle
    ;; DETAILS: increases the x value of the position by 5
    
    (define/override (after-right-event)
      (make-set-position (min (+ particle-x 5) (- width 1)) particle-y))

    ;; For test:
    (define/public (for-test:particle-x)
      particle-x)
    (define/public (for-test:particle-y)
      particle-y)
    
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; for testing, we'll create a model and a controller for it.
;;; We'll send some messages to the controller, and see if the 
;;; model gets updated appropriately.

(require rackunit)
(require "extras.rkt")
(require "Model.rkt")

(begin-for-test

  (let*
      ((m (make-model))
       (c (make-position-controller m)))
    (begin
      (check-equal? (send c for-test:particle-x) 75)
      (check-equal? (send c for-test:particle-y) 50)
      (check-equal? (send m for-test:get-x) 75)
      (check-equal? (send m for-test:get-y) 50)

      ;; send m a make-set-position command directly, and see if it
      ;; responds. 
      (send m execute-command (make-set-position 70 55))
      (check-equal? (send m for-test:get-x) 70)
      (check-equal? (send m for-test:get-y) 55)

      ;; now send the controller key events.  See if that causes the model to
      ;; respond properly.
      (send c after-button-down 300 250)
      (send c after-key-event "up")
      (send c after-key-event "right")
      (check-equal? (send m for-test:get-x) 75)
      (check-equal? (send m for-test:get-y) 50)
      (send c after-key-event "left")
      (send c after-key-event "down")
      (check-equal? (send m for-test:get-x) 70)
      (check-equal? (send m for-test:get-y) 55)

      ;; does m respond to an after-tick message (yes).
      (send m after-tick)
      (check-equal? (send m for-test:get-x) 70)
      (check-equal? (send m for-test:get-y) 55)

      )))

