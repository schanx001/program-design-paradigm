#lang racket

;; arrow keys increments or decrements velocity of the particle by 5

(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "PosVelSuperController.rkt")

(provide make-velocity-controller)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A VelocityController is an object whose class inherits the PosVelSuperController class and
;; implements the Controller<%>, CommonControllerHooks and PosVelControllerHooks
;; interface.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VelocityController Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-velocity-controller : Model -> Controller
;; GIVEN: a model m
;; RETURNS: A velocity controller for m

(define (make-velocity-controller m)
  (new VelocityController% [model m]))


;; Constructor template for VelocityController%
;; (new VelocityController% [model Model][posveltext String][particle-vx Real]
;; [particle-vy Real])
;; Interpretation:An object of class VelocityController% represents a VelocityController
;; which shows the velocity of the particle within the rectangle.
;; A Velocity controller which enables us to change the velocity of the particle,
;; by using the arrow keys to alter the velocity of the particle in the x or y direction.

(define VelocityController%
  (class* PosVelSuperController% ;; inherit method implementations from PosVelSuperController%

    ;; must implement the interface(s) of XYSuperController% + the open
    ;; hooks from the superclass
    
    (Controller<%> CommonControllerHooks<%> PosVelControllerHooks<%>)

    ;; inherit these fields from the superclass
    
    (inherit-field posveltext particle-vx particle-vy)

    (super-new)

    ;; updates the posveltext to "velocity"
    
    (set! posveltext "velocity")

    ;-------------------------------------------------------------------------------
    ;; after-up-event: -> Command
    ;; Returns: a command to set the velocity for the particle
    ;; DETAILS: decreases the vy velocity by 5
    
    (define/override (after-up-event)
      (make-incr-velocity particle-vx (- particle-vy 5)))

    ;-------------------------------------------------------------------------------
    ;; after-down-event: -> Command
    ;; Returns: a command to set the velocity for the particle
    ;; DETAILS: increases the vy velocity by 5
    
    (define/override (after-down-event)
      (make-incr-velocity particle-vx (+ particle-vy 5)))

    ;-------------------------------------------------------------------------------
    ;; after-left-event: -> Command
    ;; Returns: a command to set the velocity for the particle
    ;; DETAILS: decreases the vx velocity by 5
    
    (define/override (after-left-event)
      (make-incr-velocity (- particle-vx 5) particle-vy))

    ;-------------------------------------------------------------------------------
    ;; after-right-event: -> Command
    ;; Returns: a command to set the velocity for the particle
    ;; DETAILS: increases the vx velocity by 5
    
    (define/override (after-right-event)
      (make-incr-velocity (+ particle-vx 5) particle-vy))

    ;; For test:
    (define/public (for-test:particle-vx)
      particle-vx)
    (define/public (for-test:particle-vy)
      particle-vy)
    
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
       (c (make-velocity-controller m)))
    (begin
      (check-equal? (send c for-test:particle-vx) 0)
      (check-equal? (send c for-test:particle-vy) 0)
      (check-equal? (send m for-test:get-x) 75)
      (check-equal? (send m for-test:get-y) 50)

      ;; send m a make-incr-velocity command directly, and see if it
      ;; responds. 
      (send m execute-command (make-incr-velocity 5 10))
      (check-equal? (send m for-test:get-vx) 5)
      (check-equal? (send m for-test:get-vy) 10)

      ;; now send the controller key event. See if that causes the model to
      ;; respond properly.
      (send c after-button-down 300 250)
      (send c after-key-event "up")
      (send c after-key-event "right")
      (check-equal? (send m for-test:get-vx) 10)
      (check-equal? (send m for-test:get-vy) 5)
      (send c after-key-event "left")
      (send c after-key-event "down")
      (check-equal? (send m for-test:get-vx) 5)
      (check-equal? (send m for-test:get-vy) 10)

      ;; does m respond to an after-tick message (yes).
      (send m after-tick)
      (check-equal? (send m for-test:get-x) 80)
      (check-equal? (send m for-test:get-y) 60)

      )))

   


