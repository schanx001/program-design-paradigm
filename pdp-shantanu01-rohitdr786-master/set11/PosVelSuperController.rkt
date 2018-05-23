#lang racket

;; displays as an outline rectangle with text showing the x and y
;; coordinates and velocity vx and vy of the particle.

;; the rectangle is draggable

(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "ParticleController.rkt")

(provide PosVelSuperController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A PosVelSuperController is an object whose class inherits the ParticleController class and
;; implements the Controller<%>, CommonControllerHooks interface.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PosVelSuperController Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constructor template for PosVelSuperController%
;; (new PosVelSuperController% [model Model][width NonNegInt][height NonNegInt]
;; [selected? Boolean][particle-x Real][particle-y Real][particle-vx Int][particle-vy Int])
;; Interpretation: An object of class PosVelSuperController% represents a PosVelSuperController
;; which have some abstract methods that are implemented by the subclasses
;; (VelocityController,PositionController). And has an interface PosVelSuperControllerHooks
;; for the abstract methods which is extended by the subclasses.

(define PosVelSuperController%
  (class* ParticleController% ;; inherit method implementations from ParticleController%

    ;; must implement the interface(s) of ParitcleController% + the open
    ;; hooks from the superclass

    (Controller<%> CommonControllerHooks<%>)

    ;; inherit these fields from the superclass
    
    (inherit-field model width height selected?
                   particle-x particle-y particle-vx particle-vy )

    ;; text for the poistion and velocity controller to be displayed
    
    (field [posveltext ""])
    
    (super-new)

    ;---------------------------------------------------------------------------
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT: makes the viewer selected
    ; The viewer stays selected until a button down somewhere else
    
    (define/override (button-down-sub mx my)
      (set! selected? true))
    
    ;---------------------------------------------------------------------------
    ; after-drag : Integer Integer -> Void
    ; this class doesn't respond to the drag event
    
    (define/override (drag-event-sub mx my)
      1)

    ;---------------------------------------------------------------------------
    ;; after-key-event: KeyEvent -> Void
    ;; EFFECT: updates the particle's position and velocity following the key event
    ;; interpret arrow keys as commands to the model
    ;; arrow keys to alter position of the particle
    
    (define/override (after-key-event kev)
      (if selected?
        (cond
          [(key=? "up" kev)
           (send model execute-command
                 (send this after-up-event))]
          [(key=? "down" kev)
           (send model execute-command
                 (send this after-down-event))]
          [(key=? "left" kev)
           (send model execute-command
                 (send this after-left-event))]
          [(key=? "right" kev)
           (send model execute-command
                 (send this after-right-event))])
        'position-controller-after-key-event-value))

    ;-----------------------------------------------------------------------------
    ;; after-up-event: -> Command
    ;; these are the abstract methods, the definition will be provided by the subclasses
    
    (abstract after-up-event
              after-down-event
              after-left-event
              after-right-event)

    ;------------------------------------------------------------------------------
    ;; viewer-image: -> Image
    ;; RETURNS: the image of the viewer
    ;; STRATEGY: assemble the image from the data and rectangle

    (define/override (viewer-image)
      (let ((the-data-image (data-image)))
        (overlay 
          the-data-image
          (rectangle width height
            "outline"
            "black"))))

    ;------------------------------------------------------------------------------
    ;; data-image: -> Image
    ;; Returns: an image of the text to be displayed on the controller
    
    (define (data-image)
      (above
       (text (string-append "Arrow keys change " posveltext) 10 (current-color))
       (text (string-append
                "X = " (number->string particle-x) " Y = " (number->string particle-y))
          12 (current-color))
       (text (string-append
                " VX = " (number->string particle-vx) " VY = " (number->string particle-vy))
          12 (current-color))))
    
    ;-------------------------------------------------------------------------------
    ;; current-color: -> String
    ;; Returns: the string value for color based on selected?
    
    (define (current-color)
      (if selected? "red" "black"))

    ))