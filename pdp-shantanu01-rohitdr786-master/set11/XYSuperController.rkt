#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "ParticleController.rkt")

(provide XYSuperController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A XYSuperController is an object whose class inherits the ParticleController class and
;; implements the Controller<%>, CommonControllerHooks interface.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XYSuperController Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constructor template for XYSuperController%
;; (new XYSuperController% [model Model][width NonNegInt][height NonNegInt]
;; [particle-x Real][particle-y Real][particle-vx Int][particle-vy Int]
;; [handle-selected? Boolean][saved-mx Real][saved-my Real][selected? Boolean])
;; Interpretation: An object of class XYSuperController% represents a XYSuperController
;; which have some abstract methods that are implemented by the subclasses
;; (XYController,XController,YController). And has an interface XYSuperControllerHooks
;; for the abstract methods which is extended by the subclasses.

(define XYSuperController%
  (class* ParticleController% ;; inherit method implementations from ParticleController%

    ;; must implement the interface(s) of ParitcleController% + the open
    ;; hooks from the superclass
    
    (Controller<%> CommonControllerHooks<%>)

    ;; inherit these fields from the superclass
    
    (inherit-field model)  ; the model
    (inherit-field width height particle-x particle-y particle-vx particle-vy
                   handle-selected? saved-mx saved-my selected?)

    ;; width and height of the inner rectangle is initialized
    
    (field [inner-rect-width 0] [inner-rect-height 0])
    
    (super-new)

    ;--------------------------------------------------------------------------------------
    ;; viewer-image: -> Image
    ;; RETURNS: the image of the viewer
    ;; STRATEGY: assemble the image from the data and rectangle

    (define/override (viewer-image)   
      (let ((the-data-image (send this data-image-xy)))        
         (overlay
          (rectangle
           width
           height
           "outline"
           "black")
          (rectangle
           inner-rect-width
           inner-rect-height
           "outline"
           "blue")
          the-data-image 
          )))

    ;---------------------------------------------------------------------------------------
    ;; data-image-xy: -> Scene
    ;; Returns: a scene
    ;; this is a abstract method, definition will be supplied by the subclass
    
    (abstract data-image-xy)

    ;---------------------------------------------------------------------------------------
    ;; data-image: Integer Integer -> Scene
    ;; RETURNS: a scene like before, but with the particle drawn onto it
    
    (define/public (data-image x1 y1)
          (place-image (circle 2 "solid" "black")
                       x1
                       y1
                       (place-image (circle 10 "solid" "red")
                                    x1
                                    y1
                                    (empty-scene inner-rect-width
                                                 inner-rect-height))))

    ;----------------------------------------------------------------------------------------
    ;; button-down-sub: Integer Integer -> Void
    ;; EFFECT: updates the selected field and sends a message to the model
    ;; about the change following the button down event
    
    (define/override (button-down-sub mx my)
          (begin
            (set! selected? true)
            (send model execute-command (make-pause-world true))
            (send this button-down-sub-xy mx my)
            ))

    ;---------------------------------------------------------------------------------------
    ;; button-down-sub-xy: -> Void
    ;; EFFECT: updates the saved-mx and saved-my for the particle
    ;; this is a abstract method, definition will be supplied by the subclass
    
    (abstract button-down-sub-xy)


    ;----------------------------------------------------------------------------------------
    ;; drag-event-sub: Integer Integer -> Void
    ;; EFFECT: updates the position of the particle by sending a message to the model
    
    (define/override (drag-event-sub mx my)
      (if selected?
          (begin
            (send this drag-event-sub-xy mx my)
            (send model execute-command (make-set-position particle-x particle-y)))
          1))

    ;----------------------------------------------------------------------------------------
    ;; button-drag-event-sub-xy: -> Void
    ;; EFFECT: updates the saved-mx and saved-my for the particle
    ;; this is a abstract method, definition will be supplied by the subclass
    
    (abstract drag-event-sub-xy)

    ;----------------------------------------------------------------------------------------
    ;; within-limits: Real Real Real -> Real 
    ;; Returns: a value if it lies within the given range or else returns the min or max value

    (define/public (within-limits lo value high)
      (max lo (min value high)))
    
    ;----------------------------------------------------------------------------------------
    ;; after-button-up: Integer Integer -> Void
    ;; EFFECT: updates the handle selected and controller selected following the button up
    ;; event and sends a message to the model
    
    (define/override (after-button-up mx my)
      (set! handle-selected? false)
      (set! selected? false)
      (send model execute-command (make-pause-world false)))

    ))