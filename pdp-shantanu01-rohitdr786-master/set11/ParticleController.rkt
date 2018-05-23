#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")

(provide ParticleController%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A ParticleController is an object whose class implements the Controller interface.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ParticleController Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constructor template for ParticleController%
;; (new ParticleController% [model Model][width NonNegInt][height NonNegInt]
;; [x NonNegInteger][y NonNegInteger])
;; Interpretation: An object of class PosVelSuperController% represents a PosVelSuperController
;; which have some abstract methods that are implemented by the subclasses
;; (XYSuperController,PosVelSuperController). And has an interface CommonControllerHooks
;; for the abstract methods which is extended by the subclasses.

(define ParticleController%
  (class* object% (Controller<%>)

    (init-field model)  ; the model

    ; Position of the center of the controller
    ; both of these are NonNegInts.
    (init-field [x 300] [y 250])   

    ; width and height of the controller.
    (init-field [width 150][height 100])

    (field [half-width  (/ width  2)])
    (field [half-height (/ height 2)])

    ;; Handle width and height. Both PosInts
    (field [handle-w 10][handle-h 10])

    ;; controller's cache of the position and velocity of the
    ;; particle.

    (field [particle-x 0])
    (field [particle-y 0])
    (field [particle-vx 0])
    (field [particle-vy 0])

    ;; fields for dragging
    ;; if selected? then position of last button-down relative to
    ;; center of viewer; else any value

    (field [handle-selected? false])
    (field [selected? false])
    (field [saved-mx 0])
    (field [saved-my 0])

    (super-new)

    ;; at initialization, register this controller with the model
    (send model register this)

    ;---------------------------------------------------------------------------
    ;; receive-signal: Signal -> Void
    ;; decodes signal and updates local data
    
    (define/public (receive-signal sig)
      (cond
        [(report-position? sig)
         (set! particle-x (report-position-pos-x sig))
         (set! particle-y (report-position-pos-y sig))]
        [(report-velocity? sig)
         (set! particle-vx (report-velocity-v-x sig))
         (set! particle-vy (report-velocity-v-y sig))]))

    ;----------------------------------------------------------------------------
    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT: makes the viewer selected
    ; STRATEGY: Cases on whether the event is in handle or controller area
    ; of this object
    
    (define/public (after-button-down mx my)
      (cond
        [(in-handle? mx my) (begin
                              (set! handle-selected? true)
                              (set! saved-mx (- mx x))
                              (set! saved-my (- my y)))]
        [else (if (in-controller? mx my)
                  (send this button-down-sub mx my)
                  (set! selected? false))]))

    ;-----------------------------------------------------------------------------
    ;; button-down-sub: -> Void
    ;; this is an abstract method and definition will be provided by the sub class 

    (abstract button-down-sub)

    ;-----------------------------------------------------------------------------
    ;; in-handle?: Integer Integer -> Boolean
    ;; Returns: true iff the mouse pointer location is in the handle
    
    (define (in-handle? mx my)
      (and (<= (- x half-width) mx (- (+ x 10) half-width))
           (<= (- y half-height) my (- (+ y 10) half-height))))

    ;-----------------------------------------------------------------------------
    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; EFFECT: makes this unselected

    (define/public (after-button-up mx my)
      (set! handle-selected? false))

    ;-----------------------------------------------------------------------------
    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether this is selected.
    ; If it is selected, move it so that the vector from its position to
    ; the drag event is equal to saved-mx.  Report the new position to
    ; the registered balls.

    (define/public (after-drag mx my)
      (if handle-selected?
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my)))
          (send this drag-event-sub mx my)))

    ;------------------------------------------------------------------------------
    ;; drag-event-sub: -> Void
    ;; this an abstract method and definition will be provided by the sub class
    
    (abstract drag-event-sub)
    
    ;; the velocityh controller doesn't respond to mouse move events
    (define/public (after-move mx my)
      'velocity-controller-after-move-value)

    ;------------------------------------------------------------------------------
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this controller painted
    ;; on it.
    ;; STRATEGY: place the image centered at x y

    (define/public (add-to-scene scene)
      (place-image (send this viewer-image) x y
                   (place-image (rectangle 10 10 "outline" (if handle-selected? "red" "black"))
                                (- (+ x 5) half-width) (- (+ y 5) half-height) scene)))
    
    ;; the controller doesn't respond to ticks
    (define/public (after-tick)
      'velocity-controller-after-tick-value)

    ;; the controller doesn't respond to key event
    (define/public (after-key-event key)
      1)

    ;-------------------------------------------------------------------------------
    ;; viewer-image: -> Image
    ;; this is an abstract method and definition will be provided by the subclass.
    
    (abstract viewer-image)

    ;; for testing
    (define/public (for-test:handle-select?)
      (set! handle-selected? true))

    ;--------------------------------------------------------------------------------
    ;; in-controller?: Integer Integer -> Boolean
    ;; Returns: true iff the mouse poninter coordinates is in the controller
    
    (define (in-controller? mx my)
      (and (<= (- x half-width) mx (+ x half-width))
           (<= (- y half-height) my (+ y half-height))))

    ))


