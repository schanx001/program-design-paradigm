#lang racket

;; interfaces for MVC example

(require "WidgetWorks.rkt")

(provide Controller<%> Model<%> CommonControllerHooks<%>
         PosVelControllerHooks<%> XYSuperController<%>)

;; structs for model command language
(provide 
  (struct-out set-position) 
  (struct-out incr-velocity)
  (struct-out report-position)
  (struct-out report-velocity)
  (struct-out pause-world))

;; A Controller is an object of any class that implements
;; Controller<%>

;; There will be several such classes, and there may be several
;; objects of each such class.

(define Controller<%>    
  (interface (SWidget<%>)

    ;; Signal -> Void
    ;; receive a signal from the model and adjust controller
    ;; accordingly 
    receive-signal
    
    ))

;; A Model is an object of any class that implements Model<%>.  Models
;; will receive signals from the Container, so they must implement the
;; SWidget<%> interface in order to do so.

(define Model<%>
  (interface (SWidget<%>)

    ;; Controller -> Void
    ;; Registers the given controller to receive signal
    register          

    ;; Command -> Void
    ;; Executes the given command
    execute-command   
))

;; CONTROLLER/MODEL PROTOCOL:

;; As soon as a controller registers with the model, the model sends
;; the controller a pair of Signals so the controller will know the
;; current state of the model.

;; The controller then sends the model commands, which the model is
;; supposed to execute.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS FOR COMMUNICATING WITH MODEL

;; A Command is one of 
;; -- (make-set-position x y)     
;; -- (make-incr-velocity dvx dvy)
;; -- (make-pause-world pause?)

;; A Signal is one of
;; -- (make-report-position x y)
;; -- (make-report-velocity vx vy)

;; x, y, dvx, dvy, vx, vy are all Reals.
;; pause? is a boolean

;; provide the structs for Command and Signal
;; the syntax of provide in #lang racket has more options in it.


(define-struct set-position (pos-x pos-y) #:transparent)
(define-struct incr-velocity (dv-x dv-y) #:transparent)
(define-struct report-position (pos-x pos-y) #:transparent)
(define-struct report-velocity (v-x v-y) #:transparent)
(define-struct pause-world (pause?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interface for common controller hooks:

;; A CommonController is an object of any class that implements
;; CommonControllerHooks<%>

;; There will be several such classes, and there may be several
;; objects of each such class.

(define CommonControllerHooks<%>    
  (interface (Controller<%>)

    ;; Integer Integer -> Void
    ;; Updates the selected field and sends a message to the model
    ;; about the change following the mouse button down event
    button-down-sub

    ;; -> Image
    ;; Assembles the image from the data and rectangle and returns an image
    viewer-image

    ;; Integer Integer -> Void
    ;; Updates the position of the particle by sending a message to the model
    drag-event-sub

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interface for position velocitiy controllers hooks:

;; A PosVelControllerHooks is an object of any class that implements
;; PosVelControllerHooks<%>

;; There will be several such classes, and there may be several
;; objects of each such class.

(define PosVelControllerHooks<%>
  (interface (CommonControllerHooks<%>)

    ;; -> Command
    ;; Sends model a command to set the velocity for the particle
    after-up-event
    after-down-event
    after-left-event
    after-right-event))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interface for common controller hooks:

;; A XYSuperController is an object of any class that implements
;; XYSuperController<%>

;; There will be several such classes, and there may be several
;; objects of each such class.

(define XYSuperController<%>
  (interface (CommonControllerHooks<%>)

    ;; -> Scene
    ;; Creates a scene with the particle object drawn on it.
    data-image-xy

    ;; Integer Integer -> Void
    ;; Updates the offset values considering the  particle and mouse positions
    button-down-sub-xy

    ;; Integer Integer -> Void
    ;; Updates the particle positions according to mouse drag and offset values
    drag-event-sub-xy

    ;; Real Real Real -> Real 
    ;; Returns a value within certain range
    within-limits))







