#lang racket

(require "WidgetWorks.rkt")
(require "Interfaces.rkt")
(require "VelocityController.rkt")
(require "PositionController.rkt")
(require "XController.rkt")
(require "YController.rkt")
(require "XYController.rkt")
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require "Model.rkt")

(provide make-controller-factory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A ControllerFactory is an object whose class implements the SWidget<%>
;; interface.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ControllerFactory Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-controller-factory : Container Model -> SWidget
;; GIVEN: a container and a model
;; RETURNS: a new object of the class ControllerFactory

(define (make-controller-factory c m)
  (new ControllerFactory% [c c][m m]))

;; Contstructor Template for ControllerFactory%:
;; (new ControllerFactory% [c c][m m]))
;; Interpretation: An object of class ControllerFactory% represents a Controller
;; Factory where new controllers will be created.

(define ControllerFactory%
  (class* object% (SWidget<%>)

    ; the container in which the controllers will live
    (init-field c)   ; Container

    ; the model to which the controllers will be connected
    (init-field m)   ; Model

    (super-new)

    ;------------------------------------------------------------------
    ;; after-key-event: KeyEvent -> Void
    ;; EFFECT: creates a controller based on the key event
    ;; "v" for velocity controller
    ;; "p" for position controller
    ;; "x" for x controller
    ;; "y" for y controller
    ;; "z" for xy controller
    
    (define/public (after-key-event kev)
      (cond
        [(key=? kev "v") (add-viewer make-velocity-controller)]
        [(key=? kev "p") (add-viewer make-position-controller)]

        [(key=? kev "x") (add-viewer make-x-controller)]
        [(key=? kev "y") (add-viewer make-y-controller)]
        [(key=? kev "z") (add-viewer make-xy-controller)]
        ))


    ;; (Model -> Controller) -> Void
    (define/public (add-viewer make-viewer)
      (send c add-stateful-widget (make-viewer m)))

    ;; the factory is invisible
    (define/public (add-to-scene s) s)

    ;; the factory doesn't respond to any other events
    (define/public (after-tick) 'controller-factory-after-tick-trap)
    (define/public (after-button-down mx my)
      'controller-factory-after-button-down-trap) 
    (define/public (after-drag mx my)
      'controller-factory-after-drag-trap) 
    (define/public (after-move mx my)
      'controller-factory-after-move-trap) 
    (define/public (after-button-up mx my)
      'controller-factory-after-button-up-trap)

    ))


;; tests:
(begin-for-test

  (let*
      ((m (make-model))
       (c (container-init 600 500))
       (cont (make-controller-factory c m)))
    (begin
      (send cont after-key-event "z")
      (send cont after-key-event "x")
      (send cont after-key-event "y")
      (send cont after-key-event "p")
      (send cont after-key-event "v"))))