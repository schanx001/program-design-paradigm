#lang racket

(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "Interfaces.rkt")
(require "Model.rkt")
(require "ControllerFactory.rkt")

(check-location "11" "top.rkt")
(provide run)
;; run with (run 0.5)

;; create a container, install a factory, and run.
(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run : PosReal -> Void
;; GIVEN: a frame rate, in sec/tick
;; EFFECT: Creates and runs the MVC simulation ;; with the given frame rate.

(define (run rate)
  (let ((c (container-init CANVAS-WIDTH CANVAS-HEIGHT))
        (m (make-model)))
    (begin
      (send c add-stateful-widget m)
      (send c add-stateful-widget (make-controller-factory c m))
      (send c run rate))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;