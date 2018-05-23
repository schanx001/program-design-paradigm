#lang racket

;; The model consists of a particle, bouncing with its center from x=0
;; to x=150 and y=0 to y=100.  It accepts commands and reports when its status changes

(require rackunit)
(require "extras.rkt")
(require "interfaces.rkt")
(require "PerfectBounce.rkt")

(provide make-model)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Model Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-model: -> Model
;; RETURNS: a new object of class Model

(define (make-model) (new Model%))

;; Constructor template for Model%:
;; (new Model% [x Real][y Real][vx Int][vy Int][controllers ListOfController])
;; Interpretation: An object of class Model% represents a Model.

(define Model%
  (class* object% (Model<%>)

    ;; boundaries of the field
    (field [lo-x 0])
    (field [hi-x 150])
    (field [lo-y 0])
    (field [hi-y 100])

    ;; position and velocity of the object
    (init-field [x (/ (+ lo-x hi-x) 2)])
    (init-field [y (/ (+ lo-y hi-y) 2)])
    (init-field [vx 0][vy 0])

    ;; A ListOfController(LOC) is one of:
    ;; -- empty INTERPRETATION: Has no elements in it
    ;; -- (cons Controller LOC) INTERPRETATION: Is a list of controller
    ;; fn-loc : LOC -> ??
    ;; (define (fn-loc loc)
    ;;   (cond
    ;;     [(empty? loc)...]
    ;;     [else (cons (first loc)
    ;;                 (fn-loc (rest loc)))]))
    
    ; ListOfController.  The list of registered controllers
    (init-field [controllers empty])

    ;; Boolean. Is set to true when any of the XY,X or Y controller is clicked
    ;;          and false when unclicked
    (field [selected? false])

    (super-new)

    ;---------------------------------------------------------------------------
    ;; after-tick: -> Void
    ;; EFFECT:
    ;; moves the object by v when not selected
    ;; limits the resulting x to [0, 150] and y to [0,100].
    ;; publishes x and y at every tick
    ;; publishes velocity in x and y only when it changes
    
    (define/public (after-tick)
      (local
        ((define old-part (make-particle (max (+ lo-x 1) (min x (- hi-x 1)))
                                         (max (+ lo-y 1) (min y (- hi-y 1))) vx vy))
         (define inner-rect (make-rect lo-x hi-x lo-y hi-y))
         (define new-part (if selected?
                              old-part
                              (particle-after-tick old-part inner-rect))))
        (set! x (particle-x new-part))
        (set! y (particle-y new-part))
        (publish-position)
        (set! vx (particle-vx new-part))
        (set! vy (particle-vy new-part))
        (publish-velocity)
        "somebody tried to use the value of model.rkt after-tick"))
    
    ;----------------------------------------------------------------------------
    ;; register: Controller -> Void
    ;; EFFECT:
    ;; register the new controller and send it some data
    
    (define/public (register c)
      (begin
        (set! controllers (cons c controllers))
        (send c receive-signal (make-report-position x y))
        (send c receive-signal (make-report-velocity vx vy))))

    ;----------------------------------------------------------------------------
    ;; execute-command: Command -> Void
    ;; EFFECT:
    ;; decodes the command, executes it, and sends updates to the
    ;; controllers.
    
    (define/public (execute-command cmd)
      (cond
        [(set-position? cmd)
         (begin
           (set! x (set-position-pos-x cmd))
           (set! y (set-position-pos-y cmd))
           (publish-position))]
        [(incr-velocity? cmd)
         (begin
           (set! vx (incr-velocity-dv-x cmd))
           (set! vy (incr-velocity-dv-y cmd))
           (publish-velocity))]
        [(pause-world? cmd)
         (set! selected? (pause-world-pause? cmd))]))

    

    ;-------------------------------------------------------------------------
    ;; publish-position: -> Void
    ;; EFFECT: report position to each controller
    
    (define (publish-position)
      (let ((msg (make-report-position x y)))
        (for-each
          (lambda (obs) (send obs receive-signal msg))
          controllers)
        ))

    ;--------------------------------------------------------------------------
    ;; publish-velocity: -> Void
    ;; EFFECT: report velocity to each controller
    
    (define (publish-velocity)
      (let ((msg (make-report-velocity vx vy)))
        (for-each
          (lambda (obs) (send obs receive-signal msg))
          controllers)))

    ;; The model responds to after-tick, but not to any of the other
    ;; SWidget messages
    (define/public (after-button-down mx my) 'trap)
    (define/public (after-button-up mx my) 'trap)
    (define/public (after-drag mx my) 'trap)
    (define/public (after-move mx my) 'trap)
    (define/public (after-key-event kev) 'trap)
    (define/public (add-to-scene s) s)

    ;; test methods
    (define/public (for-test:get-x) x)
    (define/public (for-test:get-y) y)
    (define/public (for-test:get-vx) vx)
    (define/public (for-test:get-vy) vy)
    (define/public (for-test:get-selected?) selected?)
    
    ))

;;; tests

;; check to see if the model responds to incr-velocity commands (it
;; does) and to after-tick messages

(begin-for-test

  (let*
      ((m (make-model)))
    (begin
      (check-equal? (send m for-test:get-x) 75)
      (check-equal? (send m for-test:get-vx) 0)
      (send m after-tick)
      (check-equal? (send m for-test:get-x) 75)
      (check-equal? (send m for-test:get-vx) 0)

      (send m execute-command (make-incr-velocity 77 2))
      (check-equal? (send m for-test:get-vx) 77)

      (send m after-tick)
      (check-equal? (send m for-test:get-x) 150)
      (check-equal? (send m for-test:get-vx) -77)

      (send m after-tick)
      (check-equal? (send m for-test:get-x) 72)
      (check-equal? (send m for-test:get-vx) -77)

      (send m execute-command (make-set-position 75 50))
      (check-equal? (send m for-test:get-x) 75)
      (check-equal? (send m for-test:get-y) 50)
      
      (send m execute-command (make-pause-world true))
      (check-equal? (send m for-test:get-selected?) true)
      (send m after-tick)
      (check-equal? (send m for-test:get-x) 75)
      (check-equal? (send m for-test:get-y) 50)
      )))

;; m is definitely responding to after-tick messages.  Is it not
;; getting the after-tick messages from big-bang?

;; Ans: yes, that's the problem:  In top.rkt, I created the model, but
;; never added it to the Container (duh!)





    
  




    

    
