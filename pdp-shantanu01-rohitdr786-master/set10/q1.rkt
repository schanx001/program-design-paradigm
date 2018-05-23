;; MetaToy program

#lang racket

(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "WidgetWorks.rkt")

(provide make-metatoy
         run
         make-throbber
         make-clock
         make-politician
         Metatoy<%>
         Toy<%>)

(check-location "10" "q1.rkt")

;; the metatoy will consist of a list of Toys<%>'s.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; start with (run framerate).  Typically: (run 0.25)

;; Press "t" for throbber.  
;; Throbber expand/shrinks. 

;; Press "c" for clock
;; Clock ticks 

;; Press "p" for politician
;; Politician follows mouse pointer

;;; CONSTANTS

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; some arbitrary choices
(define THROBBER-INITIAL-X (/ CANVAS-WIDTH 2))
(define THROBBER-INITIAL-Y (/ CANVAS-HEIGHT 2))

(define RADIUS 5)
(define INITIAL-CHANGE 3)
(define SOLID-CIRCLE (circle RADIUS "solid" "green"))
(define OUTLINE-CIRCLE (circle RADIUS "outline" "green"))

(define CLOCK-INITIAL-X (/ CANVAS-WIDTH 2))
(define CLOCK-INITIAL-Y (/ CANVAS-HEIGHT 2))

(define INITIAL-TOY-X (/ CANVAS-WIDTH 2))
(define INITIAL-TOY-Y (/ CANVAS-HEIGHT 2))

(define NEW-THROBBER-EVENT "t")
(define NEW-CLOCK-EVENT "c")
(define NEW-POLITICIAN-EVENT "p")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data Definitions

;; A Metatoy is an object whose class implements the Metatoy<%>
;; interface.

;; A Toy is an object whose class implements the Toy<%>
;; interface.

;; ListOfToy(LOT) is one of:
;; --empty
;; --(cons Toy LOT)

;; (define(lot-fn lot)
;;   (cond
;;    [(empty? lot)...]
;;    [else (...(first lot)
;;                    (lot-fn(rest lot)))]))

;; HALTING MEASURE : (length lot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERFACES

;; A Toy is an object of any class that implements Toy<%>
;; You will probably have three such classes, one for each kind of toy. 

(define Toy<%> 
  (interface
      
      ;; The interface Toy<%> inherits from the interface SWidget<%>.
      ;; This means that any class that implements Toy<%> must implement
      ;; all the methods from SWidget<%> plus all the methods defined here.
      (SWidget<%>)
    
    ;; -> Int
    ;; RETURNS: the x or y position of the center of the toy
    toy-x
    toy-y
    
    ;; -> Int
    ;; RETURNS: some data related to the toy.  The interpretation of
    ;; this data depends on the class of the toy.
    ;; for a throbber, it is the current radius of the throbber
    ;; for the clock, it is the current value of the clock
    ;; for a politician, it is the current distance to the mouse
    toy-data
    
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Metatoy is an object of any class that implements Metatoy<%>.
;; (You will only need one such class)

(define Metatoy<%>
  (interface 
      
      ;; the (SWidget<%>) says that Metatoy<%> inherits from SWidget<%>
      ;; This means that any class that implements Metatoy<%> must
      ;; implement all the methods from SWidget<%> plus all the methods
      ;; defined here. In this case, there is just one additional method,
      ;; called get-toys.
      (SWidget<%>)
    
    ;; -> ListOfToy
    get-toys
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; run : PosNum -> Void
;; GIVEN: a frame rate (in seconds/tick)
;; EFFECT: This function creates a Container, and places a MetaToy with
;; no toys in that Container.  The function may or may not put other
;; Widgets and SWidgets in the container, depending on the
;; implementation. The function then runs the Container at the given
;; frame rate using WidgetWorks.

(define (run rate)
  (local
    ((define initial-container (container-init CANVAS-WIDTH CANVAS-HEIGHT))
     (define initial-metatoy (make-metatoy empty))
     (define meta-in-cont (send initial-container add-stateful-widget initial-metatoy)))
    (send initial-container run rate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The Metatoy% class

;; We will have only one class that implements Metatoy<%>: Metatoy%

;; Constructor template for Metatoy%:
;; (new Metatoy% [objs ListOfToy])
;; Interpretation: An object of class Metatoy% takes signals from
;; big-bang and distributes them to its objects as appropriate.

;; make-metatoy : ListOfToy -> Metatoy
;; RETURNS: a Metatoy with the given list of toys.
;; NOTE: The Metatoy<%> interface extends the SWidgets<%> interface, so the
;; result of make-metatoy is something that big-bang can use as a world.

(define (make-metatoy objs)
  (new Metatoy% [objs objs]))

(define Metatoy%
  (class* object% (Metatoy<%>)
    
    (init-field objs) ;; ListOfToy
    ;; initial x coordinate
    (field [x INITIAL-TOY-X])
    ;; initial y coordinate
    (field [y INITIAL-TOY-Y])
    
    (super-new)
    
    ;;-----------------------------------------------------------------------------
    
    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this swidget to the state it should have
    ;; following a tick.
    ;; Use HOFC map on the Toy's in this Metatoy
    
    (define/public (after-tick)
      (set! objs
            (map
             ;; Toy -> Void
             (lambda (obj) (begin (send obj after-tick) obj))
             objs)))
    
    ;;-----------------------------------------------------------------------------
    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;; painted on it.
    ;; Use HOFC map on the Toy's in this Metatoy    
    
    (define/public (add-to-scene s)
      (foldr
       ;; Toy -> Scene
       (lambda (obj scene)
         (send obj add-to-scene scene))
       s
       objs))
    
    ;;-----------------------------------------------------------------------------
    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates this swidget to the state it should have
    ;; following the given key event
    ;; STRATEGY: Cases on kev
    ;; "p", "t" and "c" create new politician, new throbber and new clock;
    ;; other keystrokes are passed on to the objects in the Metatoy.
    
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-THROBBER-EVENT)
         (set! objs
               (cons (make-throbber x y) objs)
               )]
        [(key=? kev NEW-CLOCK-EVENT)
         (set! objs
               (cons (make-clock x y) objs)
               )]
        [(key=? kev NEW-POLITICIAN-EVENT)
         (set! objs
               (cons (make-politician x y) objs)
               )]
        [else
         (set! objs
               (map
                ;; Toy -> Void
                (lambda (obj) (begin (send obj after-key-event kev) obj))
                objs)
               )]))
    
    
    ;;--------------------------------------------------------------------------------
    
    ;; after-button-down: Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this swidget to the state it should have
    ;; following the button down mouse event at the given location.
    
    (define/public (after-button-down mx my)
      (set! objs
            (map
             ;; Toy -> Void
             (lambda (obj) (begin (send obj after-button-down mx my) obj))
             objs)
            ))
    
    ;;--------------------------------------------------------------------------------
    
    ;; after-button-up: Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this swidget to the state it should have
    ;; following the button up mouse event at the given location.
    
    (define/public (after-button-up mx my)
      (set! objs
            (map
             ;; Toy -> Void
             (lambda (obj) (begin (send obj after-button-up mx my) obj))
             objs)
            ))
    
    ;;--------------------------------------------------------------------------------
    
    ;; after-drag: Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this swidget to the state it should have
    ;; following the drag mouse event at the given location.
    
    (define/public (after-drag mx my)
      (set! objs
            (map
             ;; Toy -> Void
             (lambda (obj) (begin (send obj after-drag mx my) obj))
             objs)
            ))
    
    ;;--------------------------------------------------------------------------------
    
    ;; after-move: Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this swidget to the state it should have
    ;; following the move mouse event at the given location.
    
    (define/public (after-move mx my)
      (set! objs
            (map
             ;; Toy -> Void
             (lambda (obj) (begin (send obj after-move mx my) obj))
             objs)
            ))
    
    ;;--------------------------------------------------------------------------------
    
    ;; -> ListOfToy
    
    (define/public (get-toys)
      objs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS for metatoy

(begin-for-test
  (local ((define METATOY-OBJ (new Throbber% [x 250]
                                   [y 300]
                                   [radius 5]
                                   [change 2]
                                   [selected? false]))
          (define METATOY (make-metatoy (list METATOY-OBJ))))
    (check-equal? (send METATOY add-to-scene EMPTY-CANVAS)
                  (place-image SOLID-CIRCLE 250 300 EMPTY-CANVAS)
                  "empty canvas should be displayed")
    (check-equal? (send METATOY get-toys)
                  (list METATOY-OBJ)
                  "the output should be list of metatoy objects")
    (check-equal? (void? (send METATOY after-tick))
                  true
                  "it should return true for void")
    (check-equal? (void? (send (make-metatoy empty) after-key-event "t"))
                  true
                  "output should be true")
    (check-equal? (void? (send (make-metatoy empty) after-key-event "c"))
                  true
                  "output should be true")
    (check-equal? (send (make-metatoy empty) add-to-scene EMPTY-CANVAS)
                  EMPTY-CANVAS
                  "empty canvas should be displayed")
    (send METATOY after-key-event "p")
    (send METATOY after-key-event "k")
    (send METATOY after-button-down 250 300)
    (send METATOY after-button-up 250 300)
    (send METATOY after-drag 250 300)
    (send METATOY after-move 250 300)
    (check-equal? (send METATOY get-toys)
                  (send METATOY get-toys)
                  "the list of objects should be returned")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Throbber start at the center of the canvas.
;; They are selectable and draggable.

;; Constructor template for Throbber%:
;; (new Throbber% [x Integer][y Integer][radius NonNegInteger][change Integer]
;;            [selected? Boolean][saved-mx Integer][saved-my Integer])
;; the last 3 arguments are optional
;; Interpretation: An object of class Throbber% represents a throbber.

(define Throbber%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from one throbber to
    ;; the next.
    
    ; the x and y position of the center of the throbber
    (init-field x y radius change)   
    
    ; is the throbber selected? Default is false.
    (init-field [selected? false]) 
    
    ;; if the throbber is selected, the position of
    ;; the last button-down event inside the throbber, relative to the
    ;; throbber's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    
    ; the throbber's min radius
    (field [r-min 5])
    ; the throbber's max radius
    (field [r-max 20])
    
    ; image for displaying the throbber
    (field [THROBBER-IMG-SOLID (circle radius "solid" "green")])
    (field [THROBBER-IMG-OUTLINED (circle radius "outline" "green")])                      
    
    (super-new)
    
    ;;-------------------------------------------------------------------------
    ;; after-tick : -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates this swidget to the state it should have
    ; following a tick.
    
    (define/public (after-tick)
      (begin
        (set! radius (get-radius))
        (set! change (get-rate))))
    
    ;;--------------------------------------------------------------------------
    ;; get-radius: -> Int
    ;; RETURNS: a changed radius after evalutaion
    
    (define (get-radius)
      (min (max r-min (+ radius change))
           r-max))
    
    ;;--------------------------------------------------------------------------
    ;; get-rate: -> Int
    ;; RETURNS: a change value for further evaluation of radius value
    
    (define (get-rate)
      (if (or (= r-min (get-radius))
              (= r-max (get-radius)))
          (- change)
          change))
    
    ;;---------------------------------------------------------------------------
    ;; after-key-event :; KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates this swidget to the state it should have
    ;; following the given key event
    ;; DETAILS: a throbber ignores key events
    
    (define/public (after-key-event kev)
      this)      
    
    ;;---------------------------------------------------------------------------
    ;; after-button-down : Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this swidget to the state it should have
    ;; following the button down mouse event at the given location.
    ;; STRATEGY: Cases on whether the event is in the throbber
    
    (define/public (after-button-down mx my)
      (if (in-throbber? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y)))
          this))
    
    ;;----------------------------------------------------------------------------
    ;; after-button-up : Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this swidget to the state it should have
    ;; following the button up mouse event at the given location.
    
    (define/public (after-button-up mx my)
      (if selected?
          (set! selected? false)
          1))   
    
    ;;----------------------------------------------------------------------------
    ;; after-drag : Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this swidget to the state it should have
    ;; following the drag mouse event at the given location.
    ;; STRATEGY: Cases on whether the throbber is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my))
            (set! selected? true))
          this))   
    
    ;;----------------------------------------------------------------------------    
    ;; add-to-scene :; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    
    (define/public (add-to-scene scene)
      (if selected?
          (place-image (circle radius "outline" "green") x y scene)
          (place-image (circle radius "solid" "green") x y scene)))
    
    ;;-------------------------------------------------------------------------
    
    ;; in-throbber? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this throbber.
    
    (define (in-throbber? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr radius)))
    
    ;;-------------------------------------------------------------------------
    
    ;; toy-x: -> Int
    ;; Returns: the center x of the toy
    
    (define/public (toy-x)
      x)
    
    ;;--------------------------------------------------------------------------
    
    ;; toy-y: -> Int
    ;; Returns: the center y of the toy
    
    (define/public (toy-y)
      y)
    
    ;;--------------------------------------------------------------------------
    
    ;; toy-data: -> Int
    ;; Returns: the radius data of the toy
    
    (define/public (toy-data)
      radius)
    
    ;;--------------------------------------------------------------------------
    
    ;; after-move: Int Int -> Toy
    ;; Returns: the radius data of the toy
    
    (define/public (after-move mx my)
      this)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-throbber: PosInt PosInt -> Toy
;; GIVEN: an x and a y position
;; RETURNS: an object representing a throbber at the given position.

(define (make-throbber x y)
  (new Throbber%
       [x x]
       [y y]
       [radius RADIUS]
       [change INITIAL-CHANGE]))

;; TESTS for throbber:
(begin-for-test
  (local ((define THROBBER (make-throbber 250 300))
          (define THROBBER1 (new Throbber% [x 250]
                                 [y 300]
                                 [radius 18]
                                 [change 3])))
    (check-equal? (send THROBBER add-to-scene EMPTY-CANVAS)
                  (place-image SOLID-CIRCLE 250 300 EMPTY-CANVAS)
                  "the image of circle should be solid when unselected")
    (send THROBBER after-button-down 251 301)
    (check-equal? (send THROBBER add-to-scene EMPTY-CANVAS)
                  (place-image OUTLINE-CIRCLE 250 300 EMPTY-CANVAS)
                  "the output should be
                  (place-image OUTLINE-CIRCLE 250 300 EMPTY-CANVAS)")
    (send THROBBER after-tick)
    (check-equal? (send THROBBER toy-x)
                  250
                  "the toy-x of the throbber should return 250")
    (check-equal? (send (send THROBBER1 after-key-event "k") toy-y)
                  300
                  "the toy-y should be 300")
    (send THROBBER1 after-button-down 255 300)
    (send THROBBER1 after-drag 255 300)
    (check-equal? (send THROBBER1 toy-x)
                  250
                  "the toy-x should be 250")
    
    
    (send THROBBER1 after-button-down 245 360)
    (check-equal? (send THROBBER1 toy-x)
                  250
                  "the toy-x should be 250")
    (send THROBBER1 after-button-up 255 300)
    (check-equal? (send THROBBER1 toy-x)
                  250
                  "the toy-x should be 250")
    (send THROBBER1 after-button-up 200 360)
    (check-equal? (send THROBBER toy-x)
                  250
                  "the toy-x should be 250")
    (send THROBBER1 after-drag 255 300)
    (check-equal? (send THROBBER1 toy-x)
                  250
                  "the toy-x should be 250")
    (check-equal? (send THROBBER1 toy-data)
                  18
                  "the toy-data should be 18")
    (check-equal? (send (send THROBBER1 after-move 250 300) toy-data)
                  18
                  "the radius should be 18")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constructor template for Clock%:
;; (new Clock% [x Integer][y Integer][time Integer]
;;            [selected? Boolean][mx Integer][my Integer])
;; Interpretation: An object of class Clock% represents a clock.

(define Clock%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from one clock to
    ;; the next.
    
    ; the x and y position of the center of the clock and time to display 
    (init-field x y time)   
    
    ; is the clock selected? Default is false.
    (init-field [selected? false]) 
    
    ;; if the clock is selected, the position of
    ;; the last button-down event inside the clock, relative to the
    ;; clock's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    
    ;; width of the clock
    (field [width 40])
    ;; height of the clock 
    (field [height 20])
    ;; font type is constant being used in format function (inbuilt)
    (field [font-type "~a"])
    ;; font size is the size of the text to be displayed
    (field [font-size 12])
    ;; font-color is the color of the text to be displayed
    (field [font-color "red"])
    
    ; image for displaying the clock
    (field [CLOCK-IMG-OUTLINED (rectangle width height "outline" "navy")])
    
    (super-new)
    
    ;-------------------------------------------------------------------------
    
    ;; after-tick : -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates this swidget to the state it should have
    ; following a tick.
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (set! time (+ time 1)))
    
    ;-------------------------------------------------------------------------
    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates this swidget to the state it should have
    ;; following the given key event
    ;; DETAILS: a clock ignores key events
    (define/public (after-key-event kev)
      this)      
    
    ;-------------------------------------------------------------------------
    
    ;; after-button-down : Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this swidget to the state it should have
    ;; following the button down mouse event at the given location.
    
    (define/public (after-button-down mx my)
      (if (in-clock? mx my)
          (begin
            (set! saved-mx (- mx x))
            (set! saved-my (- my y))
            (set! selected? true))
          this))
    
    ;--------------------------------------------------------------------------
    
    ; after-button-up : Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this swidget to the state it should have
    ;; following the button up mouse event at the given location.
    ;; STRATEGY: Cases on whether the event is in the clock.
    ;; If the clock is selected, then unselect it.
    
    (define/public (after-button-up mx my)
      (if selected?
          (set! selected? false)
          1))   
    
    ;---------------------------------------------------------------------------
    
    ; after-drag : Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this swidget to the state it should have
    ;; following the drag mouse event at the given location.
    ;; STRATEGY: Cases on whether the clock is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my))
            (set! selected? true))
          this))   
    
    ;----------------------------------------------------------------------------
    
    ;; add-to-scene :; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    
    (define/public (add-to-scene scene)
      (place-image  (overlay (text (format font-type time)
                                   font-size
                                   font-color)
                             CLOCK-IMG-OUTLINED) x y scene))
    
    ;----------------------------------------------------------------------------
    
    ;; in-clock? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this clock.
    
    (define (in-clock? other-x other-y)
      (and (<= (- x (/ width 2)) other-x (+ x (/ width 2)))
           (<= (- y (/ height 2)) other-y (+ y (/ height 2)))))
    
    ;-----------------------------------------------------------------------------
    
    ;; toy-x: -> Integer
    ;; toy-y: -> Integer
    ;; Returns: the x and y center coordinates of the toy
    
    (define/public (toy-x)
      x)
    
    (define/public (toy-y)
      y)
    
    ;; toy-data: -> Integer
    ;; Returns: the time of the clock
    
    (define/public (toy-data)
      time)
    
    ;; after-move: Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this widget to the state it should have
    ;; following the move mouse event at the given location.
    ;; Details: Clock doesnot have a behavior for this method
    
    (define/public (after-move mx my)
      this)
    
    ;; test methods, to probe the clock state.  Note that we don't have
    ;; a probe for radius.
    
    ))

;; make-clock: -> Toy
;; GIVEN: no arguments
;; RETURNS: a new object of class Clock% near the bottom of the screen.

(define (make-clock x y)
  (new Clock%
       [x x]
       [y y]
       [time 0]))

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; TESTS: For Clock

(begin-for-test
  (local ((define CLOCK (make-clock 250 300))
          )
    (check-equal? (send CLOCK add-to-scene EMPTY-CANVAS)
                  (place-image (overlay (text (format "~a" 0) 12 "red")
                                        (rectangle 40 20 "outline" "navy"))
                               250 300 EMPTY-CANVAS) "should return an image with clock")
    (send CLOCK after-tick)
    (send CLOCK after-key-event "k")
    (send CLOCK after-button-down 251 301)
    (check-equal? (send CLOCK toy-x)
                  250
                  "the toy-x should return 250")
    (send CLOCK after-button-down 200 350)
    
    (check-equal? (send CLOCK toy-x)
                  250
                  "the toy-x should return 250")
    (send CLOCK after-button-up 251 301)
    (check-equal? (send CLOCK toy-x)
                  250
                  "the toy-x should return 250")
    (send CLOCK after-button-up 200 360)
    (check-equal? (send (send CLOCK after-move 250 300) toy-data)
                  1
                  "the toy-x should return 250")
    (send CLOCK after-button-down 250 300)
    (send CLOCK after-drag 250 300)
    (check-equal? (send CLOCK toy-y)
                  300
                  "the toy-y should return 300")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; POLITICIAN CLASS

;; Constructor template for Politician%:
;; (new Politician% [x Integer][y Integer][slope Integer]
;;     [velocity Integer][hc? Boolean][saved-mx Integer][saved-my Integer])
;; Interpretation: An object of class Politician% represents a politician.

(define Politician%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from one politician to
    ;; the next.
    
    ;; Intial x and y positions of the politician in the canvas
    (define POLITICIAN-INITIAL-X (/ CANVAS-WIDTH 2))
    (define POLITICIAN-INITIAL-Y (/ CANVAS-HEIGHT 2))
    
    ;; the x and y position of the center of the politician
    (init-field [x POLITICIAN-INITIAL-X] [y POLITICIAN-INITIAL-Y])
    (init-field [slope 999999])
    
    ;; The velocity by which the politician moves towards mouse pointer
    (field [POLITICIAN-VELOCITY 20])
    
    (init-field [velocity (* -1 POLITICIAN-VELOCITY)])
    
    ;; HC? is a init-field containing value true iff curent face of politician is of
    ;; Hillary Clinton
    (init-field [HC? true])
    
    ;; saved-mx and saved-my are the mouse positions of the last move of the mouse
    (init-field [saved-mx POLITICIAN-INITIAL-X] [saved-my 0])
    
    
    ;; images for displaying faces of the politician
    (field [HC-IMG (bitmap "hillary-clinton.jpg")])
    (field [DT-IMG (bitmap "donald-trump.jpg")])
    
    
    
    ;; The distance by which the politician jums away from the mouse pointer
    (field [POLITICIAN-JUMP 200])
    
    ;; The radius within which the politician gets scared after entering 
    (field [SCARE-RADIUS 75])
    
    (super-new)
    
    ;---------------------------------------------------------------------------
    
    ; after-tick: -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates this swidget to the state it should have
    ; following a tick.
    ;; STRATEGY: Divide into cases on near-mouse?
    
    (define/public (after-tick)
      (local
        ((define new-x (updated-x velocity))
         (define new-y (updated-y new-x)))
        (if (near-mouse? x y saved-mx saved-my)
            (begin
              (set! HC? (not HC?))
              (set! x (jump-x))
              (set! y (jump-y)))
            (begin
              (set! x new-x)
              (set! y new-y)))))
    
    ;---------------------------------------------------------------------------
    
    ;; near-mouse? : Real Real Real Real -> Boolean
    ;; GIVEN: coordinates 1 x1, y1 and coordinates 2 x2, y2
    ;; RETURNS: true iff distance between them is less than or equal to set SCARE-RADIUS
    ;; STRATEGY: Combine simpler functions
    
    (define (near-mouse? x1 y1 x2 y2)
      (<= (distance-calc x1 y1 x2 y2) SCARE-RADIUS))
    
    ;---------------------------------------------------------------------------
    
    ;; updated-x : Int -> Real
    ;; GIVEN: a value for updating value of x co-ordinate
    ;; RETURNS: the updated value of x
    ;; STRATEGY: Combine simpler functions
    
    (define (updated-x vel)
      (+ (/ vel (sqrt (+ (sqr slope) 1))) x))
    
    ;---------------------------------------------------------------------------
    
    ;; updated-y : Int -> Real
    ;; GIVEN: a value for updating value of y co-ordinate
    ;; RETURNS: the updated value of y
    ;; STRATEGY: Combine simpler functions
    
    (define (updated-y x2)
      (+ (* slope (- x2 x)) y))
    
    ;---------------------------------------------------------------------------
    
    ;; jump-x: -> Real
    ;; RETURNS: new coordinate of x after being repelled away from mouse pointer
    ;; STRATEGY: Cases on value of (updated-velocity)
    
    (define (jump-x)
      (if (< velocity 0)
          (updated-x POLITICIAN-JUMP)
          (updated-x (* -1 POLITICIAN-JUMP))))
    
    ;---------------------------------------------------------------------------
    
    ;; jump-y: -> Real
    ;; RETURNS: new coordinate of y after being repelled away from mouse pointer
    ;; STRATEGY: Combine simpler functions
    
    (define (jump-y)
      (+ (* slope (- (jump-x) x)) y))
    
    ;---------------------------------------------------------------------------
    
    ;; distance-calc : Real Real Real Real -> NonNegReal
    ;; GIVEN: two coordinates (x1,y1), (x2,y2)
    ;; RETURNS: the distance between the two coordinates
    ;; STRATEGY: Combine simpler functions
    
    (define (distance-calc nx1 ny1 nx2 ny2 )
      (sqrt (+ (sqr (- nx1 nx2))
               (sqr (- ny1 ny2)))))
    
    ;---------------------------------------------------------------------------
    
    ;; after-key-event: KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates this widget to the state it should have
    ;; following the given key event
    
    ;; DETAILS: a politician ignores key events
    
    (define/public (after-key-event kev)
      this)
    
    ;---------------------------------------------------------------------------
    
    ;; after-button-down : Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this swidget to the state it should have
    ;; following the button down mouse event at the given location.
    ;; STRATEGY: Combine simpler functions
    
    (define/public (after-button-down mx my)
      (after-move mx my))
    
    ;---------------------------------------------------------------------------
    
    ;; after-button-up : Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this swidget to the state it should have
    ;; following the button up mouse event at the given location.
    ;; STRATEGY: Combine simpler functions
    
    (define/public (after-button-up mx my)
      this)
    
    ;---------------------------------------------------------------------------
    
    ;; after-drag: Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this swidget to the state it should have
    ;; following the drag mouse event at the given location.
    ;; STRATEGY: Combine simpler functions
    
    (define/public (after-drag mx my)
      (after-move mx my))
    
    ;---------------------------------------------------------------------------
    
    ;; after-move : Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this swidget to the state it should have
    ;; following the move mouse event at the given location.
    ;; STRATEGY: Cases on near-mouse? iff true jump away from mouse position
    ;;           else come near
    
    (define/public (after-move mx my)
      (if (near-mouse? x y mx my)
          (begin
            (set! HC? (not HC?))
            (set! x (jump-x))
            (set! y (jump-y))
            (set! saved-mx mx)
            (set! saved-my my)
            (set! slope (updated-slope))
            (set! velocity (updated-velocity)))
          (begin
            (set! saved-mx mx)
            (set! saved-my my)
            (set! slope (updated-slope))
            (set! velocity (updated-velocity)))))
    
    ;---------------------------------------------------------------------------
    
    ;; updated-slope : -> Real
    ;; RETURNS: a values for slope of the line from
    ;;          politician current position to mouse position
    ;; STRATEGY: Cases on (= saved-mx x)
    
    (define (updated-slope)
      (if (= saved-mx x)
          9999
          (/ (- saved-my y) (- saved-mx x))))
    
    ;---------------------------------------------------------------------------
    
    ;; updated-velocity : -> Real
    ;; RETURNS: a value for the velocity depending in positon of
    ;;          mouse pointer related to politician's center
    ;; STRATEGY: Cases on (>= x saved-mx)
    
    (define (updated-velocity)
      (if (>= x saved-mx)
          (* -1 POLITICIAN-VELOCITY)
          POLITICIAN-VELOCITY))
    
    ;---------------------------------------------------------------------------
    
    ;; add-to-scene : Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    ;; STRATEGY: Combine simpler functions
    
    (define/public (add-to-scene scene)
      (place-image  (if HC? HC-IMG DT-IMG) x y scene))
    
    ;---------------------------------------------------------------------------
    
    ;; toy-x : -> Int
    ;; RETURNS: the x position of the politician
    ;; STRATEGY: Combine simpler functions
    
    (define/public (toy-x)
      (inexact->exact (round x)))
    
    ;---------------------------------------------------------------------------
    
    ;; toy-y : -> Int
    ;; RETURNS: the y position of the politician
    ;; STRATEGY: Combine simpler functions
    
    (define/public (toy-y)
      (inexact->exact (round y)))
    
    ;---------------------------------------------------------------------------
    
    ;; toy-data : -> Int
    ;; RETURNS: the distance of politician from the mouse pointer
    ;; STRATEGY: Combine simpler functions
    
    (define/public (toy-data)
      (inexact->exact (round (distance-calc x y saved-mx saved-my))))
    
    ;---------------------------------------------------------------------------
    
    ;; Test methods, to probe the politician state.
    
    ;; for-test:saved-mx : -> Int
    ;; RETURNS: the x position of the mouse after last move event
    ;; STRATEGY: Combine simpler functions
    
    (define/public (for-test:saved-mx) (round saved-mx))
    
    ;---------------------------------------------------------------------------
    
    ;; for-test:saved-my : -> Int
    ;; RETURNS: the y position of the mouse after last move event
    ;; STRATEGY: Combine simpler functions
    
    (define/public (for-test:saved-my) (inexact->exact (round saved-my)))
    
    ;---------------------------------------------------------------------------
    
    ;; for-test:HC? : -> Boolean
    ;; RETURNS: the true if the current face of politician is of Hillary Clinton
    ;; STRATEGY: Combine simpler functions
    
    (define/public (for-test:HC?) HC?)
    (define/public (for-test:test-slope) (updated-slope))
    
    )) ;; END OF Class

;; make-politician : PosInt PosInt -> Toy
;; GIVEN: an x and a y position
;; RETURNS: an object representing a politician at the given position.

(define (make-politician x y)
  (new Politician% [x x] [y y]))

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; TESTS: For Politician

;; For newly created politician

(begin-for-test
  (local ((define POLITICIAN (make-politician 250 300))
          (define POLITICIAN-DT (new Politician% [x 250]
                                     [y 300]
                                     [HC? false]
                                     [saved-mx 250])))
    (check-equal?
     (send POLITICIAN add-to-scene EMPTY-CANVAS)
     (place-image (bitmap "hillary-clinton.jpg")  250 300 EMPTY-CANVAS))
    (check-equal? (send (send POLITICIAN after-key-event "k") toy-x)
                  250
                  "toy-x should return 250")
    (send POLITICIAN after-button-down 450 500)
    (send POLITICIAN after-drag 500 550)
    (send POLITICIAN after-button-up 500 550)
    (send POLITICIAN after-move 550 600)
    (check-equal? (send POLITICIAN toy-x)
                  250
                  "Should return 250")
    (check-equal?
     (send POLITICIAN toy-y) 300
     "Should return 300")
    
    (check-equal?
     (send POLITICIAN toy-data) 424
     "Should return 424")
    
    (check-equal?
     (send POLITICIAN for-test:saved-mx) 550
     "Should return 250")
    
    (check-equal?
     (send POLITICIAN for-test:saved-my) 600
     "Should return 0")
    
    (check-equal?
     (send POLITICIAN for-test:HC?) true
     "Should return true")
    (send POLITICIAN after-tick)
    (send POLITICIAN after-move 250 250)
    (send POLITICIAN after-tick)
    
    (check-equal?
     (send POLITICIAN-DT add-to-scene EMPTY-CANVAS)
     (place-image (bitmap "donald-trump.jpg")  250 300 EMPTY-CANVAS))
    
    (check-equal? (send POLITICIAN-DT for-test:test-slope)
                  9999
                  "output should be 9999 for same mx and x")
    ))
;;-------------------------------------------------------------------------------
;
;; For politician after-tick

(begin-for-test
  
  (local
    ((define NEW-POLITICIAN (new Politician%)))
    (send NEW-POLITICIAN after-tick)
    (send NEW-POLITICIAN after-move 250 250)
    (send NEW-POLITICIAN after-move 170 480)
    (send NEW-POLITICIAN after-tick)
    (send NEW-POLITICIAN after-tick)
    (send NEW-POLITICIAN after-move 520 480)
    (send NEW-POLITICIAN after-tick)
    (send NEW-POLITICIAN after-tick)
    (send NEW-POLITICIAN after-move 250 480)
    
    (check-equal?
     (send NEW-POLITICIAN toy-x) 50
     "Should return 50")
    
    (check-equal?
     (send NEW-POLITICIAN toy-y) 480
     "Should return 480")
    
    (check-equal?
     (send NEW-POLITICIAN toy-data) 200
     "Should return 200")
    
    (check-equal?
     (send NEW-POLITICIAN for-test:saved-mx) 250
     "Should return 250")
    
    (check-equal?
     (send NEW-POLITICIAN for-test:saved-my) 480
     "Should return 480")
    
    (check-equal?
     (send NEW-POLITICIAN for-test:HC?) true
     "Should return true")
    
    (check-equal?
     (send NEW-POLITICIAN for-test:HC?) true
     "Should return true")
    
    (check-equal?
     (send NEW-POLITICIAN for-test:HC?) true
     "Should return true")))

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;