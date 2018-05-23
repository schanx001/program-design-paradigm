;; MetaToy program

#lang racket

(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)

(provide make-metatoy
         run
         make-throbber
         make-clock
         make-politician
         Metatoy<%>
         Toy<%>)

(check-location "09" "q1.rkt")

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

;; A Widget is an object whose class implements the Widget<%>
;; interface. 

;; A World is an object whose class implements the World<%>
;; interface.

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

;; big-bang will communicate with the world through the World<%>
;; interface. 

(define World<%>
  (interface ()
    
    ; -> World
    ; GIVEN: no arguments
    ; RETURNS: the state of the world at the next tick
    after-tick          
    
    ; Integer Integer MouseEvent-> World
    ; GIVEN: a location
    ; RETURNS: the state of the world that should follow the
    ; given mouse event at the given location.
    after-mouse-event
    
    ; KeyEvent : KeyEvent -> Widget
    ; GIVEN: a key event
    ; RETURNS: the state of the world that should follow the
    ; given key event
    after-key-event     
    
    ; -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene that depicts this World
    to-scene
    ))


;; Every object that lives in the world must implement the Widget<%>
;; interface.

(define Widget<%>
  (interface ()
    
    ; -> Widget
    ; GIVEN: no arguments
    ; RETURNS: the state of this object that should follow at time t+1.
    after-tick          
    
    ; Integer Integer -> Widget
    ; GIVEN: a location
    ; RETURNS: the state of this object that should follow the
    ; specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag
    
    ; KeyEvent : KeyEvent -> Widget
    ; GIVEN: a key event and a time
    ; RETURNS: the state of this object that should follow the
    ; given key event
    after-key-event     
    
    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run : PosNum -> Metatoy
;; GIVEN: a frame rate (in seconds/tick)
;; EFFECT: creates a MetaToy with no toys in it, and runs it using big-bang
;; at the given frame rate.  Returns the final state of the Metatoy.

(define (run rate)
  (big-bang (initial-world)
            (on-tick
             (lambda (w) (send w after-tick))
             rate)
            (on-draw
             (lambda (w) (send w to-scene)))
            (on-key
             (lambda (w kev)
               (send w after-key-event kev)))
            (on-mouse
             (lambda (w mx my mev)
               (send w after-mouse-event mx my mev)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Toy is an object of any class that implements Toy<%>
;; You will probably have three such classes, one for each kind of toy. 

(define Toy<%> 
  (interface
      
      ;; The interface Toy<%> inherits from the interface Widget<%>.
      ;; This means that any class that implements Toy<%> must implement
      ;; all the methods from Widget<%> plus all the methods defined here.
      (Widget<%>)
    
    
    ;; Note: the Widgets of the space-invader-examples don't respond
    ;; to mouse "move" events, but some of our toys do.  So we add an
    ;; after-move method to the interface.
    
    ;;  Int Int -> Toy
    ;;  RETURNS: the state of this toy that should follow a mouse-move
    ;;  at the given coordinates
    after-move
    
    
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
      
      ;; the (World<%>) says that Metatoy<%> inherits from World<%>
      ;; This means that any class that implements Metatoy<%> must
      ;; implement all the methods from World<%> plus all the methods
      ;; defined here. In this case, there is just one additional method,
      ;; called get-toys.
      (World<%>)
    
    ;; -> ListOfToy
    get-toys
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Metatoy% class

;; We will have only one class that implements Metatoy<%>: Metatoy%

;; Constructor template for Metatoy%:
;; (new Metatoy% [objs ListOfToy])
;; Interpretation: An object of class Metatoy% takes signals from
;; big-bang and distributes them to its objects as appropriate.

;; make-metatoy : ListOfToy -> Metatoy
;; RETURNS: a Metatoy with the given list of toys.
;; NOTE: The Metatoy<%> interface extends the World<%> interface, so the
;; result of make-metatoy is something that big-bang can use as a world.

(define (make-metatoy objs)
  (new Metatoy% [objs objs]))

(define Metatoy%
  (class* object% (Metatoy<%>)
    
    (init-field objs) ;  ListOfToy
    ;; initial x coordinate
    (field [x INITIAL-TOY-X])
    ;; initial y coordinate
    (field [y INITIAL-TOY-Y])
    
    (super-new)
    
    ;;-----------------------------------------------------------------------------
    ;; after-tick : -> Metatoy
    ;; Use HOFC map on the Toy's in this Metatoy
    
    (define/public (after-tick)
      (make-metatoy
       (map
        (lambda (obj) (send obj after-tick))
        objs)))
    
    ;;-----------------------------------------------------------------------------
    ;; to-scene : -> Scene
    ;; Use HOFC map on the Toy's in this Metatoy    

    (define/public (to-scene)
      (foldr
       (lambda (obj scene)
         (send obj add-to-scene scene))
       EMPTY-CANVAS
       objs))
    
    ;;-----------------------------------------------------------------------------
    ;; after-key-event : KeyEvent -> Metatoy
    ;; STRATEGY: Cases on kev
    ;; "p", "t" and "c" create new politician, new throbber and new clock;
    ;; other keystrokes are passed on to the objects in the Metatoy.
    
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-THROBBER-EVENT)
         (make-metatoy
          (cons (make-throbber x y) objs)
          )]
        [(key=? kev NEW-CLOCK-EVENT)
         (make-metatoy
          (cons (make-clock x y) objs)
          )]
        [(key=? kev NEW-POLITICIAN-EVENT)
         (make-metatoy
          (cons (make-politician x y) objs)
          )]
        [else
         (make-metatoy
          (map
           (lambda (obj) (send obj after-key-event kev))
           objs)
          )]))
    
    ;;------------------------------------------------------------------------------
    ;; world-after-mouse-event : Nat Nat MouseEvent -> Metatoy
    ;; STRATEGY: Cases on mev
    
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (world-after-button-down mx my)]
        [(mouse=? mev "drag")
         (world-after-drag mx my)]
        [(mouse=? mev "button-up")
         (world-after-button-up mx my)]
        [(mouse=? mev "move")
         (world-after-move mx my)]
        [else this]))
    
    ;; the next few functions are local functions, not in the interface.
    
    ;;--------------------------------------------------------------------------------
    
    ;; world-after-button-down: Int Int -> Metatoy
    ;; Returns: a Metatoy
    
    (define (world-after-button-down mx my)
      (make-metatoy
       (map
        (lambda (obj) (send obj after-button-down mx my))
        objs)
       ))
    
    ;;--------------------------------------------------------------------------------
    
    ;; world-after-button-up: Int Int -> Metatoy
    ;; Returns: a Metatoy
    
    (define (world-after-button-up mx my)
      (make-metatoy
       (map
        (lambda (obj) (send obj after-button-up mx my))
        objs)
       ))
    
    ;;--------------------------------------------------------------------------------
    
    ;; world-after-drag: Int Int -> Metatoy
    ;; Returns: a Metatoy
    
    (define (world-after-drag mx my)
      (make-metatoy
       (map
        (lambda (obj) (send obj after-drag mx my))
        objs)
       ))
    
    ;;--------------------------------------------------------------------------------
    
    ;; world-after-move: Int Int -> Metatoy
    ;; Returns: a Metatoy
    
    (define (world-after-move mx my)
      (make-metatoy
       (map
        (lambda (obj) (send obj after-move mx my))
        objs)
       ))
    
    ;;--------------------------------------------------------------------------------
    
    ;; -> ListOfToy
    
    (define/public (get-toys)
      objs)
    (define/public (for-test:t)
      0)
    
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TESTS for metatoy

(begin-for-test
  (local ((define METATOY-OBJ (new Throbber% [x 250]
                                   [y 300]
                                   [radius 5]
                                   [change 2]
                                   [selected? false]))
          (define METATOY (make-metatoy (list METATOY-OBJ))))
    (check-equal? (send METATOY get-toys)
                  (list METATOY-OBJ)
                  "the output should be list of metatoy objects")
    (check-equal? (send (send METATOY after-tick) for-test:t)
                  0
                  "output should be 0")
    
    (check-equal? (send (make-metatoy empty) to-scene)
                  EMPTY-CANVAS
                  "the output should be an empty Metatoy")
    
    (check-equal? (send METATOY to-scene)
                  (place-image SOLID-CIRCLE 250 300 EMPTY-CANVAS))
    (check-equal? (send (send (make-metatoy empty) after-key-event "t") for-test:t)
                  0
                  "output should be 0")
    (check-equal? (send (send (make-metatoy empty) after-key-event "c") for-test:t)
                  0
                  "output should be 0")
    (check-equal? (send (send (make-metatoy empty) after-key-event "p") for-test:t)
                  0
                  "output should be 0")
    (check-equal? (send (send METATOY after-key-event "x") for-test:t)
                  0
                  "output should be 0")
    (check-equal? (send (send METATOY after-mouse-event 255 0 "button-down") for-test:t)
                  0
                  "output should be 0")
    (check-equal? (send (send METATOY after-mouse-event 0 0 "button-up") for-test:t)
                  0
                  "output should be 0")
    (check-equal? (send (send METATOY after-mouse-event 0 0 "drag") for-test:t)
                  0
                  "output should be 0")
    (check-equal? (send (send METATOY after-mouse-event 0 0 "move") for-test:t)
                  0"output should be 0")
    (check-equal? (send (send METATOY after-mouse-event 0 0 "leave") for-test:t)
                  0
                  "output should be 0")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : -> Metatoy
;; RETURNS: an empty metatoy

(define (initial-world)
  (make-metatoy
   empty))

;; TESTS

(begin-for-test
  (check-equal? (send (initial-world) for-test:t)
                0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Throbber start at the center of the canvas.
;; They are selectable and draggable.

;; Constructor template for Throbber%:
;; (new Throbber% [x Integer][y Integer][radius NonNegInteger][change Integer]
;;            [selected? Boolean][mx Integer][my Integer])
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
    ;; after-tick : -> Toy
    ;; RETURNS: A throbber like this one, but as it should be after a tick
    ;; STRATEGY: 
    
    (define/public (after-tick)
      (new Throbber%
           [x x]
           [y y]
           [radius (get-radius)]
           [change (get-rate)]
           [selected? selected?]
           [saved-mx saved-mx]
           [saved-my saved-my]))
    
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
    ;; after-key-event : KeyEvent -> Toy
    ;; RETURNS: A toy like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: a throbber ignores key events
    
    (define/public (after-key-event kev)
      this)      
    
    ;;---------------------------------------------------------------------------
    ; after-button-down : Integer Integer -> Toy
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in the throbber
    
    (define/public (after-button-down mx my)
      (if (in-throbber? mx my)
          (new Throbber%
               [x x][y y]
               [radius radius]
               [change change]
               [selected? true]
               [saved-mx (- mx x)]
               [saved-my (- my y)])
          this))
    
    ;;----------------------------------------------------------------------------
    ; after-button-up : Integer Integer -> Toy
    ; GIVEN: the location of a button-up event
    ; If the throbber is selected, then unselect it.
    
    (define/public (after-button-up mx my)
      (new Throbber%
           [x x][y y]
           [radius radius]
           [change change]
           [selected? false]
           [saved-mx saved-mx]
           [saved-my saved-my]))   
    
    ;;----------------------------------------------------------------------------
    ; after-drag : Integer Integer -> Toy
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the throbber is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (new Throbber%
               [x (- mx saved-mx)]
               [y (- my saved-my)]
               [radius radius]
               [change change]
               [selected? true]
               [saved-mx saved-mx]
               [saved-my saved-my])
          this))   
    
    ;;----------------------------------------------------------------------------    
    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this thobber painted
    ;; on it.
    (define/public (add-to-scene scene)
      (if selected?
          (place-image THROBBER-IMG-OUTLINED x y scene)
          (place-image THROBBER-IMG-SOLID x y scene)))
    
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
          (define THROBBER1 (new Throbber%
                                 [x 250]
                                 [y 300]
                                 [radius 18]
                                 [change 3])))
    
    (check-equal? (send (send THROBBER after-tick) toy-x)
                  250
                  "the toy-x should be 250")
    
    (check-equal? (send (send THROBBER1 after-tick) toy-x)
                  250
                  "the toy-x should be 250")
    (check-equal? (send (send THROBBER1 after-tick) toy-x)
                  250
                  "the toy-x should be 250")
    (check-equal? (send (send THROBBER1 after-key-event "k") toy-y)
                  300
                  "the toy-y should be 300")
    (check-equal? (send (send THROBBER1 after-button-down 255 300) toy-x)
                  250
                  "the toy-x should be 250")
    (check-equal? (send (send THROBBER1 after-button-up 255 300) toy-x)
                  250
                  "the toy-x should be 250")
    (check-equal? (send (send THROBBER1 after-drag 255 300) toy-x)
                  250
                  "the toy-x should be 250")
    (check-equal? (send
                   (send
                    (send THROBBER1 after-button-down 255 300)
                    after-drag
                    255
                    300) toy-x)
                  250
                  "the toy-x should be 250")
    (check-equal? (send (send THROBBER after-button-down 255 300) add-to-scene EMPTY-CANVAS)
                  (place-image OUTLINE-CIRCLE 250 300 EMPTY-CANVAS)
                  "the output should be (place-image OUTLINE-CIRCLE 250 300 EMPTY-CANVAS)")
    (check-equal? (send THROBBER1 toy-data)
                  18
                  "the toy-data should be 18")))

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
    
    ;; after-tick : -> Toy
    ;; RETURNS: A clock like this one, but as it should be after a tick
    ;; a selected clock doesn't move.
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (new Clock%
           [x x]
           [y y]
           [time (+ time 1)]
           [selected? selected?]
           [saved-mx saved-mx]
           [saved-my saved-my]))
    
    ;; after-key-event : KeyEvent -> Toy
    ;; RETURNS: A clock like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: a clock ignores key events
    (define/public (after-key-event kev)
      this)      
    
    ; after-button-down : Integer Integer -> Toy
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in the clock
    (define/public (after-button-down mx my)
      (if (in-clock? mx my)
          (new Clock%
               [x x][y y]
               [time time]
               [selected? true]
               [saved-mx (- mx x)]
               [saved-my (- my y)])
          this))
    
    ; after-button-up : Integer Integer -> Toy
    ; GIVEN: the location of a button-up event
    ; STRATEGY: Cases on whether the event is in the clock.
    ; If the clock is selected, then unselect it.
    (define/public (after-button-up mx my)
      (new Clock%
           [x x][y y]
           [time time]
           [selected? false]
           [saved-mx saved-mx]
           [saved-my saved-my]))   
    
    ; after-drag : Integer Integer -> Toy
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the clock is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
          (new Clock%
               [x (- mx saved-mx)]
               [y (- my saved-my)]
               [time time]
               [selected? true]
               [saved-mx saved-mx]
               [saved-my saved-my])
          this))   
    
    
    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this clock painted
    ;; on it.
    (define/public (add-to-scene scene)
      (place-image  (overlay (text (format font-type time)
                                   font-size
                                   font-color)
                             CLOCK-IMG-OUTLINED) x y scene))
    
    ;; in-clock? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this clock.
    (define (in-clock? other-x other-y)
      (and (<= (- x (/ width 2)) other-x (+ x (/ width 2)))
           (<= (- y (/ height 2)) other-y (+ y (/ height 2)))))
    
    (define/public (toy-x)
      x)
    (define/public (toy-y)
      y)
    (define/public (toy-data)
      time)
    (define/public (after-move mx my)
      this)
    ;; test methods, to probe the clock state.  Note that we don't have
    ;; a probe for radius.
    
    ;; -> Int
    (define/public (for-test:saved-mx) saved-mx)
    
    ;; -> Int
    (define/public (for-test:saved-my) saved-my)
    
    ;; -> Boolean
    (define/public (for-test:selected?) selected?)))

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
  
  (local
    ((define NEW-CLOCK (make-clock 250 300))
     (define CLOCK-SELECTED (send NEW-CLOCK after-button-down 260 310))
     (define CLOCK-DRAG (send CLOCK-SELECTED after-drag 270 320))
     (define CLOCK-UNSELECTED (send CLOCK-DRAG after-button-up 280 330))
     (define CLOCK-AFTER-MOVE (send CLOCK-UNSELECTED after-move 290 340))
     (define CLOCK-KEY-EVENT (send CLOCK-AFTER-MOVE after-key-event "k"))
     (define CLOCK-BUTTON-DOWN-OUTSIDE (send CLOCK-KEY-EVENT after-button-down 500 500))
     (define CLOCK-DRAG-OUTSIDE (send CLOCK-BUTTON-DOWN-OUTSIDE after-drag 400 400))
     (define CLOCK-AFTER-TICK (send CLOCK-DRAG-OUTSIDE after-tick)))
    
    (check-equal?
     (send CLOCK-AFTER-TICK toy-x) 260
     "Should return 260")
    
    (check-equal?
     (send CLOCK-AFTER-TICK toy-y) 310
     "Should return 310")
    
    (check-equal?
     (send CLOCK-AFTER-TICK for-test:saved-mx) 10
     "Should return 10")
    
    (check-equal?
     (send CLOCK-AFTER-TICK for-test:saved-my) 10
     "Should return 10")
    
    (check-equal?
     (send CLOCK-AFTER-TICK toy-data) 1
     "Should return 1")
    
    (check-equal?
     (send CLOCK-AFTER-TICK for-test:selected?) false
     "Should return false")
    
    (check-equal?
     (send CLOCK-AFTER-TICK add-to-scene EMPTY-CANVAS)
     (place-image (overlay (text (format "~a" 1) 12 "red")
                           (rectangle 40 20 "outline" "navy"))
                  260 310 EMPTY-CANVAS) "should return an image with clock")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; POLITICIAN CLASS

(define Politician%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from one politician to
    ;; the next.
    
    ;; Intial x and y positions of the politician in the canvas
    (define POLITICIAN-INITIAL-X (/ CANVAS-WIDTH 2))
    (define POLITICIAN-INITIAL-Y (/ CANVAS-HEIGHT 2))
    
    ;; the x and y position of the center of the politician
    (init-field [x POLITICIAN-INITIAL-X] [y POLITICIAN-INITIAL-Y])
    
    ;; HC? is a init-field containing value true iff curent face of politician is of
    ;; Hillary Clinton
    (init-field [HC? true])
    
    ;; saved-mx and saved-my are the mouse positions of the last move of the mouse
    (init-field [saved-mx POLITICIAN-INITIAL-X] [saved-my 0])
    
    
    ;; images for displaying faces of the politician
    (field [HC-IMG (bitmap "hillary-clinton.jpg")])
    (field [DT-IMG (bitmap "donald-trump.jpg")])
    
    ;; The velocity by which the politician moves towards mouse pointer
    (field [POLITICIAN-VELOCITY 20])
    
    ;; The distance by which the politician jums away from the mouse pointer
    (field [POLITICIAN-JUMP 200])
    
    ;; The radius within which the politician gets scared after entering 
    (field [SCARE-RADIUS 75])
    
    (super-new)
    
    ;---------------------------------------------------------------------------
    
    ; -> Toy
    ; GIVEN: no arguments
    ; RETURNS: the state of this object that should follow at time t+1.
    ;; DETAILS: here the object returned is of class Politician
    ;; STRATEGY: Divide into cases on near-mouse?
    
    (define/public (after-tick)
      (local
        ((define new-x (updated-x (updated-velocity)))
         (define new-y (updated-y new-x)))
        (if (near-mouse? x y saved-mx saved-my)
            (new Politician%
                 [HC? (if HC? false true)]
                 [x (jump-x)]
                 [y (jump-y)]
                 [saved-mx saved-mx]
                 [saved-my saved-my])
            (new Politician%
                 [HC? HC?]
                 [x new-x]
                 [y new-y]
                 [saved-mx saved-mx]
                 [saved-my saved-my]))))
    
    ;---------------------------------------------------------------------------
    
    ;; near-mouse? : Real Real Real Real -> Boolean
    ;; GIVEN: coordinates 1 x1, y1 and coordinates 2 x2, y2
    ;; RETURNS: true iff distance between them is less than or equal to set SCARE-RADIUS
    ;; STRATEGY: COmbine simpler functions
    
    (define (near-mouse? x1 y1 x2 y2)
      (<= (distance-calc x1 y1 x2 y2) SCARE-RADIUS))
    
    ;---------------------------------------------------------------------------
    
    ;; updated-x : Int -> Real
    ;; GIVEN: a value for updating value of x co-ordinate
    ;; RETURNS: the updated value of x
    ;; STRATEGY: Combine simpler functions
    
    (define (updated-x vel)
      (+ (/ vel (sqrt (+ (sqr (updated-slope)) 1))) x))
    
    ;---------------------------------------------------------------------------
    
    ;; updated-y : Int -> Real
    ;; GIVEN: a value for updating value of y co-ordinate
    ;; RETURNS: the updated value of y
    ;; STRATEGY: Combine simpler functions
    
    (define (updated-y x2)
      (+ (* (updated-slope) (- x2 x)) y))
    
    ;---------------------------------------------------------------------------
    
    ;; jump-x: -> Real
    ;; RETURNS: new coordinate of x after being repelled away from mouse pointer
    ;; STRATEGY: Cases on value of (updated-velocity)
    
    (define (jump-x)
      (if (< (updated-velocity) 0)
          (updated-x POLITICIAN-JUMP)
          (updated-x (* -1 POLITICIAN-JUMP))))
    
    ;---------------------------------------------------------------------------
    
    ;; jump-y: -> Real
    ;; RETURNS: new coordinate of y after being repelled away from mouse pointer
    ;; STRATEGY: Combine simpler functions
    
    (define (jump-y)
      (+ (* (updated-slope) (- (jump-x) x)) y))
    
    ;---------------------------------------------------------------------------
    
    ;; distance-calc : Real Real Real Real -> NonNegReal
    ;; GIVEN: two coordinates (x1,y1), (x2,y2)
    ;; RETURNS: the distance between the two coordinates
    ;; STRATEGY: Combine simpler functions
    
    (define (distance-calc nx1 ny1 nx2 ny2 )
      (sqrt (+ (sqr (- nx1 nx2))
               (sqr (- ny1 ny2)))))
    
    ;---------------------------------------------------------------------------
    
    ;; after-key-event : KeyEvent -> Toy
    ;; RETURNS: A toy like this one, but as it should be after the
    ;;          given key event.
    ;; DETAILS: a politician ignores key events
    
    (define/public (after-key-event kev)
      this)
    
    ;---------------------------------------------------------------------------
    
    ;; after-button-down : Integer Integer -> Toy
    ;; GIVEN: the location of a button-down event
    ;; RETURNS: A toy like this one, but as it should be after the
    ;;          button down event 
    ;; STRATEGY: Combine simpler functions
    
    (define/public (after-button-down mx my)
      (after-move mx my))
    
    ;---------------------------------------------------------------------------
    
    ;; after-button-up : Integer Integer -> Toy
    ;; GIVEN: the location of a button-up event
    ;; RETURNS: A toy like this one, but as it should be after the
    ;;          button up event 
    ;; STRATEGY: Combine simpler functions
    
    (define/public (after-button-up mx my)
      this)
    
    ;---------------------------------------------------------------------------
    
    ;; after-drag : Integer Integer -> Toy
    ;; GIVEN: the location of a drag event
    ;; RETURNS: A toy like this one, but as it should be after the
    ;;          drag event 
    ;; STRATEGY: Combine simpler functions
    
    (define/public (after-drag mx my)
      (after-move mx my))
    
    ;---------------------------------------------------------------------------
    
    ;; after-move : Integer Integer -> Toy
    ;; GIVEN: the location of a drag event
    ;; RETURNS: A toy like this one, but as it should be after the
    ;;          mouse move event 
    ;; STRATEGY: Cases on near-mouse? iff true jump away from mouse position
    ;;           else come near
    
    (define/public (after-move mx my)
      (if (near-mouse? x y mx my)
          (new Politician%
               [HC? (if HC? false true)]
               [x (jump-x)]
               [y (jump-y)]
               [saved-mx mx]
               [saved-my my])
          (new Politician%
               [HC? HC?]
               [x x]
               [y y]
               [saved-mx mx]
               [saved-my my])))
    
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
    
    ;; to-scene : Scene -> Scene
    ;; GIVEN: A scene
    ;; RETURNS: a scene like the given one, but with this politician painted
    ;; on it.
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
    
    ;; Test methods, to probe the politicia state.
    
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
  
  (local
    
    ((define NEW-POLITICIAN (make-politician 250 300)))
    
    (check-equal?
     (send NEW-POLITICIAN toy-x) 250
     "Should return 250")
    
    (check-equal?
     (send NEW-POLITICIAN toy-y) 300
     "Should return 300")
    
    (check-equal?
     (send NEW-POLITICIAN toy-data) 300
     "Should return 300")
    
    (check-equal?
     (send NEW-POLITICIAN add-to-scene EMPTY-CANVAS)
     (place-image (bitmap "hillary-clinton.jpg")  250 300 EMPTY-CANVAS))
    
    (check-equal?
     (send NEW-POLITICIAN for-test:saved-mx) 250
     "Should return 250")
    
    (check-equal?
     (send NEW-POLITICIAN for-test:saved-my) 0
     "Should return 0")
    
    (check-equal?
     (send NEW-POLITICIAN for-test:HC?) true
     "Should return true")))

;-------------------------------------------------------------------------------

;; For a polictician that is moved after button-down, button-up, drag event
;; and move event without after-tick

(begin-for-test
  
  (local
    
    ((define NEW-POLITICIAN (make-politician 250 300))
     (define P-AFTER-KEY-EVENT (send NEW-POLITICIAN after-key-event "k"))
     (define P-AFTER-BUTTON-DOWN (send P-AFTER-KEY-EVENT after-button-down 450 500))
     (define P-AFTER-DRAG (send P-AFTER-BUTTON-DOWN after-drag 500 550))
     (define P-AFTER-BUTTON-UP (send P-AFTER-DRAG after-button-up 500 550))
     (define P-AFTER-MOVE (send P-AFTER-BUTTON-UP after-move 550 600)))
    
    (check-equal?
     (send P-AFTER-MOVE toy-x) 250
     "Should return 250")
    
    (check-equal?
     (send P-AFTER-MOVE toy-y) 300
     "Should return 300")
    
    (check-equal?
     (send P-AFTER-MOVE toy-data) 424
     "Should return 424")
    
    (check-equal?
     (send P-AFTER-MOVE for-test:saved-mx) 550
     "Should return 250")
    
    (check-equal?
     (send P-AFTER-MOVE for-test:saved-my) 600
     "Should return 0")
    
    (check-equal?
     (send P-AFTER-MOVE for-test:HC?) true
     "Should return true")))

;-------------------------------------------------------------------------------

;; For politician after-tick

(begin-for-test
  
  (local
    ((define NEW-POLITICIAN (new Politician%))
     (define POLITICIAN-AFTER-TICK (send NEW-POLITICIAN after-tick))
     (define P-AFTER-MOUSE-MOVE-NEAR-TOP (send POLITICIAN-AFTER-TICK after-move 250 250))
     (define P-AFTER-TICK-NEAR (send (send (send P-AFTER-MOUSE-MOVE-NEAR-TOP
                                                 after-move 170 480)
                                           after-tick) after-tick))
     (define P-AFTER-TICK-NEAR-AGAIN (send (send (send P-AFTER-TICK-NEAR
                                                       after-move 520 480)
                                                 after-tick) after-tick))
     (define P-AFTER-MOUSE-MOVE-NEAR-LEFT (send P-AFTER-TICK-NEAR-AGAIN after-move 250 480)))
    
    (check-equal?
     (send P-AFTER-MOUSE-MOVE-NEAR-TOP toy-x) 250
     "Should return 250")
    
    (check-equal?
     (send P-AFTER-MOUSE-MOVE-NEAR-TOP toy-y) 480
     "Should return 480")
    
    (check-equal?
     (send P-AFTER-MOUSE-MOVE-NEAR-TOP toy-data) 230
     "Should return 230")
    
    (check-equal?
     (send P-AFTER-MOUSE-MOVE-NEAR-TOP for-test:saved-mx) 250
     "Should return 250")
    
    (check-equal?
     (send P-AFTER-MOUSE-MOVE-NEAR-TOP for-test:saved-my) 250
     "Should return 250")
    
    (check-equal?
     (send P-AFTER-MOUSE-MOVE-NEAR-TOP for-test:HC?) false
     "Should return false")
    
    (check-equal?
     (send P-AFTER-MOUSE-MOVE-NEAR-TOP add-to-scene EMPTY-CANVAS)
     (place-image (bitmap "donald-trump.jpg")  250 480 EMPTY-CANVAS))
    
    (check-equal?
     (send P-AFTER-TICK-NEAR for-test:HC?) true
     "Should return true")
    
    (check-equal?
     (send P-AFTER-TICK-NEAR-AGAIN for-test:HC?) false
     "Should return false")
    
    (check-equal?
     (send P-AFTER-MOUSE-MOVE-NEAR-LEFT for-test:HC?) true
     "Should return true")))

;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;