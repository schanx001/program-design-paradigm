;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname screensaver-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A program to build a screensaver
;; with two circles moving around the canvas.
;; The user can pause/unpause the simulation with space bar.
;; Simulation is paused initially.
;; User can select and drag the circles whether or not
;; the simulation is paused.
;; Running the simulation: give speed as an argument
;; Example: (screensaver 0.5)

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(check-location "03" "screensaver-2.rkt")

(provide screensaver
         initial-world
         world-after-tick
         world-after-key-event
         world-after-mouse-event
         circ-after-mouse-event
         circ-selected?
         world-circ1
         world-circ2
         world-paused?
         new-circle
         circ-x
         circ-y
         circ-vx
         circ-vy)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; dimensions of canvas

(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; radius of the circles

(define RADIUS 40)

;; DEFAULT VALUES for mouse pointer coordinates and for mouse event
(define MOUSE-X 0)
(define MOUSE-Y 0)
(define MEV "button-up")

;; Points

(define MIN-CENTER-X (+ 0 RADIUS))
(define MAX-CENTER-X (- CANVAS-WIDTH RADIUS))
(define MIN-CENTER-Y (+ 0 RADIUS))
(define MAX-CENTER-Y (- CANVAS-HEIGHT RADIUS))

(define RED-CIRCLE (circle 5 "solid" "red"))
(define EMPTY-CIRCLE (circle 0 "solid" "white"))

(define RED-OUTLINE-CIRCLE (circle RADIUS "outline" "red"))
(define BLUE-OUTLINE-CIRCLE (circle RADIUS "outline" "blue"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS:

(define-struct circ(x y vx vy selected? mx my))

;; A Circle is a (make-circ NonNegInteger NonNegInteger Integer Integer Boolean Integer Integer)
;; Interpretation:
;; x is the x co-ordinate of the circle's center
;; y is the y co-ordinate of the circle's center
;; vx is the velocity of the circle in x axis direction 
;; vy is the velocity of the circle in y axis direction
;; selected? describes whether or not the circle is selected
;; mx is the x co-ordinate of a mouse event
;; my is the y co-ordinate of a mouse event

;; template:
;; circ-fn: Circle -> ??
;; (define (circ-fn c)
;;   (...(circ-x c)
;;       (circ-y c)
;;       (circ-vx c)
;;       (circ-vy c)
;;       (circ-selected? c)
;;       (circ-mx c)
;;       (circ-my c)))

;; 2 Circles for this program

(define circle1 (make-circ 200 100 -12 20 false 0 0))
(define circle2 (make-circ 200 200 23 -14 false 0 0))

;; Samples of circles to be used for tests
(define circle1t (make-circ 188 120 -12 20 false 0 0))
(define circle2t (make-circ 223 186 23 -14 false 0 0))

(define circle1-selected-initial (make-circ 200 100 -12 20 true 0 0))
(define circle2-selected-initial (make-circ 200 200 23 -14 true 0 0))

(define circle1-unselected-initial (make-circ 200 100 -12 20 false 0 0))
(define circle2-unselected-initial (make-circ 200 200 23 -14 false 0 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct world(circ1 circ2 paused? mevent mx my))

;; A WorldState is a (make-worldstate Circle Circle Boolean MouseEvent Integer Integer)
;; Interpretation:
;; circle1 is one of the circles of the scene.
;; circle2 is the other circle of the scene.
;; paused? describes whether or not the scene is paused.
;; mevent  is a mouse event
;; mx      is the x co-ordinate of the mouse pointer location 
;; my      is the y co-ordinate of the mouse pointer location

;; template:
;; worldstate-fn: WorldState -> ??
;; (define (world-fn w)
;;     (...(world-circ1 w)
;;         (world-circ2 w)
;;         (world-paused? w)
;;         (world-mevent w)
;;         (world-mx w)
;;         (world-my w)))

;; Examples and sample for tests of the world

(define unpaused-world-initial-state (make-world circle1 circle2 false MEV MOUSE-X MOUSE-Y))
(define unpaused-world-after-1-tick (make-world circle1t circle2t false MEV MOUSE-X MOUSE-Y))

(define paused-world-initial-state (make-world circle1 circle2 true MEV MOUSE-X MOUSE-Y))

(define paused-world-initial-state-selected
  (make-world circle1-selected-initial
              circle2 true MEV MOUSE-X MOUSE-Y))



(define paused-world-after-1-tick (make-world circle1 circle2 true MEV MOUSE-X MOUSE-Y))

;; A MouseEvent is one of
;; --"button-up"
;; --"drag"
;; --"button-down"
;; --"move"

;; mev-fn: MouseEvent -> ??
;; (define (mev-fn mevent)
;;  (cond
;;     [(string=? mevent "button-up")...]
;;     [(string=? mevent "button-down")...]
;;     [(string=? mevent "drag")...]
;;     [(string=? mevent "move")...]))

;; help function for key event
;; is-pause-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pause instruction
(define (is-pause-key-event? k)
  (key=? k " "))

;; examples for testing
(define pause-key-event " ")
(define non-pause-key-event "q") 

;; dimension of the circles

(define HALF-CIRCLE-WIDTH  (/ (image-width  BLUE-OUTLINE-CIRCLE) 2)) 
(define HALF-CIRCLE-HEIGHT (/ (image-height BLUE-OUTLINE-CIRCLE) 2))

(define-struct mousestate (mevent mx my))

;;; END DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; screensaver : PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state as
;; specified in the problem set.
;; RETURNS: the final state of the WorldState

(define (screensaver speed)
  (big-bang (initial-world "sa")
            (on-tick world-after-tick speed)
            (on-key world-after-key-event)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; EXAMPLES:
;; (initial-world 1) = (make-world
;;                        circle1 circle2 true MSTATE)
;; (initial-world "str") = (make-world
;;                           circle1 circle2 true MSTATE)
;; (initial-world true) = (make-world
;;                           circle1 circle2 true MSTATE)
;; DESIGN STRATEGY: using template for WorldState

(define (initial-world any)
  (make-world circle1 circle2 true MEV MOUSE-X MOUSE-Y))

;; TESTS 
(begin-for-test
  (check-equal? (initial-world 1)
                (make-world circle1
                            circle2
                            true
                            MEV
                            MOUSE-X
                            MOUSE-Y) "The initial world state wasn't returned")
  (check-equal? (initial-world "str")
                (make-world circle1
                            circle2
                            true
                            MEV
                            MOUSE-X
                            MOUSE-Y) "The initial world state wasn't returned")
  (check-equal? (initial-world true)
                (make-world circle1
                            circle2
                            true
                            MEV
                            MOUSE-X
                            MOUSE-Y) "The initial world state wasn't returned"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene: WorldState -> Scene
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE:
;; (world-to-scene paused-world-initial-state) should return two circles with their centers at
;; (200,100) and (200,200) and a mouse pointer (red circle) on a mouse event respectively.
;; DESIGN STRATEGY: using template for WorldState on w

(define (world-to-scene w)
   (place-circ-pointer w
                       (world-circ1 w)
                       (place-circ-pointer w
                                           (world-circ2 w)
                                           EMPTY-CANVAS)))

;; place-circ-pointer: WorldState Circle Scene -> Scene
;; RETURN: a scene like the given one, but with the given circles
;;         and their respective velocities painted on it along
;;         with the mouse pointer indicated by a red circle on
;;         specific mouse events.
;; DESIGN STRATEGY: Combine simpler functions

(define (place-circ-pointer w c s)
  (place-circ c
              (place-mouse-pointer w s)))

;; place-mouse-pointer: WorldState Scene -> Scene
;; RETURN: a scene like the given one, but with the mouse location
;;         indicated by a red circle on mouse events like button-down
;;         drag. 
;;DESIGN STRATEGY: Using template for WorldState on w

(define (place-mouse-pointer w s)
  (place-image
   (if (or (mouse=? "move" (world-mevent w))
           (equal? 0 (world-mx w)))
       EMPTY-CIRCLE
       RED-CIRCLE)
  (world-mx w)
  (world-my w)
   s))

;; place-circ: Circle Scene -> Scene
;; RETURN: a scene like the given one, but with the given circles
;;         drawn and their respective velocities painted on it.
;; DESIGN STRATEGY: Using template for Circle on c

(define (place-circ c s)
  (place-image (overlay (text (string-append
                               "("
                               (number->string (circ-vx c))
                               ","
                               (number->string(circ-vy c))
                               ")")
                              14
                              "blue")
                        (if(circ-selected? c)
                           RED-OUTLINE-CIRCLE
                           BLUE-OUTLINE-CIRCLE))
               (circ-x c)
               (circ-y c)
               s))

;; image output for testing
(define image-circle
  (place-image (overlay (text"(-12,20)" 14 "blue") RED-OUTLINE-CIRCLE)
                                  200
                                  100
                                  EMPTY-CANVAS))

(begin-for-test
  (check-equal? (place-circ circle1-selected-initial EMPTY-CANVAS) image-circle))

;;TESTS for world-to-scene
;; Sample output for testing purpose
(define circles-of-paused-world-initial-state
  (place-circ-pointer paused-world-initial-state
                      (world-circ1 paused-world-initial-state)
                      (place-circ-pointer paused-world-initial-state
                                          (world-circ2 paused-world-initial-state)
                                          EMPTY-CANVAS)))
(begin-for-test
  (check-equal?
    (world-to-scene paused-world-initial-state)
    circles-of-paused-world-initial-state
    "(world-to-scene paused-world-initial-state) returned incorrect image"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-tick : WorldState -> WorldState
;; RETURNS: the world state that should follow the given world state
;; after a tick.
;; EXAMPLES:
;; (world-after-tick unpaused-world-initial-state) = unpaused-world-after-1-tick
;; (world-after-tick paused-world-initial-state) = paused-world-after-1-tick
;; DESIGN STRATEGY: Using template for WorldState on w

(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world
       (circle-after-tick (world-circ1 w))
       (circle-after-tick (world-circ2 w))
       (world-paused? w)
       (world-mevent w)
       (world-mx w)
       (world-my w))))

;; TESTS for world-after-tick
(begin-for-test
  (check-equal? (world-after-tick unpaused-world-initial-state)
                unpaused-world-after-1-tick
                "the world state returned isn't the one expected")
  (check-equal? (world-after-tick paused-world-initial-state)
                paused-world-after-1-tick
                "the world state returned isn't the one expected"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; circle-after-tick: Circle -> Circle
;; GIVEN: the state of circle c
;; RETURNS: the state of the given circle after a tick if
;;          it were in an unpaused world.
;; EXAMPLES: (circle-after-tick circle1-selected-initial)
;;              = (circle1-selected-initial)                                    
;; DESIGN STRATEGY: Using template for Circle on c

(define (circle-after-tick c)
  (if (circ-selected? c)
      c
      (circle-new-pos c)))

;; TESTS
(begin-for-test
  (check-equal? (circle-after-tick circle1-selected-initial)
                circle1-selected-initial
                "the circle should have stopped moving but it didn't"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; left-wall-adjustment    : Circle -> Circle
;; right-wall-adjustment   : Circle -> Circle
;; top-wall-adjustment     : Circle -> Circle
;; bottom-wall-adjustment  : Circle -> Circle
;; top-right-adjustment    : Circle -> Circle
;; bottom-right-adjustment : Circle -> Circle
;; bottom-left-adjustment  : Circle -> Circle
;; top-left-adjustment     : Circle -> Circle
;; GIVEN: a Circle c        
;; RETURNS: a Circle like before, but with the evaluated co-ordinates of
;;          its center (x,y) and with velcity values determined. 
;; EXAMPLES: refer the tests below
;; DESIGN STRATEGY: Using Template for Circle on c

(define (left-wall-adjustment c)
  (make-circ MIN-CENTER-X
             (+ (circ-y c) (circ-vy c))
             (* (circ-vx c) -1)
             (circ-vy c)
             (circ-selected? c)
             MOUSE-X
             MOUSE-Y))

(define (right-wall-adjustment c)
  (make-circ MAX-CENTER-X
             (+ (circ-y c) (circ-vy c))
             (* (circ-vx c) -1)
             (circ-vy c)
             (circ-selected? c)
             MOUSE-X
             MOUSE-Y))

(define (top-wall-adjustment c)
  (make-circ (+ (circ-x c) (circ-vx c))
             MIN-CENTER-Y
             (circ-vx c)
             (* (circ-vy c) -1)
             (circ-selected? c)
             MOUSE-X
             MOUSE-Y))
  
(define (bottom-wall-adjustment c)
  (make-circ (+ (circ-x c) (circ-vx c))
             MAX-CENTER-Y
             (circ-vx c)
             (* (circ-vy c) -1)
             (circ-selected? c)
             MOUSE-X
             MOUSE-Y))

(define (top-right-adjustment c)
  (make-circ MAX-CENTER-X
             MIN-CENTER-Y
             (* (circ-vx c) -1)
             (* (circ-vy c) -1)
             (circ-selected? c)
             MOUSE-X
             MOUSE-Y))


(define (bottom-right-adjustment c)
  (make-circ MAX-CENTER-X
             MAX-CENTER-Y
             (* (circ-vx c) -1)
             (* (circ-vy c) -1)
             (circ-selected? c)
             MOUSE-X
             MOUSE-Y))

(define (bottom-left-adjustment c)
  (make-circ MIN-CENTER-X
             MAX-CENTER-Y
             (* (circ-vx c) -1)
             (* (circ-vy c) -1)
             (circ-selected? c)
             MOUSE-X
             MOUSE-Y))

(define (top-left-adjustment c)
  (make-circ MIN-CENTER-X
             MIN-CENTER-Y
             (* (circ-vx c) -1)
             (* (circ-vy c) -1)
             (circ-selected? c)
             MOUSE-X
             MOUSE-Y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; circle-new-pos: Circle -> Circle
;; GIVEN: a Circle c
;; RETURNS: a Circle that should follow c after its evaluation
;;          considering the velocity parameters vx and vy
;; EXAMPLES:

;; circle1 is (200 100 -12 20) circle2 is (200 200 23 -14)
;; circle1t is (188 120 -12 20) circle2t is (223 186 23 -14)

;; (circle-new-pos circle1) = circle1t
;; (circle-new-pos circle2) = circle2t
;; DESIGN STRATEGY: Using template for Circle on c

(define (circle-new-pos c)
  (cond
    [(and (<= (+ (circ-y c) (circ-vy c)) MIN-CENTER-Y)
          (>= (+ (circ-x c) (circ-vx c)) MAX-CENTER-X))
     (top-right-adjustment c)]
    
    [(and (>= (+ (circ-y c) (circ-vy c)) MAX-CENTER-Y)
          (>= (+ (circ-x c) (circ-vx c)) MAX-CENTER-X))
     (bottom-right-adjustment c)]

    [(and (>= (+ (circ-y c) (circ-vy c)) MAX-CENTER-Y)
          (<= (+ (circ-x c) (circ-vx c)) MIN-CENTER-X))
     (bottom-left-adjustment c)]

    [(and (<= (+ (circ-y c) (circ-vy c)) MIN-CENTER-Y)
          (<= (+ (circ-x c) (circ-vx c)) MIN-CENTER-X))
     (top-left-adjustment c)]
    
    [(<= (+ (circ-x c) (circ-vx c)) MIN-CENTER-X)
     (left-wall-adjustment c)]

    [(>= (+ (circ-x c) (circ-vx c)) MAX-CENTER-X)
     (right-wall-adjustment c)]

    [(<= (+ (circ-y c) (circ-vy c)) MIN-CENTER-Y)
     (top-wall-adjustment c)]

    [(>= (+ (circ-y c) (circ-vy c)) MAX-CENTER-Y)
     (bottom-wall-adjustment c)]
    
    [else (make-circ (+ (circ-x c) (circ-vx c))
                     (+ (circ-y c) (circ-vy c))
                     (circ-vx c)
                     (circ-vy c)
                     (circ-selected? c) 0 0)]))

;; Tests for circle
(begin-for-test
  (check-equal? (circle-new-pos circle1) ;; normal flow for else case
                circle1t
                "the circle returned, isn't the one that should follow circle1")
  (check-equal? (circle-new-pos circle2) ;; normal flow for else case
                circle2t
                "the circle returned, isn't the one that should follow circle2")
  (check-equal? (circle-new-pos (make-circ 45 80 -12 20 false 0 0)) ;; for left boundary
                (make-circ 40 100 12 20 false 0 0) 
                "the circle returned, isn't the one that should follow the input circle")
  (check-equal? (circle-new-pos (make-circ 355 200 12 20 false 0 0)) ;; for right boundary
                (make-circ 360 220 -12 20 false 0 0)
                "the circle returned, isn't the one that should follow the input circle")
  (check-equal? (circle-new-pos (make-circ 45 42 12 -20 false 0 0)) ;; for bottom boundary
                (make-circ 57 40 12 20 false 0 0)
                "the circle returned, isn't the one that should follow the input circle")
  (check-equal? (circle-new-pos (make-circ 57 255 12 20 false 0 0)) ;; for top boundary
                (make-circ 69 260 12 -20 false 0 0)
                "the circle returned, isn't the one that should follow the input circle")
  (check-equal? (circle-new-pos (make-circ 357 46 12 -20 false 0 0)) ;; for top-right corner
                (make-circ 360 40 -12 20 false 0 0)
                "the circle returned, isn't the one that should follow the input circle")
  (check-equal? (circle-new-pos (make-circ 357 250 12 20 false 0 0)) ;; for bottom-right corner
                (make-circ 360 260 -12 -20 false 0 0)
                "the circle returned, isn't the one that should follow the input circle")
  (check-equal? (circle-new-pos (make-circ 42 257 -12 20 false 0 0)) ;; for bottom-left corner 
                (make-circ 40 260 12 -20 false 0 0)
                "the circle returned, isn't the one that should follow the input circle")
  (check-equal? (circle-new-pos (make-circ 45 43 -12 -20 false 0 0)) ;; for top-left corner
                (make-circ 40 40 12 20 false 0 0)
                "the circle returned, isn't the one that should follow the input circle"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-circle : NonNegInt NonNegInt Int Int -> Circle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a circle centered at (x,y), which will travel with
;; velocity (vx, vy) and unselected.
;; EXAMPLES: (new-circle 200 100 -12 20) = (make-circ 200 100 -12 20 false 0 0)
;; DESIGN STRATEGY: Using template for Circle

(define (new-circle x y vx vy)
  (make-circ x y vx vy false MOUSE-X MOUSE-Y))

;; TESTs for new-circle
(begin-for-test
  (check-equal? (new-circle 200 100 -12 20)
                (make-circ 200 100 -12 20 false 0 0)
                "the expected output of a circle wasn't achieved"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; RETURNS: the WorldState that should follow the given worldstate
;; after the given keyevent
;; EXAMPLES:
;; refer tests
;; DESIGN STRATEGY: Using template for WorldState on w

(define (world-after-key-event w kev)
  (if (is-pause-key-event? kev)
      (world-with-paused-toggled w)
      w))

;; TESTS
(begin-for-test
  (check-equal? (world-after-key-event paused-world-initial-state pause-key-event)
                unpaused-world-initial-state
                "after pause key, paused world should become unpaused")
  (check-equal? (world-after-key-event unpaused-world-initial-state pause-key-event)
                paused-world-initial-state
                "after pause key, unpaused world should become paused")
  (check-equal? (world-after-key-event paused-world-initial-state non-pause-key-event)
                paused-world-initial-state
                "after a non-pause key, paused world should be unchanged")
  (check-equal? (world-after-key-event unpaused-world-initial-state non-pause-key-event)
                unpaused-world-initial-state
                "after a non-pause key, unpaused world should be unchanged"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-with-paused-toggled : WorldState -> WorldState
;; RETURNS: a world just like the given one, but with paused? toggled
;; STRATEGY: Use template for World on w
;; EXAMPLES:
;; (world-with-paused-toggled
;;   (make-world circle1 circle2 true MEV MOUSE-X MOUSE-Y))
;;                        = (make-world circle1 circle2 false MEV MOUSE-X MOUSE-Y)
;; (world-with-paused-toggled
;;   (make-world circle1 circle2 false MEV MOUSE-X MOUSE-Y))
;;                        = (make-world circle1 circle2 true  MEV MOUSE-X MOUSE-Y)
;; DESIGN STRATEGY: Using template for WorldState on w

(define (world-with-paused-toggled w)
  (make-world
   (world-circ1 w)
   (world-circ2 w)
   (not (world-paused? w))
   (world-mevent w)
   (world-mx w)
   (world-my w)))

;; TESTS
(begin-for-test
  (check-equal? (world-with-paused-toggled
                 (make-world circle1 circle2 true MEV MOUSE-X MOUSE-Y))
                (make-world circle1 circle2 false MEV MOUSE-X MOUSE-Y)
                "the world wasn't paused? toggled")
  (check-equal? (world-with-paused-toggled
                 (make-world circle1 circle2 false MEV MOUSE-X MOUSE-Y))
                (make-world circle1 circle2 true MEV MOUSE-X MOUSE-Y)
                "the world wasn't paused? toggled"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

;; circ-x : Circle -> NonNegInt
;; circ-y : Circle -> NonNegInt
;; circ-vx : Circle -> Int
;; circ-vy : Circle -> Int
;; RETURNS: the coordinates of the center of the circle and its
;; velocity in the x- and y- directions.
;; EXAMPLES: circle1 is (make-circ 200 100 -12 20) as defined above.
;; (circ-x circle1) = 200
;; (circ-y circle1) = 100
;; (circ-vx circle1) = -12 
;; (circ-vy circle1) = 20
;; DESIGN STRATEGY: Predefined by struct for Circle

;; TESTS
(begin-for-test
  (check-equal? (circ-x circle1) 200 "The output obtained is not as expected")

  (check-equal? (circ-y circle1) 100 "The output obtained is not as expected")

  (check-equal? (circ-vx circle1) -12 "The output obtained is not as expected")

  (check-equal? (circ-vy circle1) 20 "The output obtained is not as expected"))

;; circ-selected? : Circle -> Boolean
;; RETURNS: true iff the given circle is selected.
;; EXAMPLES:
;; (circ-selected? circle1) = false
;; (circ-selected? circle2) = false
;; DESIGN STRATEGY: Predefined by struct for Circle

;; TESTS
(begin-for-test
  (check-equal? (circ-selected? circle1)
                false
                "The circle should be unselected but it was selected."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event
;;  : WorldState Int Int MouseEvent -> WorldState
;; GIVEN: A World, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the world that should follow the given world after the given mouse
;; event.
;; DESIGN STRATEGY: Using template for WorldState on w

(define (world-after-mouse-event w mx my mev)
  (make-world
   (circ-after-mouse-event (world-circ1 w) mx my mev)
   (circ-after-mouse-event (world-circ2 w) mx my mev)
   (world-paused? w)
   mev
   mx
   my))

;; Samples for testing;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define mouse-point-x (+ (circ-x circle1) 5))
(define mouse-point-y (+ (circ-y circle1) 0))
(define paused-world-initial-st (make-world circle1
                                            circle2
                                            true
                                            "button-down"
                                            mouse-point-x
                                            mouse-point-y))
(define c1-selected-initial (make-circ 200 100 -12 20 true 5 0))

;; for drag
(define circle-for-dragstart (make-circ 200 100 -12 20 true 5 0))
(define circle-for-dragfin (make-circ 200 100 -12 20 true 5 0))
(define paused-world-for-drag (make-world circle-for-dragstart
                                          circle2
                                          true
                                          "button-up"
                                          205
                                          100))
;; for button-up
(define output-world-initial-st (make-world c1-selected-initial
                                            circle2
                                            true
                                            "button-down"
                                            mouse-point-x
                                            mouse-point-y))

(define paused-world-initial-state-move (make-world circle1 circle2 true "move" 205 100))

;;;;End of Samples;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for world-after-mouse-event;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(begin-for-test
  ;; button-down
  (check-equal?
   (world-after-mouse-event paused-world-initial-state ;; unselected state
                            mouse-point-x 
                            mouse-point-y 
                            "button-down")
   output-world-initial-st  ;; selected state
   "button down inside circle should select it but didn't")

  ;; button-up
  (check-equal?
   (world-after-mouse-event paused-world-initial-state-selected ;; selected circle1
                                         mouse-point-x 
                                         mouse-point-y 
                                         "button-up")
   (make-world circle1 ;; unselected circle1
               circle2
               true
               "button-up"
               mouse-point-x
               mouse-point-y)
   "button-up failed to unselect ")

  ;; drag
   (check-equal?
    (world-after-mouse-event paused-world-for-drag
                             mouse-point-x 
                             mouse-point-y 
                             "drag")
    (make-world circle-for-dragfin
                circle2
                true
                "drag"
                205
                100)
    "drag when circle is selected should just move circle, but didn't")
   
   ;; tests for other mouse events
  (check-equal?
   (world-after-mouse-event paused-world-initial-state-move 
                            205
                            100    ;; arbitrary coordinate
                            "move")
   paused-world-initial-state-move
   "other mouse events should leave the world unchanged, but didn't"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; circle-after-mouse-event : Circle Integer Integer MouseEvent -> Circle
;; GIVEN: a circle and a description of a mouse event
;; RETURNS: the circle that should follow the given mouse event
;; strategy: Cases on mouse event mev

(define (circ-after-mouse-event c mx my mev)
  (cond
    [(mouse=? mev "button-down") (circle-after-button-down c mx my)]
    [(mouse=? mev "drag") (circle-after-drag c mx my)]
    [(mouse=? mev "button-up") (circle-after-button-up c mx my)]
    [else c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; offset-val-x : Circle Integer -> Integer
;; offset-val-y : Circle Integer -> Integer
;; GIVEN: a Circle c, mouse co-ordinates x and y
;; RETURNS: the offset value of for x and y coordinates of the mouse
;; EXAMPLES:
;; (offset-val-x circle1 220) = 20
;; (offset-val-y circle1 140) = 40
;; DESIGN STRATEGY: Using template for Circle on c

(define (offset-val-x c x)
  (- x (circ-x c)))

(define (offset-val-y c y)
  (- y (circ-y c)))

;; TESTS

(begin-for-test
  (check-equal? (offset-val-x circle1 220) 20 "the offset value for x wasn't calculated")
  (check-equal? (offset-val-y circle1 140) 40 "the offset value for y wasn't calculated"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; circle-after-button-down : Circle Integer Integer -> Circle
;; GIVEN: a Circle c, mouse co-ordinates x and y
;; RETURNS: the state of a given circle if
;;          it were in an unpaused world on mouse event "button-down".
;; DESIGN STRATEGY: Using template for Circle on c

(define (circle-after-button-down c x y)  
  (if(in-circle? c x y)
           (make-circ (circ-x c)
                      (circ-y c)
                      (circ-vx c)
                      (circ-vy c)
                      true
                      (offset-val-x c x)
                      (offset-val-y c y))
           c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; circle-after-drag: Circle Integer Integer -> Circle
;; GIVEN: a Circle c, mouse co-ordinates x and y
;; RETURNS: the state of a given circle if
;;          it were in an unpaused world  on mouse event "drag".
;; DESIGN STRATEGY: Using template for Circle on c

(define (circle-after-drag c x y)
  (if (circ-selected? c) 
      (make-circ (- x (circ-mx c))
                 (- y (circ-my c))
                 (circ-vx c)
                 (circ-vy c)
                 true
                 (circ-mx c)
                 (circ-my c))
      c))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; circle-after-button-up: Circle Integer Integer -> Circle
;; GIVEN: a Circle c, mouse co-ordinates x and y
;; RETURNS: the state of a given circle if
;;          it were in an unpaused world  on mouse event "button-up".
;; DESIGN STRATEGY: Using template for Circle on c

(define (circle-after-button-up c x y)
  (if (circ-selected? c)
      (make-circ (circ-x c)
                 (circ-y c)
                 (circ-vx c)
                 (circ-vy c)
                 false
                 MOUSE-X
                 MOUSE-Y)
      c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in-circle? : Circle Integer Integer -> Circle
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given circle.
;; EXAMPLES: see tests for world-after-mouse-event
;; STRATEGY: Use template for Circle on c

(define (in-circle? c x y)
  (and
    (< 
      (- (circ-x c) HALF-CIRCLE-WIDTH)
      x
      (+ (circ-x c) HALF-CIRCLE-WIDTH))
    (< 
      (- (circ-y c) HALF-CIRCLE-HEIGHT)
      y
      (+ (circ-y c) HALF-CIRCLE-HEIGHT))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; following are a part of the world struct

;; world-circ1 : WorldState -> Circle
;; world-circ2 : WorldState -> Circle
;; world-paused? : WorldState -> Boolean
;; RETURNS: the specified attribute of the WorldState
;; NOTE: if these are part of the world struct, you don't need to
;; write any deliverables for these functions.

