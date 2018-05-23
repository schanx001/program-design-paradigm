;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname screensaver-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A program to build a screensaver
;; with list of circles moving around the canvas.
;; The user can pause/unpause the simulation with space bar.
;; Simulation is paused initially.
;; User can select and drag the circles whether or not
;; the simulation is paused.
;; Various key events and mouse events invoke different features on the circles
;; within the canvas.
;; Running the simulation: give speed as an argument
;; Example: (screensaver 0.5)

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(check-location "04" "screensaver-3.rkt")

(provide screensaver
         initial-world
         world-after-tick
         world-after-key-event
         world-after-mouse-event
         circ-after-mouse-event
         circ-selected?
         world-paused?
         new-circle
         circ-x
         circ-y
         circ-vx
         circ-vy
         world-circles
         circle-after-key-event
         circle-pen-down?)

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

(define MIN-CENTER-X RADIUS)
(define MAX-CENTER-X (- CANVAS-WIDTH RADIUS))
(define MIN-CENTER-Y RADIUS)
(define MAX-CENTER-Y (- CANVAS-HEIGHT RADIUS))

(define RED-CIRCLE (circle 5 "solid" "red"))
(define TEXT-COLOR "blue")
(define TEXT-SIZE 14)
(define PEN-TRAIL (circle 1 "solid" "black"))

(define RED-OUTLINE-CIRCLE (circle RADIUS "outline" "red"))
(define BLUE-OUTLINE-CIRCLE (circle RADIUS "outline" "blue"))

(define TRAILLIST empty)
(define CIRCLELIST empty) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS:

(define-struct circ(x y vx vy selected? mx my trailpoints pen-down?))

;; A Circle is a (make-circ NonNegInteger NonNegInteger Integer Integer Boolean Boolean)
;; Interpretation:
;; x is the x co-ordinate of the circle's center
;; y is the y co-ordinate of the circle's center
;; vx is the velocity of the circle in x axis direction 
;; vy is the velocity of the circle in y axis direction
;; selected? describes whether or not the circle is selected
;; mx is the x co-ordinate of a mouse event within the circle
;; my is the y co-ordinate of a mouse event within the circle
;; trailpoints is the list of trail points in the form of a solid circle of radius 1
;; pen-down? describes whether or not the pen is down for the circle

;; template:
;; circ-fn: Circle -> ??
;; (define (circ-fn c)
;;   (...(circ-x c)
;;       (circ-y c)
;;       (circ-vx c)
;;       (circ-vy c)
;;       (circ-selected? c)
;;       (circ-mx c)
;;       (circ-my c)
;;       (circ-trailer c)
;;       (circ-pen-down? c)))

(define-struct trailpoint (x y))

;; A TrailPoint is a (make-trailpoint NonNegInt NonNegInt)
;; Intepretation:
;; x is the x co-ordinate for the center of the trailpoint
;; y is the y co-ordinate for the center of the trailpoint

;; template:
;; trailpoint-fn: TrailPoint -> ??
;; (define (trailpoint-fn t)
;;   (...(trailpoint-x t)
;;       (trailpoint-y t)))

;; A ListOfTrailPoint (LOTP) is either
;; -- empty
;; -- (cons TrailPoint LOTP)

;; Template:
;; lotp-fn : LOTP -> ??
;; HALTING MEASURE: (length lst)
;; (define (lotp-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... (first lst)
;;                (lotp-fn (rest lst)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct world(circles paused? mevent mx my))

;; A WorldState is a (make-world Circle Boolean MouseEvent Integer Integer)
;; Interpretation:
;; circles is a list of circles 
;; paused? describes whether or not the scene is paused.
;; mevent  is a mouse event
;; mx      is the x co-ordinate of the mouse pointer location 
;; my      is the y co-ordinate of the mouse pointer location

;; template:
;; worldstate-fn: WorldState -> ??
;; (define (world-fn w)
;;     (...(world-circles w)
;;         (world-paused? w)
;;         (world-mevent w)
;;         (world-mx w)
;;         (world-my w)))

;; A ListOfCircle (LOC) is either
;; -- empty
;; -- (cons Circle LOC)

;; Template:
;; ;; loc-fn : LOC -> ??
;; ;; HALTING MEASURE: (length loc)
;; (define (loc-fn loc)
;;   (cond
;;     [(empty? loc) ...]
;;     [else (... (first loc)
;;                (loc-fn (rest loc)))]))

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

;; A KeyEvent is one of
;; --"up"
;; --"down"
;; --"right"
;; --"left"
;; --"n"
;; --"e"
;; --"d"
;; --"u"
;; --" "

;; kev-fn: KeyEvent -> ??
;; (define (kev-fn kev)
;;  (cond
;;     [(string=? kev "up")...]
;;     [(string=? kev "down")...]
;;     [(string=? kev "right")...]
;;     [(string=? kev "left")...]
;;     [(string=? kev "n")...]
;;     [(string=? kev "d")...]
;;     [(string=? kev "e")...]
;;     [(string=? kev "u")...]
;;     [(string=? kev " ")...]))

;; help function for key event
;; is-pause-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pause instruction

(define (is-pause-key-event? k)
  (key=? k " "))

;; examples for testing
(define pause-key-event " ")
(define non-pause-key-event "q") 

;; END DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Samples for tests:

(define trail1 (make-trailpoint 200 150))
(define traillist (cons trail1 empty))
(define circle1us (make-circ 200 150 2 2 false 0 0 traillist false))
(define circle1s (make-circ 200 150 2 2 true 0 0 traillist false))
(define circle1us-next (make-circ 202 152 2 2 false 0 0 traillist false))
(define circle1s-next (make-circ 200 150 2 2 true 0 0 traillist false))

(define circlelist '())
(define circle1us-new (make-circ 200 150 0 0 false 0 0 empty false))
(define circle1s-new (make-circ 200 150 0 0 true 0 0 empty false))
(define circlelist1 (cons circle1us-new circlelist))
(define circlelist2 (cons circle1s-new circlelist))
(define world-paused (make-world circlelist 
                            true
                            "drag"
                            20
                            20))

(define trail2 (make-trailpoint 200 150))
(define traillist1 (cons trail1 empty))
(define circlelist1-selected (cons circle1s empty))
(define circlelist1-unselected (cons circle1us empty))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; EXAMPLES:
;; (initial-world 1) = (make-world
;;                        circles true MEV MOUSE-X MOUSE-Y)
;; (initial-world "str") = (make-world
;;                           circles true MEV MOUSE-X MOUSE-Y)
;; (initial-world true) = (make-world
;;                           circles true MEV MOUSE-X MOUSE-Y)
;; DESIGN STRATEGY: using template for WorldState

(define (initial-world any)
  (make-world CIRCLELIST true MEV MOUSE-X MOUSE-Y))

;; TESTS

(begin-for-test
  (check-equal? (initial-world 1)
                (make-world circlelist
                            true
                            MEV
                            MOUSE-X
                            MOUSE-Y) "The initial world state wasn't returned")
  (check-equal? (initial-world "str")
                (make-world circlelist
                            true
                            MEV
                            MOUSE-X
                            MOUSE-Y) "The initial world state wasn't returned")
  (check-equal? (initial-world true)
                (make-world circlelist
                            true
                            MEV
                            MOUSE-X
                            MOUSE-Y) "The initial world state wasn't returned"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene: WorldState -> Scene
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE:
;; (world-to-scene paused-world-initial-state)
;;  = circles-of-paused-world-initial-state
;; DESIGN STRATEGY: using template for WorldState on w

(define (world-to-scene w)
  (place-mouse-pointer w
                      (place-circ (world-circles w) EMPTY-CANVAS)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; place-trail-list: ListOfTrailPoint Scene -> Scene
;; GIVEN: a lotp and a scene
;; RETURN: a scene like the given one, but with the trailpoints printed on the scene
;; DESIGN STRATEGY: Use template for ListOfTrailPoint on lst
;; HALTING MEASURE: (length lst)

(define (place-trail-list lst s)
   (cond
     [(empty? lst) s]
     [else (place-trailpoint (first lst)
                        (place-trail-list (rest lst) s))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; place-trailpoint: TrailPoint Scene -> Scene 
;; GIVEN: a trailpoint and a scene
;; RETURNS: a scene like given one, but with a trailpoint drawn on every tick 
;; DESIGN STRATEGY: Using template for TrailPoint on t

(define (place-trailpoint t s)
  (place-image PEN-TRAIL
               (trailpoint-x t)
               (trailpoint-y t)
               s))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; place-mouse-pointer: WorldState Scene -> Scene
;; RETURN: a scene like the given one, but with the mouse location
;;         indicated by a red circle on mouse events like button-down
;;         drag. 
;; DESIGN STRATEGY: Use cases on MouseEvent

(define (place-mouse-pointer w s)

   (if (or (mouse=? "drag" (world-mevent w))
           (mouse=? "button-down" (world-mevent w)))
       (place-image
        RED-CIRCLE
       (world-mx w)
       (world-my w)
       s)
       s))

;; TESTS:
;; Samples
(define pointer (place-image RED-CIRCLE
                             20
                             20
                             EMPTY-CANVAS))

(begin-for-test
  (check-equal? (place-mouse-pointer world-paused EMPTY-CANVAS)
                pointer
                "the pointer didn't show on display but it should have shown"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; place-circ: ListOfCircle Scene -> Scene
;; RETURN: a scene like the given one, but with the given circles
;;         drawn and their respective velocities painted on it.
;; DESIGN STRATEGY: Using template for ListOfCircle on loc
;; HALTING MEASURE: (length loc)

(define (place-circ loc s)
  (cond
    [(empty? loc) EMPTY-CANVAS]
    [else (place-outline-circle (first loc)
                                (place-circ (rest loc) s))]))

;; image output for testing
(define image-circle ;; red outline circle 
  (place-image (overlay (text"(2,2)" 14 "blue") RED-OUTLINE-CIRCLE)
                                  200
                                  150
                                  (place-trail-list (circ-trailpoints circle1s)
                                                    EMPTY-CANVAS)))

(define image-circle1 ;; blue outline circle
  (place-image (overlay (text"(2,2)" 14 "blue") BLUE-OUTLINE-CIRCLE)
                                  200
                                  150
                                  (place-trail-list (circ-trailpoints circle1s)
                                                    EMPTY-CANVAS)))


(begin-for-test
  (check-equal? (place-circ circlelist1-selected EMPTY-CANVAS)
                image-circle
                "the circle returned should have a red outline")
  (check-equal? (place-circ circlelist1-unselected EMPTY-CANVAS)
                image-circle1
                "the circle returned should have a blue outline"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; place-outline-circle: Circle Scene -> Scene
;; GIVEN: a circle and a scene
;; RETURNS: a scene like given one, but with a circle drawn onto it.
;; DESIGN STRATEGY: Using template for Circle on c

(define (place-outline-circle c s)
  (place-image (overlay (text
                         (string-append "("
                                        (number->string (circ-vx c))
                                        ","
                                        (number->string (circ-vy c))
                                        ")") TEXT-SIZE TEXT-COLOR)
                        (if(circ-selected? c)
                           RED-OUTLINE-CIRCLE
                           BLUE-OUTLINE-CIRCLE))
               (circ-x c)
               (circ-y c)
               (place-trail-list (circ-trailpoints c) s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS for world-to-scene:
;; Samples for tests
(define paused-world-initial-state (make-world circlelist true MEV MOUSE-X MOUSE-Y))
(define circles-of-paused-world-initial-state
  (place-mouse-pointer paused-world-initial-state
                       (place-circ (world-circles paused-world-initial-state)
                                   EMPTY-CANVAS)))

(begin-for-test
  (check-equal?
    (world-to-scene paused-world-initial-state)
    circles-of-paused-world-initial-state
    "(world-to-scene paused-world-initial-state) returned incorrect image"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
       (clist-after-tick (world-circles w))
       (world-paused? w)
       (world-mevent w)
       (world-mx w)
       (world-my w))))

;; SAMPLES for tests:

(define circlelist-for-after-tick (cons circle1us-next empty))
(define unpaused-world-initial-state
  (make-world circlelist1-unselected false MEV MOUSE-X MOUSE-Y)) ;;1 element in list
(define unpaused-world-after-1-tick
  (make-world circlelist-for-after-tick false MEV MOUSE-X MOUSE-Y))
(define paused-world-after-1-tick
  (make-world circlelist true MEV MOUSE-X MOUSE-Y)) ;;empty list


;; TESTS for world-after-tick
(begin-for-test
  (check-equal? (world-after-tick unpaused-world-initial-state)
                unpaused-world-after-1-tick
                "the world state returned isn't the one expected")
  (check-equal? (world-after-tick paused-world-initial-state)
                paused-world-after-1-tick
                "the world state returned isn't the one expected"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; clist-after-tick: ListOfCircle -> ListOfCircle
;; GIVEN: a list of circle 
;; RETURNS: a list of circle, but with the updated values of
;;          each circle for its parameters.
;; DESIGN STRATEGY: Using template for ListOfCircle on loc

(define (clist-after-tick loc)
   (cond
     [(empty? loc) loc]
     [else (cons (circle-after-tick (first loc))
                (clist-after-tick (rest loc)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; circle-after-tick: Circle -> Circle
;; GIVEN: the state of circle c
;; RETURNS: the state of the given circle after a tick if
;;          it were in an unpaused world.
;; EXAMPLES: (circle-after-tick circle1-selected-initial)
;;              = (circle1-selected-initial)
;; circle-selected-initial is (200 100 -12 20 true 0 0 empty false)
;; DESIGN STRATEGY: Using template for Circle on c

(define (circle-after-tick c)
  (if (circ-selected? c)
      c
      (circle-new-pos (make-circ (circ-x c)
                                 (circ-y c)
                                 (circ-vx c)
                                 (circ-vy c)
                                 (circ-selected? c)
                                 (circ-mx c)
                                 (circ-my c)
                                 (circle-trail-after-tick c)
                                 (circ-pen-down? c)))))

;; TESTS:
(define circle-selected-initial (make-circ 200 100 -12 20 true 0 0 empty false))
(begin-for-test
  (check-equal? (circle-after-tick circle-selected-initial)
                circle-selected-initial
                "the circle should have stopped moving but it didn't on selection by mouse"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; circle-trail-after-tick: Circle -> ListOfTrailPoint
;; GIVEN: a circle
;; RETURNS: a new list of trailpoints when the pen is down
;;          else returns the same list of trailpoints
;; DESIGN STRATEGY: Using template for Circle on c

(define (circle-trail-after-tick c)
  (if (circ-pen-down? c) 
      (cons (make-trailpoint (circ-x c) (circ-y c)) (circ-trailpoints c))
      (circ-trailpoints c)))
               
;; TESTS:
(define circl-pendown (make-circ 200 150 0 0 false 0 0 empty true))
(begin-for-test
  (check-equal? (circle-trail-after-tick circl-pendown)
                traillist))

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
             MOUSE-Y
             (circle-trail-after-tick c)
             (circ-pen-down? c)))

(define (right-wall-adjustment c)
  (make-circ MAX-CENTER-X
             (+ (circ-y c) (circ-vy c))
             (* (circ-vx c) -1)
             (circ-vy c)
             (circ-selected? c)
             MOUSE-X
             MOUSE-Y
             (circle-trail-after-tick c)
             (circ-pen-down? c)))

(define (top-wall-adjustment c)
  (make-circ (+ (circ-x c) (circ-vx c))
             MIN-CENTER-Y
             (circ-vx c)
             (* (circ-vy c) -1)
             (circ-selected? c)
             MOUSE-X
             MOUSE-Y
             (circle-trail-after-tick c)
             (circ-pen-down? c)))
  
(define (bottom-wall-adjustment c)
  (make-circ (+ (circ-x c) (circ-vx c))
             MAX-CENTER-Y
             (circ-vx c)
             (* (circ-vy c) -1)
             (circ-selected? c)
             MOUSE-X
             MOUSE-Y
             (circle-trail-after-tick c)
             (circ-pen-down? c)))

(define (top-right-adjustment c)
  (make-circ MAX-CENTER-X
             MIN-CENTER-Y
             (* (circ-vx c) -1)
             (* (circ-vy c) -1)
             (circ-selected? c)
             MOUSE-X
             MOUSE-Y
             (circle-trail-after-tick c)
             (circ-pen-down? c)))


(define (bottom-right-adjustment c)
  (make-circ MAX-CENTER-X
             MAX-CENTER-Y
             (* (circ-vx c) -1)
             (* (circ-vy c) -1)
             (circ-selected? c)
             MOUSE-X
             MOUSE-Y
             (circle-trail-after-tick c)
             (circ-pen-down? c)))

(define (bottom-left-adjustment c)
  (make-circ MIN-CENTER-X
             MAX-CENTER-Y
             (* (circ-vx c) -1)
             (* (circ-vy c) -1)
             (circ-selected? c)
             MOUSE-X
             MOUSE-Y
             (circle-trail-after-tick c)
             (circ-pen-down? c)))

(define (top-left-adjustment c)
  (make-circ MIN-CENTER-X
             MIN-CENTER-Y
             (* (circ-vx c) -1)
             (* (circ-vy c) -1)
             (circ-selected? c)
             MOUSE-X
             MOUSE-Y
             (circle-trail-after-tick c)
             (circ-pen-down? c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                     (circ-selected? c)
                     0
                     0
                     (circ-trailpoints c)
                     (circ-pen-down? c))]))

;; TESTS:
;; SAMPLES
(define circle1 (make-circ 200 100 -12 20 false 0 0 traillist true))
(define circle2 (make-circ 200 200 23 -14 false 0 0 traillist true))
(define circle1t (make-circ 188 120 -12 20 false 0 0 traillist true))
(define circle2t (make-circ 223 186 23 -14 false 0 0 traillist true))
(define circle-b1 (make-circ 45 80 -12 20 false 0 0 traillist false))
(define circle-b2 (make-circ 355 200 12 20 false 0 0 traillist false))
(define circle-b3 (make-circ 45 42 12 -20 false 0 0 traillist false))
(define circle-b4 (make-circ 57 255 12 20 false 0 0 traillist false))
(define circle-b5 (make-circ 357 46 12 -20 false 0 0 traillist false))
(define circle-b6 (make-circ 357 250 12 20 false 0 0 traillist false))
(define circle-b7 (make-circ 42 257 -12 20 false 0 0 traillist false))
(define circle-b8  (make-circ 45 43 -12 -20 false 0 0 traillist false))

;; Tests for circle
(begin-for-test

  ;; (x,y,vx,vy) in comments is the center co-ordinates and the
  ;; velocities of the circle
  
  (check-equal? (circle-new-pos circle1) ;; normal flow for else case
                circle1t
                "the circle should be at x=188 and y=120, for (200,100,-12,20)")
  
  (check-equal? (circle-new-pos circle2) ;; normal flow for else case
                circle2t
                "the circle should be at x=223 and y=186, for (200,200,23,-14)")
  
  (check-equal? (circle-new-pos circle-b1)
                ;; for left boundary
                (make-circ 40 100 12 20 false 0 0 traillist false) 
                "the circle should be at x=40 and y=100, for (45,80,-12,20)")

  (check-equal? (circle-new-pos circle-b2)
                ;; for right boundary
                (make-circ 360 220 -12 20 false 0 0 traillist false)
                "the circle should be at x=360 and y=220, for (355,200,12,20)")

  (check-equal? (circle-new-pos circle-b3)
                ;; for bottom boundary
                (make-circ 57 40 12 20 false 0 0 traillist false)
                "the circle should be at x=57 and y=40, for (45,42,12,-20)")

  (check-equal? (circle-new-pos circle-b4)
                ;; for top boundary
                (make-circ 69 260 12 -20 false 0 0 traillist false)
                "the circle should be at x=69 and y=260, for (57,255,12,20)")

  (check-equal? (circle-new-pos circle-b5)
                ;; for top-right corner
                (make-circ 360 40 -12 20 false 0 0 traillist false)
                "the circle should be at x=360 and y=40, for (357,46,12,-20)")

  (check-equal? (circle-new-pos circle-b6)
                ;; for bottom-right corner
                (make-circ 360 260 -12 -20 false 0 0 traillist false)
                "the circle should be at x=360 and y=260, for (357,250,12,20)")

  (check-equal? (circle-new-pos circle-b7)
                ;; for bottom-left corner 
                (make-circ 40 260 12 -20 false 0 0 traillist false)
                "the circle should be at x=40 and y=260, for (42,257,-12,20)")

  (check-equal? (circle-new-pos circle-b8)
                ;; for top-left corner
                (make-circ 40 40 12 20 false 0 0 traillist false)
                "the circle should be at x=40 and y=40, for (45,43,-12,-20)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-circle : NonNegInt NonNegInt Int Int -> Circle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a circle centered at (x,y), which will travel with
;; velocity (vx, vy) and unselected.
;; EXAMPLES: (new-circle 200 100 -12 20) = (make-circ 200 100 -12 20 false 0 0)
;; DESIGN STRATEGY: Using template for Circle

(define (new-circle x y vx vy)
  (make-circ x y vx vy false MOUSE-X MOUSE-Y TRAILLIST false))

;; TESTs for new-circle
(begin-for-test
  (check-equal? (new-circle 200 100 -12 20)
                (make-circ 200 100 -12 20 false 0 0 TRAILLIST false)
                "the new circle was not created"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; RETURNS: the WorldState that should follow the given worldstate
;; after the given keyevent
;; EXAMPLES:
;; refer tests
;; DESIGN STRATEGY: Cases on KeyEvents 

(define (world-after-key-event w kev)
  (cond
    [(is-pause-key-event? kev) (world-with-paused-toggled w)]
    [(key=? kev "n") (world-new-circle w)]
    [else (make-world (circle-list-after-key-event (world-circles w) kev)
                      (world-paused? w)
                      (world-mevent w)
                      (world-mx w)
                      (world-my w))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-new-circle : WorldState -> WorldState
;; RETURNS: the WorldState that should follow the given worldstate
;; after the given keyevent
;; EXAMPLES:
;; refer tests
;; DESIGN STRATEGY: Using template for WorldState on w

(define (world-new-circle w)
  (make-world (cons (new-circle (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2) 0 0)
                                       (world-circles w))
                                 (world-paused? w)
                                 MEV
                                 MOUSE-X
                                 MOUSE-Y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS for world-after-key-event
;; Samples
(define unpaused-world-initial-state1 (make-world circlelist false MEV MOUSE-X MOUSE-Y))
(define circle-e (make-circ 200 150 0 0 true 0 0 traillist false))
(define circlelist-output-on-e (cons circle1s-new empty))
(define circlelist-erased (cons circle-e empty))

(begin-for-test
  (check-equal? (world-after-key-event paused-world-initial-state pause-key-event)
                unpaused-world-initial-state1
                "after pause key, paused world should become unpaused")
  (check-equal? (world-after-key-event unpaused-world-initial-state1 pause-key-event)
                paused-world-initial-state
                "after pause key, unpaused world should become paused")
  (check-equal? (world-after-key-event (make-world circlelist
                                                   true
                                                   MEV
                                                   MOUSE-X
                                                   MOUSE-Y) "n")
                (make-world circlelist1
                            true
                            MEV
                            MOUSE-X
                            MOUSE-Y)
                "the new circle was not created on the world state")
  (check-equal? (world-after-key-event (make-world circlelist-erased
                                                   false
                                                   MEV
                                                   MOUSE-X
                                                   MOUSE-Y) "e")
                (make-world circlelist-output-on-e
                            false
                            MEV
                            MOUSE-X
                            MOUSE-Y)
                "the trailpoints were not erased from the scene"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-with-paused-toggled : WorldState -> WorldState
;; RETURNS: a world just like the given one, but with paused? toggled
;; STRATEGY: Use template for WorldState on w
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
   (world-circles w)
   (not (world-paused? w))
   (world-mevent w)
   (world-mx w)
   (world-my w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; circle-list-key-event: ListOfCircle KeyEvent -> ListOfCircle 
;; GIVEN: a list of circle and a key event
;; RETURNS: a list like before,but with the updated circles after the key event
;; EXAMPLES: refer test cases
;; DESIGN STRATEGY: Using template for ListOfCircle on loc
;; HALTING MEASURE: (length lst)


(define (circle-list-after-key-event loc kev)
  (cond
    [(empty? loc) empty]
    [else (cons (circle-after-key-event (first loc) kev)
                (circle-list-after-key-event (rest loc) kev))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; circle-after-key-event : Circle KeyEvent -> Circle
;; RETURNS: the state of the circle that should follow the given
;; circle after the given key event
;; EXAMPLES:
;;  circle1s is (make-circ 200 150 2 2 true 0 0 traillist false)
;;  circle1us is (make-circ 200 150 2 2 false 0 0 traillist false)
;;  (circle-after-key-event circle1s "n") = circle1us
;; DESIGN STRATEGY: Cases on KeyEvents

(define (circle-after-key-event c kev)
  (if (circ-selected? c)
      (cond
        [(key=? "n" kev) (new-circle (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2) 0 0)]
        [(key=? "d" kev) (circle-after-key-d c)]
        [(key=? "e" kev) (circle-after-key-e c)]
        [(key=? "u" kev) (circle-after-key-u c)]
        [(key=? "up" kev) (circle-after-key-up c)]
        [(key=? "right" kev) (circle-after-key-right c)]
        [(key=? "down" kev) (circle-after-key-down c)]
        [(key=? "left" kev) (circle-after-key-left c)]
        [else c])
      c))

;; TESTS:

;; Samples:
;(define traillst-d (cons (make-trailpoint 202 152) traillist))
(define circle-after-d (make-circ 200 150 2 2 true 0 0 traillist true))
(define circle1sd (make-circ 200 150 2 2 true 0 0 empty true))

(define circle-after-u (make-circ 200 150 2 2 true 0 0 traillist false))
(define circle1su (make-circ 200 150 2 2 true 0 0 traillist true))

(define circle-after-up (make-circ 200 150 2 0 true 0 0 traillist false))
(define circle1sup (make-circ 200 150 2 2 true 0 0 traillist false))

(define circle-after-right (make-circ 200 150 4 2 true 0 0 traillist false))
(define circle1sright (make-circ 200 150 2 2 true 0 0 traillist false))

(define circle-after-down (make-circ 200 150 2 4 true 0 0 traillist false))
(define circle1sdown (make-circ 200 150 2 2 true 0 0 traillist false))

(define circle-after-left (make-circ 200 150 0 2 true 0 0 traillist false))
(define circle1sleft (make-circ 200 150 2 2 true 0 0 traillist false))


(begin-for-test
  (check-equal? (circle-after-key-event circle1us "d")
                circle1us
                "the returned circle should be same as the input circle")

  (check-equal? (circle-after-key-event circle1s "n")
                circle1us-new
                "the new circle was not created")

  (check-equal? (circle-after-key-event circle1sd "d")
                circle-after-d
                "the dot was not added to the trailpoints list")
  
  (check-equal? (circle-after-key-event circle1su "u")
                circle-after-u
                "the circle didn't stop writing dots as the pen is still down")

  (check-equal? (circle-after-key-event circle1sup "up")
                circle-after-up
                "the circle's velocity should be vy=0 for input vy=2")

  (check-equal? (circle-after-key-event circle1sright "right")
                circle-after-right
                "the circle's velocity should be vx=4 for input vx=2")

  (check-equal? (circle-after-key-event circle1sleft "left")
                circle-after-left
                "the circle's velocity should be vx=0 for input vx=2")

  (check-equal? (circle-after-key-event circle1sdown "down")
                circle-after-down
                "the circle's velocity should be vy=4 for input vy=2")

  (check-equal? (circle-after-key-event circle1sright "q")
                ;; for else condition
                circle1sright
                "the circle's velocity should be vx=4 for input vx=2"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; circle-after-key-d: Circle -> Circle
;; circle-after-key-e: Circle -> Circle
;; circle-after-key-u: Circle -> Circle
;; circle-after-key-up: Circle -> Circle
;; circle-after-key-right: Circle -> Circle
;; circle-after-key-down: Circle -> Circle
;; circle-after-key-left: Circle -> Circle
;; GIVEN: a circle
;; RETURNS: a circle as before, but with the updated values
;;          based on the key events
;; DESIGN STRATEGY: Using template for Circle on c

(define (circle-after-key-d c)
  (make-circ (circ-x c)
             (circ-y c)
             (circ-vx c)
             (circ-vy c)
             (circ-selected? c)
             (circ-mx c)
             (circ-my c)
             (cons (make-trailpoint (circ-x c) (circ-y c)) (circ-trailpoints c))
             true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (circle-after-key-e c)
  (make-circ (circ-x c)
             (circ-y c)
             (circ-vx c)
             (circ-vy c)
             (circ-selected? c)
             (circ-mx c)
             (circ-my c)
             empty
             (circ-pen-down? c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (circle-after-key-u c)
  (make-circ (circ-x c)
             (circ-y c)
             (circ-vx c)
             (circ-vy c)
             (circ-selected? c)
             (circ-mx c)
             (circ-my c)
             (circ-trailpoints c)
             false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (circle-after-key-up c)
  (make-circ (circ-x c)
             (circ-y c)
             (circ-vx c)
             (- (circ-vy c) 2)
             (circ-selected? c)
             (circ-mx c)
             (circ-my c)
             (circ-trailpoints c)
             (circ-pen-down? c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (circle-after-key-right c)
  (make-circ (circ-x c)
             (circ-y c)
             (+ (circ-vx c) 2)
             (circ-vy c)
             (circ-selected? c)
             (circ-mx c)
             (circ-my c)
             (circ-trailpoints c)
             (circ-pen-down? c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (circle-after-key-down c)
  (make-circ (circ-x c)
             (circ-y c)
             (circ-vx c)
             (+ (circ-vy c) 2)
             (circ-selected? c)
             (circ-mx c)
             (circ-my c)
             (circ-trailpoints c)
             (circ-pen-down? c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (circle-after-key-left c)
  (make-circ (circ-x c)
             (circ-y c)
             (- (circ-vx c) 2)
             (circ-vy c)
             (circ-selected? c)
             (circ-mx c)
             (circ-my c)
             (circ-trailpoints c)
             (circ-pen-down? c)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event
;;  : WorldState Int Int MouseEvent -> WorldState
;; GIVEN: A World, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the world that should follow the given world after the given mouse
;; event.
;; DESIGN STRATEGY: Using template for WorldState on w

(define (world-after-mouse-event w mx my mev)
  (make-world
   (circlist-after-mouse-event (world-circles w) mx my mev)
   (world-paused? w)
   mev
   mx
   my))

;; Samples for tests


(define c1-us-new (make-circ 200 100 -12 20 false 0 0 empty false))
(define c1-s-new (make-circ 200 100 -12 20 true 5 0 empty false))
(define c1-list (cons c1-us-new circlelist))
(define c2-list (cons c1-s-new circlelist))
(define mouse-point-x (+ (circ-x c1-us-new) 5))
(define mouse-point-y (+ (circ-y c1-us-new) 0))

(define world1
  (make-world c1-list true MEV MOUSE-X MOUSE-Y))
(define world2
  (make-world c2-list true "button-down" mouse-point-x mouse-point-y))

(define c3-s-new (make-circ 200 100 -12 20 true 0 0 empty false))
(define c3-list (cons c3-s-new circlelist))
(define world3
  (make-world c3-list true MEV MOUSE-X MOUSE-Y))

(define c4-s-new (make-circ 205 100 -12 20 true 0 0 empty false))
(define c4-list (cons c4-s-new circlelist))

(define c5-s-new (make-circ 200 100 -12 20 true 0 0 empty false))
(define c5-list-s (cons c5-s-new circlelist))
(define world5
  (make-world c5-list-s true "move" 205 100))

;; TESTS:

(begin-for-test
  ;; button-down
  (check-equal?
   (world-after-mouse-event world1 ;; unselected state
                            mouse-point-x
                            mouse-point-y
                            "button-down")
   world2  ;; selected state
   "button down inside circle should select it but didn't")

  ;; button-up
  (check-equal?
   (world-after-mouse-event world3 ;; selected circle1
                            MOUSE-X 
                            MOUSE-Y 
                            "button-up")
   (make-world c1-list ;; unselected circle1
               true
               "button-up"
               MOUSE-X
               MOUSE-Y)
   "button-up failed to unselect ")

;; drag
   (check-equal?
    (world-after-mouse-event world3
                             mouse-point-x 
                             mouse-point-y 
                             "drag")
    (make-world c4-list
                true
                "drag"
                205
                100)
    "drag when circle is selected should just move circle, but didn't")

;; tests for other mouse events
  (check-equal?
   (world-after-mouse-event world5 
                            205
                            100    ;; arbitrary coordinate
                            "move")
   world5
   "other mouse events should leave the world unchanged, but didn't"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; circlist-after-mouse-event: ListOfCircle Integer Integer MouseEvent -> ListOfCircle
;; GIVEN: a list of circle and description of a mouse event
;; RETURNS: a list of circle, updated with circles based on the mouse event.
;; DESIGN STRATEGY: Using template for ListOfCircle on loc
(define (circlist-after-mouse-event loc mx my mev)
  (cond
    [(empty? loc) loc]
    [else (cons (circ-after-mouse-event (first loc) mx my mev)
                (circlist-after-mouse-event (rest loc) mx my mev))]))

;; circ-after-mouse-event : Circle Integer Integer MouseEvent -> Circle
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
;; Sample
(define circle-offset (make-circ 200 100 0 0 false 0 0 empty false))

(begin-for-test
  (check-equal? (offset-val-x circle-offset 220) 20
                "the offset value for x wasn't calculated")
  (check-equal? (offset-val-y circle-offset 140) 40
                "the offset value for y wasn't calculated"))

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
                      (offset-val-y c y)
                      (circ-trailpoints c)
                      (circ-pen-down? c))
           c))

;; TESTS:

(begin-for-test
  (check-equal? (circle-after-button-down c1-us-new 0 0)
                c1-us-new
                "the circle was not returned"))

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
                 (circ-my c)
                 (circ-trailpoints c)
                 (circ-pen-down? c))
      c))

;; TESTS:

(begin-for-test
  (check-equal? (circle-after-drag c1-us-new 0 0)
                c1-us-new
                "the circle was not returned"))

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
                 MOUSE-Y
                 (circ-trailpoints c)
                 (circ-pen-down? c))
      c))

;; TESTS:

(begin-for-test
  (check-equal? (circle-after-button-up c1-us-new 0 0)
                c1-us-new
                "the circle was not returned"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in-circle? : Circle Integer Integer -> Circle
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given circle.
;; EXAMPLES: see tests for world-after-mouse-event
;; STRATEGY: Use template for Circle on c

(define (in-circle? c x y)
  (< (+ (sqr (- x (circ-x c)))
        (sqr (- y (circ-y c)))) (sqr RADIUS)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; circle-pen-down? : Circle -> Boolean
;; RETURNS: true if the pen in the given circle is down
;; EXAMPLES: (circle-pen-down circle-pendown) = true
;; DESIGN STRATEGY: Using template for Circle on c

(define (circle-pen-down? c)
  (circ-pen-down? c))

;; TESTS
;; Sample for test
(define circle-pendown (make-circ 200 100 -20 10 false 0 0 traillist true))

(begin-for-test
  (check-equal? (circle-pen-down? circle-pendown)
                true
                "the circle-pen-down should have been true"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (check-equal? (circ-x circle1) 200 "The x coordinate output should be 200")

  (check-equal? (circ-y circle1) 100 "The y coordinate output should be 100")

  (check-equal? (circ-vx circle1) -12 "The vx coordinate output should be -12")

  (check-equal? (circ-vy circle1) 20 "The vy coordinate output should be 20"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;