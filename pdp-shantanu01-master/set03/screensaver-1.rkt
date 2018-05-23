;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname screensaver-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A program to build a screensaver
;; with two circles moving around the canvas.
;; The user can pause/unpause the simulation with space bar.
;; Simulation is paused initially.
;; Running the simulation: give speed as an argument
;; Example: (screensaver 0.5)


(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(check-location "03" "screensaver-1.rkt")

(provide screensaver
         initial-world
         world-after-tick
         world-after-key-event
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

;; Maximum and Minimum co-ordinates for center of the 
;; circle to be within the canvas

(define MIN-CENTER-X (+ 0 RADIUS))
(define MAX-CENTER-X (- CANVAS-WIDTH RADIUS))
(define MIN-CENTER-Y (+ 0 RADIUS))
(define MAX-CENTER-Y (- CANVAS-HEIGHT RADIUS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS:

(define-struct circ(x y vx vy))

;; A Circle is a (make-circ NonNegInteger NonNegInteger Integer Integer)
;; Interpretation:
;; x is the x co-ordinate of the circle's center
;; y is the y co-ordinate of the circle's center
;; vx is the velocity of the circle in x axis direction 
;; vy is the velocity of the circle in y axis direction

;; template:
;; circ-fn: Circle -> ??
;; (define (circ-fn c)
;;   (...(circ-x)
;;       (circ-y)
;;       (circ-vx)
;;       (circ-vy)))

;; 2 Circles for Test and as an Example

(define circle1 (make-circ 200 100 -12 20))
(define circle2 (make-circ 200 200 23 -14))

;; Samples to be used for tests
(define circle1t (make-circ 188 120 -12 20))
(define circle2t (make-circ 223 186 23 -14))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct world(circ1 circ2 paused?))

;; A WorldState is a (make-world Circle Circle Boolean)
;; Interpretation:
;; circle1 is one of the circles of the scene
;; circle2 is the other circle of the scene
;; paused? describes whether or not the scene is paused

;; template:
;; world-fn: WorldState -> ??
;; (define (world-fn w)
;;     (...(world-circ1)
;;         (world-circ2)
;;         (world-paused?)))

;; Examples and sample for tests

(define unpaused-world-initial-state (make-world circle1 circle2 false))
(define unpaused-world-after-1-tick (make-world circle1t circle2t false))

(define paused-world-initial-state (make-world circle1 circle2 true))
(define paused-world-after-1-tick (make-world circle1 circle2 true))

;; help function for key event
;; is-pause-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pause instruction

(define (is-pause-key-event? k)
  (key=? k " "))

;; examples for testing
(define pause-key-event " ")
(define non-pause-key-event "q") 


;;; END DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; screensaver : PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state as
;; specified in the problem set.
;; RETURNS: the final state of the world

(define (screensaver speed)
  (big-bang (initial-world "sa")
            (on-tick world-after-tick speed)
            (on-key world-after-key-event)
            (on-draw world-to-scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; EXAMPLES:
;; (initial-world 1) = (make-world (make-circ 200 100 -12 20) (make-circ 200 200 23 -14) true)
;; (initial-world "str") = (make-world (make-circ 200 100 -12 20) (make-circ 200 200 23 -14) true)
;; (initial-world true) = (make-world (make-circ 200 100 -12 20) (make-circ 200 200 23 -14) true)
;; DESIGN STRATEGY: using template for WorldState

(define (initial-world any)
  (make-world circle1 circle2 true))

;; TESTS 
(begin-for-test
  (check-equal? (initial-world 1)
                (make-world (make-circ 200 100 -12 20)
                            (make-circ 200 200 23 -14)
                            true) "The world returned wasn't the one expected")
  (check-equal? (initial-world "str")
                (make-world (make-circ 200 100 -12 20)
                            (make-circ 200 200 23 -14)
                            true) "The world returned wasn't the one expected")
  (check-equal? (initial-world true)
                (make-world (make-circ 200 100 -12 20)
                            (make-circ 200 200 23 -14)
                            true) "The world returned wasn't the one expected"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world-to-scene: WorldState -> Scene
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE:
;; (world-to-scene paused-world-initial-state) should return two circles with their centers at
;; (200,100) and (200,200) respectively.
;; DESIGN STRATEGY: using template for WorldState on w

(define (world-to-scene w)
  (place-circ (world-circ1 w)
              (place-circ
              (world-circ2 w)
              EMPTY-CANVAS)))

;; place-circ: Circle Scene -> Scene
;; RETURN: a scene like the given one, but with the given circles
;;         and their respective velocities painted on it.
;; DESIGN STRATEGY: Using template for Circle on c

(define (place-circ c s)
  (place-image
   (overlay (text (string-append
                   "("
                   (number->string (circ-vx c))
                   ","
                   (number->string(circ-vy c))
                   ")")
                  14
                  "blue")
            (circle RADIUS "outline" "blue"))
   (circ-x c)
   (circ-y c)
   s))

;; Sample output for testing purpose
(define circles-of-paused-world-initial-state
  (place-circ (world-circ1 paused-world-initial-state)
              (place-circ
               (world-circ2 paused-world-initial-state)
               EMPTY-CANVAS)))

;;TESTS for world-to-scene
(begin-for-test
  (check-equal?
    (world-to-scene paused-world-initial-state)
    circles-of-paused-world-initial-state
    "(world-to-scene paused-world-initial-state) returned incorrect image"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
       (circle-new-pos (world-circ1 w))
       (circle-new-pos (world-circ2 w))
       (world-paused? w))))


;; TESTS for world-after-tick
(begin-for-test
  (check-equal? (world-after-tick unpaused-world-initial-state)
                unpaused-world-after-1-tick
                "the world state returned isn't the one expected")
  (check-equal? (world-after-tick paused-world-initial-state)
                paused-world-after-1-tick
                "the world state returned isn't the one expected"))
                                                   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
                     (circ-vy c))]))

;; Tests for circle
(begin-for-test
  (check-equal? (circle-new-pos circle1) ;; else case for normal flow
                circle1t
                "the circle returned, isn't the one that should follow circle1")
  (check-equal? (circle-new-pos circle2) ;; else case for normal flow
                circle2t
                "the circle returned, isn't the one that should follow circle2")
  (check-equal? (circle-new-pos (make-circ 45 80 -12 20)) ;; for left boundary
                (make-circ 40 100 12 20)
                "the circle returned, isn't the one that should follow the input circle")
  (check-equal? (circle-new-pos (make-circ 355 200 12 20)) ;; for right boundary
                (make-circ 360 220 -12 20)
                "the circle returned, isn't the one that should follow the input circle")
  (check-equal? (circle-new-pos (make-circ 45 42 12 -20)) ;; for top boundary
                (make-circ 57 40 12 20)
                "the circle returned, isn't the one that should follow the input circle")
  (check-equal? (circle-new-pos (make-circ 57 255 12 20)) ;; for bottom boundary
                (make-circ 69 260 12 -20)
                "the circle returned, isn't the one that should follow the input circle")
  (check-equal? (circle-new-pos (make-circ 357 46 12 -20))  ;; for top-right corner
                (make-circ 360 40 -12 20)
                "the circle returned, isn't the one that should follow the input circle")
  (check-equal? (circle-new-pos (make-circ 357 250 12 20)) ;; for bottom-right corner
                (make-circ 360 260 -12 -20)
                "the circle returned, isn't the one that should follow the input circle")
  (check-equal? (circle-new-pos (make-circ 42 257 -12 20)) ;; for botton-left corner
                (make-circ 40 260 12 -20)
                "the circle returned, isn't the one that should follow the input circle")
  (check-equal? (circle-new-pos (make-circ 45 43 -12 -20)) ;; for top-left corner
                (make-circ 40 40 12 20)
                "the circle returned, isn't the one that should follow the input circle"))

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
;; EXAMPLES: refer the tests above
;; DESIGN STRATEGY: Using Template for Circle on c

(define (left-wall-adjustment c)
  (make-circ MIN-CENTER-X
             (+ (circ-y c) (circ-vy c))
             (* (circ-vx c) -1)
             (circ-vy c)))

(define (right-wall-adjustment c)
  (make-circ MAX-CENTER-X
             (+ (circ-y c) (circ-vy c))
             (* (circ-vx c) -1)
             (circ-vy c)))

(define (top-wall-adjustment c)
  (make-circ (+ (circ-x c) (circ-vx c))
             MIN-CENTER-Y
             (circ-vx c)
             (* (circ-vy c) -1)))
  
(define (bottom-wall-adjustment c)
  (make-circ (+ (circ-x c) (circ-vx c))
             MAX-CENTER-Y
             (circ-vx c)
             (* (circ-vy c) -1)))

(define (top-right-adjustment c)
  (make-circ MAX-CENTER-X
             MIN-CENTER-Y
             (* (circ-vx c) -1)
             (* (circ-vy c) -1)))


(define (bottom-right-adjustment c)
  (make-circ MAX-CENTER-X
             MAX-CENTER-Y
             (* (circ-vx c) -1)
             (* (circ-vy c) -1)))

(define (bottom-left-adjustment c)
  (make-circ MIN-CENTER-X
             MAX-CENTER-Y
             (* (circ-vx c) -1)
             (* (circ-vy c) -1)))

(define (top-left-adjustment c)
  (make-circ MIN-CENTER-X
             MIN-CENTER-Y
             (* (circ-vx c) -1)
             (* (circ-vy c) -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-circle : NonNegInt NonNegInt Int Int -> Circle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a circle centered at (x,y), which will travel with
;; velocity (vx, vy).
;; EXAMPLES: (new-circle 200 100 -12 20) = (make-circ 200 100 -12 20)
;; DESIGN STRATEGY: Using template for Circle

(define (new-circle x y vx vy)
  (make-circ x y vx vy))

;; TESTs for new-circle
(begin-for-test
  (check-equal? (new-circle 200 100 -12 20)
                (make-circ 200 100 -12 20)
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
;;   (make-world circle1 circle2 true)) = (make-world circle1 circle2 false)
;; (world-with-paused-toggled
;;   (make-world circle1 circle2 false)) = (make-world circle1 circle2 true)
;; DESIGN STRATEGY: Using template for WorldState on w

(define (world-with-paused-toggled w)
  (make-world
   (world-circ1 w)
   (world-circ2 w)
   (not (world-paused? w))))

;; TESTS
(begin-for-test
  (check-equal? (world-with-paused-toggled (make-world circle1 circle2 true))
                (make-world circle1 circle2 false)
                "the world wasn't paused? toggled")
  (check-equal? (world-with-paused-toggled (make-world circle1 circle2 false))
                (make-world circle1 circle2 true)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; following are a part of the world struct

;; world-circ1 : WorldState -> Circle
;; world-circ2 : WorldState -> Circle
;; world-paused? : WorldState -> Boolean
;; RETURNS: the specified attribute of the WorldState
;; NOTE: if these are part of the world struct, you don't need to
;; write any deliverables for these functions.
