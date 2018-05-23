;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname probe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A program to show the workings of a space probe that landed on Pluto.

(require rackunit)
(require "extras.rkt")
(check-location "02" "probe.rkt")

(provide probe-at
         probe-turned-left
         probe-turned-right
         probe-direction-equal?
         probe-location-equal?
         probe-forward-possible-outcome?)

;; DATA DEFINITIONS:

(define-struct probe (x y direction))

;; A space probe is a:
;; (make-probe Integer Integer String)
;; Interpretation:
;; x is the x coordinate of the probe's location on the graphics-style coordinates
;; y is the y coordinate of the probe's location on the graphics-style coordinates
;;   (i.e if probe moves north, its y-coordinate decreases)
;; direction is where the probe is facing and can have
;;           one of the values : "north" "south" "east" "west"

;; TEMPLATE:
;; probe-fn : Probe -> ??
#|
(define (probe-fn p)
  (...
    (probe-x p)
    (probe-y p)
    (probe-direction p)))
|# 

;; probe-at: Integer Integer -> Probe
;; GIVEN: an x-coordinate and a y-coordinate
;; RETURNS: a probe with its center at those coordinates, facing north.
;; EXAMPLES:
;; (probe-at 10 10) = (make-probe 10 10 "north")
;; DESIGN STRATEGY: Combine simpler functions

(define (probe-at x y)
  (if(and (integer? x) (integer? y))
  (make-probe x y "north")
  "not an integer"
  ))


;; TEST CASE for probe-at fn
(begin-for-test
  (check-equal? (probe-at 10 10) (make-probe 10 10 "north")
                "the probe should be in (make-probe 10 10 'north')"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; probe-turned-left : Probe -> Probe
;; GIVEN: a probe
;; RETURNS: a probe like the original, but turned 90 degrees left.
;; EXAMPLES:
;; (probe-turned-left (make-probe 10 10 "north")) = (make-probe 10 10 "west")
;; DESIGN STRATEGY: Cases on probe's direction for turning left

(define (probe-turned-left probe-n)
  (cond
  [(equal? (probe-direction probe-n) "north")
   (make-probe (probe-x probe-n) (probe-y probe-n) "west")]
  [(equal? (probe-direction probe-n) "west")
   (make-probe (probe-x probe-n) (probe-y probe-n) "south")]
  [(equal? (probe-direction probe-n) "south")
   (make-probe (probe-x probe-n) (probe-y probe-n) "east")]
  [(equal? (probe-direction probe-n) "east")
   (make-probe (probe-x probe-n) (probe-y probe-n) "north")]
  [else "not a valid input"]))

;; TEST CASE for probe-turned-left fn
(begin-for-test
  (check-equal? (probe-turned-left (make-probe 10 10 "north")) (make-probe 10 10 "west")
                "the direction should be west")
  
  (check-equal? (probe-turned-left (make-probe 10 10 "south")) (make-probe 10 10 "east")
                "the direction should be east")
  
  (check-equal? (probe-turned-left (make-probe 10 10 "east")) (make-probe 10 10 "north")
                "the direction should be north")
  
  (check-equal? (probe-turned-left (make-probe 10 10 "west")) (make-probe 10 10 "south")
                "the direction should be south"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; probe-turned-right : Probe -> Probe
;; GIVEN: a probe
;; RETURNS: a probe like the original, but turned 90 degrees right.
;; EXAMPLES:
;; (probe-turned-right (make-probe 10 10 "north")) = (make-probe 10 10 "east")
;; DESIGN STRATEGY: Cases on probe's direction for turning right
  
(define (probe-turned-right probe-n)
  (cond
  [(equal? (probe-direction probe-n) "north")
   (make-probe (probe-x probe-n) (probe-y probe-n) "east")]

  [(equal? (probe-direction probe-n) "west")
   (make-probe (probe-x probe-n) (probe-y probe-n) "north")]

  [(equal? (probe-direction probe-n) "south")
   (make-probe (probe-x probe-n) (probe-y probe-n) "west")]

  [(equal? (probe-direction probe-n) "east")
   (make-probe (probe-x probe-n) (probe-y probe-n) "south")]

  [else "not a valid input"]))

;; TEST CASE for probe-turned-right fn
(begin-for-test
  (check-equal? (probe-turned-right (make-probe 10 10 "north")) (make-probe 10 10 "east")
                "the direction should be east")

  (check-equal? (probe-turned-right (make-probe 10 10 "east")) (make-probe 10 10 "south")
                "the direction should be south")

  (check-equal? (probe-turned-right (make-probe 10 10 "west")) (make-probe 10 10 "north")
                "the direction should be north")

  (check-equal? (probe-turned-right (make-probe 10 10 "south")) (make-probe 10 10 "west")
                "the direction should be west"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; probe-direction-equal? : Probe Probe -> Boolean
;; GIVEN: two probes
;; RETURNS: true iff the two probes are facing in the same direction,
;; else false
;; EXAMPLES:
;; (probe-direction-equal? (make-probe 12 13 "south") (make-probe -12 1 "east")) = false
;; (probe-direction-equal? (make-probe 12 13 "east") (make-probe -12 1 "east")) = true
;; DESIGN STRATEGY: Combine simpler functions

(define (probe-direction-equal? probe-1 probe-2)
  (if(equal? (probe-direction probe-1) (probe-direction probe-2))
     true
     false))

;; TESTS for probe-direction-equal? fn
(begin-for-test
  (check-equal? (probe-direction-equal? (make-probe 12 13 "south")
                                        (make-probe -12 1 "east"))
                #false
                "the directions shouldn't be equal")
  (check-equal? (probe-direction-equal? (make-probe 12 13 "east")
                                        (make-probe -12 1 "east"))
                #true
                "the directions shouldn't be equal"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; probe-location-equal? : Probe Probe -> Boolean
;; GIVEN: two probles
;; RETURNS: true iff the two probes are at the same location
;; EXAMPLES: 
;; (probe-location-equal? (make-probe 12 13 "west") (make-probe 12 13 "south")) = true
;; DESIGN STRATEGY: Combine simpler functions
 
(define (probe-location-equal? probe-1 probe-2)
  (if(and(equal? (probe-x probe-1) (probe-x probe-2))
         (equal? (probe-y probe-1) (probe-y probe-2)))
     true
     false))

;; TESTS for probe-location-equal? fn
(begin-for-test
  (check-equal? (probe-location-equal? (make-probe 12 13 "west")
                                       (make-probe 12 13 "south")
                        ) #true "it should be true")
  (check-equal? (probe-location-equal? (make-probe 11 13 "west")
                                       (make-probe 12 13 "south")
                        ) #false "it should be false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; move-forward-probe : Probe PosInt -> Probe
;; GIVEN: a probe and a distance 
;; RETURNS: a probe like the original, moved forward by steps/distance given
;; EXAMPLES:
;; (move-forward-probe (make-probe -13 -13 "north") 7) = (make-probe -13 -20 "north")
;; (move-forward-probe (make-probe 11 17 "south") 7) = (make-probe 11 23 "south")
;; (move-forward-probe (make-probe -13 -13 "west") 7) = (make-probe -20 -13 "west")
;; DESIGN STRATEGY: Cases on probe direction.

(define (move-forward-probe probe-n steps)
  (cond
  [(equal? (probe-direction probe-n) "north")
   (make-probe (probe-x probe-n)(- (probe-y probe-n) steps) "north")]
  [(equal? (probe-direction probe-n) "west")
   (make-probe (- (probe-x probe-n) steps) (probe-y probe-n) "west")]
  [(equal? (probe-direction probe-n) "south")
   (make-probe (probe-x probe-n)(+ (probe-y probe-n) steps) "south")]
  [(equal? (probe-direction probe-n) "east")
   (make-probe (+ (probe-x probe-n) steps)(probe-y probe-n) "east")])
  )

;; TESTS for move-forward-probe fn
(begin-for-test
  (check-equal? (move-forward-probe (make-probe -13 -13 "west") 7)
                (make-probe -20 -13 "west") "x should decrease")
  (check-equal? (move-forward-probe (make-probe -13 -13 "north") 7)
                (make-probe -13 -20 "north") "y should decrease")
  (check-equal? (move-forward-probe (make-probe 11 17 "south") 7)
                (make-probe 11 24 "south") "y should increase")
  (check-equal? (move-forward-probe (make-probe 11 24 "east") 7)
                (make-probe 18 24 "east") "x should increase"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; x-equal? : Probe PosInt Probe -> Boolean
;; GIVEN: a probe1 , distance and probe2
;; RETURNS: true iff the probe1's x coordinate is within +1 and -1 of the
;;          probe2's x coordinate after evaluating the distance (west/east),
;;          else returns false
;; EXAMPLES:
;; (x-equal? (make-probe 5 17 "east") 7 (make-probe 11 24 "east")) = true 
;; DESIGN STRATEGY: Combine simpler functions

(define (x-equal? p1 dist p2)
  (if(and (>=(probe-x (move-forward-probe p1 dist))
              (- (probe-x p2) 1))
          (<= (probe-x (move-forward-probe p1 dist))
              (+ (probe-x p2) 1)))
  true
  false))

;; TESTS for x-equal? fn
(begin-for-test
  (check-equal? (x-equal? (make-probe 5 17 "east") 7 (make-probe 11 24 "east")) #true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; y-equal?: Probe PosInt Probe -> Boolean
;; GIVEN: a probe1 , distance and probe2
;; RETURN: true iff the probe1's y coordinate is within +1 and -1 of the
;;              probe2's y coordinate after evaluating the distance (north/south),
;;         else returns false
;; EXAMPLES:
;; (y-equal? (make-probe 11 17 "north") 7 (make-probe 11 24 "north")) = true 
;; DESIGN STRATEGY: Combine simpler functions

(define (y-equal? p1 dist p2)
  (if(and (>= (probe-y (move-forward-probe p1 dist))
              (- (probe-y p2) 1))
          (<= (probe-y (move-forward-probe p1 dist))
              (+ (probe-y p2) 1)))
  true
  false))

;; TESTS for y-equal? fn
(begin-for-test
  (check-equal? (y-equal? (make-probe 5 17 "south") 7 (make-probe 11 24 "south")) #true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; probe-forward-possible-outcome? : Probe PosInt Probe -> Boolean
;; GIVEN: two probes and a distance
;; RETURNS: true iff the first probe, given a move-forward command with
;; the specified number of steps, could wind up in the state described by
;; the second probe.
;; EXAMPLES:
;; (probe-forward-possible-outcome? (make-probe -13 -13 "north")
;;                                  5
;;                                  (make-probe -13 -20 "north")) = false
;; (probe-forward-possible-outcome? (make-probe -13 -13 "north")
;;                                  6
;;                                  (make-probe -13 -20 "north")) = true
;; (probe-forward-possible-outcome? (make-probe -13 -13 "north")
;;                                  7
;;                                  (make-probe -13 -20 "north")) = true
;; (probe-forward-possible-outcome? (make-probe -13 -13 "north")
;;                                  8
;;                                  (make-probe -13 -20 "north")) = true
;; DESIGN STRATEGY: Cases on probe direction

(define (probe-forward-possible-outcome? probe-1 distance probe-2)
  (cond
    [(or (equal? (probe-direction probe-2) "west")
         (equal? (probe-direction probe-2) "east"))
     (if (and (x-equal? probe-1 distance probe-2)
              (y-equal? probe-1 distance probe-2)
              (probe-direction-equal? probe-1 probe-2))
        true
        false)]
    [(or (equal? (probe-direction probe-2) "north")
         (equal? (probe-direction probe-2) "south"))
     (if (and (x-equal? probe-1 distance probe-2)
              (y-equal? probe-1 distance probe-2)
              (probe-direction-equal? probe-1 probe-2))
        true
        false) ]
    ))

;; TESTS for probe-forward-possible-outcome? fn
(begin-for-test
;; north direction: for distance 7 (8 and 6 as unreliable steps)
  (check-equal? (probe-forward-possible-outcome? (make-probe -13 -13 "north")
                                                  7
                                                 (make-probe -13 -20 "north"))
                #true)
  (check-equal? (probe-forward-possible-outcome? (make-probe -13 -13 "north")
                                                  8
                                                 (make-probe -13 -20 "north"))
                #true)
  (check-equal? (probe-forward-possible-outcome? (make-probe -13 -13 "north")
                                                  6
                                                 (make-probe -13 -20 "north"))
                #true)
;; 5 as distance added to probe1 and then compared to probe2
;; is more than +1 and -1 of the probe2 coordinate
;; (as its not within -19 to -21 for y coordinate of probe2) 

  (check-equal? (probe-forward-possible-outcome? (make-probe -13 -13 "north")
                                                  5
                                                 (make-probe -13 -20 "north"))
                #false)

;; for south direction
  
  (check-equal? (probe-forward-possible-outcome? (make-probe -13 -13 "south")
                                                  7
                                                 (make-probe -13 -6 "south"))
                #true)

  ;; for west/east direction x coordinate is evaluated for probe1 and compared
 
  (check-equal? (probe-forward-possible-outcome? (make-probe -13 -13 "west")
                                                 2
                                                 (make-probe -16 -20 "west"))
                #false)

  (check-equal? (probe-forward-possible-outcome? (make-probe 9 -13 "east")
                                                 2
                                                 (make-probe 11 -13 "east"))
                #true)

