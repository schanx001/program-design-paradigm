;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; q2.rkt
;; PURPOSE: Create a programmable probe

;; Probe
(require "extras.rkt")
(require rackunit)
(check-location "07" "q2.rkt")

(provide probe-possible-outcome?
         make-turn-left
         make-turn-right
         make-move-forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;A Program is a ListOfInstruction
;Interp: A sequence of instructions, to be executed from left to
;right. 

;; ListOfInstuction is one of:
;; --empty
;; --(cons Instruction LOI)

;; template:

;;; loi-fn: LOI -> ??
;(define (loi-fn loi)
;  (cond
;    [(empty? loi)...]
;    [else (...(first loi)
;              (loi-fn (rest loi)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct turn-left ())

;; A TurnLeft is a (make-turn-left)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct turn-right())

;; A TurnRight is a (make-turn-right)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct move-forward (dist))

;; A MoveForward is (make-move-forward PosInt)
;; dist is the steps/distance to move the probe forward

;; Template:
;; moveforward-fn: MoveForward -> ??
;; (define (moveforward-fn m)
;;  (...(move-forward-dist m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;An Instruction is one of
;-- (make-turn-left)            Interp: a turn-left instruction
;-- (make-turn-right)           Interp: a turn-right instruction
;-- (make-move-forward PosInt)  Interp: an instruction to move forward
;                                       the given number of steps.

;; template:
;; instr-fn: Instruction -> ??
;(define (instr-fn i)
;  (cond
;    [(turn-left? i)...]
;    [(turn-right? i)...]
;    [(move-forward? i)...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct probe(direction x-min x-max y-min y-max))

;; A Probe is a (make-probe Integer Integer Number Integer Integer Integer Integer)

;; Interpretation:

;; direction is the probe's facing direction 
;; x-min is the minimum x range value of the probe (i.e for west and east direction)
;; x-max is the maximum x range value of the probe (i.e for west and east direction)
;; y-min is the minimum y range value of the probe (i.e for north and south direction)
;; y-max is the maximum y range value of the probe (i.e for north and south direction)

;; Template:
;; probe-fn: Probe -> ??
;(define (probe-fn p)
;  (...
;   (probe-direction p)
;   (probe-xmin p)
;   (probe-xmax p)
;   (probe-ymin p)
;   (probe-ymax p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Direction is one of:
;; -- N INTERP: North
;; -- E INTERP: East
;; -- S INTERP: South
;; -- W INTERP: West

;; template: 

;; dir-fn: Direction -> ??
;(define (dir-fn d)
;  (cond
;    [(= d N)...]
;    [(= d S)...]
;    [(= d W)...]
;    [(= d E)...]))

(define N 0)
(define E 1)
(define S 2)
(define W 3)
(define TOTALD 4)
(define LEFTDIR 3)
(define RIGHTDIR 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; probe-possible-outcome? : Int Int Program Int Int -> Bool
;; GIVEN: starting coordinates x0, y0, a robot program p, and ending
;;        coordinates x1, y1.
;; RETURNS: true iff the robot, starting at (x0, y0) and facing north,
;;          and executing program p according to the tolerances given above,
;;          could end at (x1, y1).
;; EXAMPLES:
;; Let p1 = (list (make-turn-right)
;;                (make-move-forward 10)
;;                (make-turn-right)
;;                (make-move-forward 5))
;; then (probe-possible-outcome 20 100 p1 28 103) = true since
;; x1 is in the interval [28, 32] and y1 is in the interval [103,107].
;; STRATEGY: Cases on Program being empty

(define (probe-possible-outcome? x y program xfinal yfinal)
  (cond
    [(empty? program) (check-probe? x y xfinal yfinal)]
    [else (check-probe-in-range (get-evaluated-probe (make-probe N x x y y)
                                                     program)
                                xfinal
                                yfinal)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-probe-in-range : Probe Int Int -> Boolean
;; GIVEN: A Probe and the x and y coordinates of the probe
;; RETURNS: true iff x and y coordinate of the probe are within their minimum
;;          and maximum limits
;; EXAMPLES:
;; (check-probe-in-range (make-probe 24 100 0 14 24 98 102) 24 100) -> true
;; STRATEGY: Use template for Probe on p

(define (check-probe-in-range p x y)
  (and (<= (probe-x-min p) x (probe-x-max p))
       (<= (probe-y-min p) y (probe-y-max p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-probe? : Int Int Int Int -> Boolean
;; GIVEN: The initial and final x and y coordinates of the probe
;; RETURNS: true iff the initial and final coordinates of the probe are equal
;; EXAMPLES:
;; (check-probe? 20 100 20 100) -> true
;; STRATEGY: Combine simpler functions

(define (check-probe? x1 y1 x2 y2)
  (and (= x1 x2) (= y1 y2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-evaluated-probe : Probe Program -> Probe
;; GIVEN: A Probe and a Program to execute the list of instructions on the probe
;; RETURNS: The Probe after executing the given set of instructions on it
;; EXAMPLES: (get-evaluated-probe (make-probe 20 100 0 28 32 95 105) p1) ->
;;   (make-probe 36 98 2 36 44 95 112)
;; STRATEGY: Cases on Program being empty
;; HALTING-MEASURE: (length Program)

(define (get-evaluated-probe p program)
  (cond
    [(empty? program) p]
    [else (process-instruction p (first program) (rest program))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; process-instruction : Probe Instruction ListOfInstruction -> Probe
;; GIVEN: A Probe, an instruction and a list of instructions
;; RETURNS: The state of the probe after executing the instruction
;; EXAMPLES:
;; (process-instruction (make-probe 20 100 0 28 32 95 105)
;;   (make-turn-right) (list (make-turn-right) (make-move-forward 10))) ->
;; (make-probe 20 103 2 28 32 103 117)
;; STRATEGY: Use template for Instruction on instr
;; HALTING-MEASURE: (length Program)

(define (process-instruction p instr loi)
  (cond
   [(turn-left? instr) (get-evaluated-probe (change-direction p LEFTDIR) loi)]
   [(turn-right? instr) (get-evaluated-probe (change-direction p RIGHTDIR) loi)]
   [(move-forward? instr) (get-evaluated-probe
                            (probe-move-forward p (move-forward-dist instr))
                            loi)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; probe-move-forward : Probe PosInt -> Probe
;; GIVEN: A Probe and distance(steps) by which the probe is to be moved forward
;; RETURNS: The probe after moving it forward by the specified number of steps
;; EXAMPLES: (probe-move-forward (make-probe 20 100 0 28 32 95 105) 10) ->
;;            (make-probe 20 97 0 28 32 83 97)
;; STRATEGY: Cases on Direction of the probe

(define (probe-move-forward p dist)
  (cond
    [(= N (probe-direction p)) (get-north-probe p dist)]
    [(= E (probe-direction p)) (get-east-probe p dist)]
    [(= W (probe-direction p)) (get-west-probe p dist)]
    [(= S (probe-direction p)) (get-south-probe p dist)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-north-probe: Probe PosInt -> Probe
;; GIVEN: A Probe and distance(steps) by which to move the probe in the north
;;        direction
;; WHERE: the probe p0 is facing north  
;; RETURNS: The probe after moving it with the specified number of steps north
;; EXAMPLES: (get-north-probe (make-probe 20 100 0 28 32 95 105) 10) ->
;;            (make-probe 20 97 0 28 32 83 97)
;; STRATEGY: Using template for Probe on p

(define (get-north-probe p dist)
  (make-probe (probe-direction p)
              (probe-x-min p)
              (probe-x-max p)
              (- (probe-y-min p) (+ dist 2))
              (calculate-range-nw-dir (probe-y-max p) dist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-west-probe: Probe PosInt -> Probe
;; GIVEN: A Probe and distance(steps) by which to move the probe in the west
;;        direction
;; WHERE: the probe p0 is facing west
;; RETURNS: The probe after moving it with the specified number of steps west
;; EXAMPLES: (get-west-probe (make-probe 20 100 0 28 32 95 105) 10) ->
;;            (make-probe 24 100 0 16 32 95 105)
;; STRATEGY: Using template for Probe on p

(define (get-west-probe p dist)
  (make-probe (probe-direction p)
              (- (probe-x-min p) (+ dist 2))
              (calculate-range-nw-dir (probe-x-max p) dist)
              (probe-y-min p)
              (probe-y-max p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-east-probe: Probe PosInt -> Probe
;; GIVEN: A Probe and distance(steps) by which to move the probe in the east
;;        direction
;; WHERE: the probe p0 is facing east 
;; RETURNS: The probe after moving it with the specified number of steps east
;; EXAMPLES: (get-east-probe (make-probe 20 100 1 28 32 95 105) 10)
;;            (make-probe 36 100 1 36 44 95 105)
;; STRATEGY: Using template for Probe on p

(define (get-east-probe p dist)
  (make-probe (probe-direction p)
              (calculate-range-se-dir (probe-x-min p) dist)
              (+ (probe-x-max p) (+ dist 2))
              (probe-y-min p)
              (probe-y-max p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-south-probe: Probe PosInt -> Probe
;; GIVEN: A Probe and distance(steps) by which to move the probe in the south
;;        direction
;; WHERE: the probe p0 is facing south  
;; RETURNS: The probe after moving it with the specified number of steps south
;; EXAMPLES: (get-south-probe (make-probe 20 100 2 28 32 95 105) 10)
;;            (make-probe 20 103 2 28 32 103 117)
;; STRATEGY: Using template for Probe on p

(define (get-south-probe p dist)
  (make-probe (probe-direction p)
              (probe-x-min p)
              (probe-x-max p)
              (calculate-range-se-dir (probe-y-min p) dist)
              (+ (probe-y-max p) (+ dist 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; calculate-range-se-dir: Integer PosInt
;; GIVEN: a minimum range value and a distance
;; WHERE: min range value is previous min value
;; RETURNS: a calculated range value for minimum of range
;; STRATEGY: Combine using simpler functions

(define (calculate-range-se-dir min dist)
  (if (< min (+ min (- dist 2)))
      (+ min (- dist 2))
      min))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; calculate-range-nw-dir: Integer PosInt
;; GIVEN: a maximum range value and a distance
;; WHERE: max range value is previous max value
;; RETURNS: a calculated range value for maximum of range
;; STRATEGY: Combine using simpler functions

(define (calculate-range-nw-dir max dist)
  (if (> max (- max (- dist 2)))
      (- max (- dist 2))
      max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; change-direction : Probe Number -> Probe
;; GIVEN: A Probe which is to be turned left or right by 90 degrees and a string
;;        indicating whether the probe is to be turned left or right
;; RETURNS: A Probe after turning it left or right by 90 degrees
;; EXAMPLES: (change-direction (make-probe 20 100 0 28 32 95 105) LEFTDIR) ->
;;            (make-probe 20 100 3 28 32 95 105)
;;           (change-direction (make-probe 20 100 0 28 32 95 105) RIGHTDIR) ->
;;            (make-probe 20 100 1 28 32 95 105)
;; STRATEGY: Using template for Probe on p


(define (change-direction p direction)
   (make-probe (remainder (+ (probe-direction p) direction) TOTALD)
               (probe-x-min p)
               (probe-x-max p)
               (probe-y-min p)
               (probe-y-max p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-turn-right : -> Instruction
;; make-turn-left  : -> Instruction
;; make-move-forward : PosInt -> Instruction

;;;; scalable-stress-program : Int -> Program
;;;; Given: an integer n
;;;; Returns: a Program of length (* 6 (max 0 n))
;
;(define (scalable-stress-program n)
;  (if (> n 0)
;      (append (list (make-turn-right)
;                    (make-move-forward 10)
;                    (make-turn-right)
;                    (make-move-forward 10)
;                    (make-turn-left)
;                    (make-turn-left))
;              (scalable-stress-program (- n 1)))
;      empty))
;
;;;; stress-benchmark : Int -> Boolean
;;;; Given: an integer, preferably non-negative, stating the desired
;;;;     degree of difficulty for the benchmark
;;;; Returns: true
;;;; Effect: reports how much time probe-possible-outcome?
;;;;     takes to compute the correct answer
;
;(define (stress-benchmark n)
;  (let ((pgm (scalable-stress-program n)))
;    (time (probe-possible-outcome? 0 0 pgm (* 10 n) (* 10 n)))))


(define p1 (list (make-turn-right)
                (make-move-forward 10)
                (make-turn-right)
                (make-move-forward 5)))

(define p2 (list (make-turn-right)
                (make-move-forward 2)
                (make-turn-right)
                (make-move-forward 2)
                (make-turn-right)
                (make-move-forward 2)
                (make-turn-right)
                (make-move-forward 2)
                (make-turn-left)))

(define p3 (list (make-turn-right)
                (make-turn-right)
                (make-turn-right)
                (make-turn-right)
                (make-move-forward 2)
                (make-turn-left)))

(begin-for-test
  (check-equal? (probe-possible-outcome? 20 100 p1 28 103)
                true
                "function should return true")

  (check-equal? (probe-possible-outcome? 20 100 p2 28 108)
                false
                "function should return false")
  (check-equal? (probe-possible-outcome? 20 100 p3 20 98)
                true
                "function should return true")
  (check-equal? (check-probe? 20 100 20 100)
                true
                "function should return true")

  (check-equal? (probe-possible-outcome? 20 100 empty 20 100)
                true
                "function should return true")
   (check-equal? (get-west-probe (make-probe 1 18 20 95 105) 10)
                (make-probe 1 6 12 95 105)
                "function should return probe at 12,100")
   (check-equal? (get-north-probe (make-probe 0 6 12 95 100) 10)
                 (make-probe 0 6 12 83 92)
                 "function should return probe at 12,92"))