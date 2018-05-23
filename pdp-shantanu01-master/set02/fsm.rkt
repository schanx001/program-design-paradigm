;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A program to illustrate the workings of a finite state machine for accepting strings.

(require rackunit)
(require "extras.rkt")
(check-location "02" "fsm.rkt")

(provide
  initial-state
  next-state
  accepting-state?
  error-state?
  ) 

;; Lists of input strings a particular state accepts.
(define q0-ip (list "q" "x" "u" "a" "b" "d")) 
(define q1-ip (list "q" "x" "u" "a" "b" "d")) 
(define q2-ip (list "u" "a" "b" "d"))         
(define q3-ip (list "a" "b" "d"))
(define q4-ip (list "e" "f"))
(define q5-ip (list "e" "f"))

;; DATA DEFINITIONS:
;; A machine input is one of:
;; -- "q"
;; -- "x"
;; -- "u"
;; -- "a"
;; -- "b"
;; -- "d"
;; -- "e"
;; -- "f"

;; A machine state is one of:
;; -- "q0"
;; -- "q1"
;; -- "q2"
;; -- "q3"
;; -- "q4"
;; -- "q5"
;; -- "q6"

;; INTERPRETATION:
;; q0 is the initial state of the machine
;; q1 is the state when machine input is [q or x] on state q0 OR q1 i.e (q|x)*
;; q2 is the state when machine input is [u] on state q0, q1 OR q2 i.e u*
;; q3 is the state when machine input is [a or b] on state q0, q1, q2 OR q3 i.e (a|b)*
;; q4 is the state when machine input is [d] on state q0, q1, q2 OR q3 i.e d
;; q5 is the state when machine input is [e or f] on state q4 i.e (e|f)*
;; q6 is the dummy state which the machine reaches to if
;; it isn't able to reach any of the other states

;; TEMPLATE:
;; fsm-fn: State MachineInput -> ??
#|(define fsm-fn mcstate mcinput
  (cond
    [(equal? mcstate "q0")...]
    [(equal? mcstate "q1")...]
    [(equal? mcstate "q2")...]
    [(equal? mcstate "q3")...]
    [(equal? mcstate "q4")...]
    [(equal? mcstate "q5")...]
    [else ...]))
|#

;; initial-state : Number -> State
;; GIVEN: a number
;; RETURNS: a representation of the initial state
;; of your machine.  The given number is ignored.
;; EXAMPLES:
;; (initial-state 91) = "q0"
;; DESIGN STRATEGY: Combine simpler functions

(define (initial-state num)
  (if(number? num)
     "q0"
     "not a number"))

;; TESTS for initial-state fn
(begin-for-test
  (check-equal? (initial-state 91) "q0" "the initial state should be 'q0'")
  (check-equal? (initial-state "a") "not a number" "the input should be a number"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; next-state : State MachineInput -> State
;; GIVEN: a state of the machine and a machine input
;; RETURNS: the state that should follow the given input.
;; EXAMPLES:
;; (next-state "q5" "d") = "q6" ;dummy state
;; (next-state "q1" "q") = "q1"
;; (next-state "q2" "a") = "q3"
;; STRATEGY: Use template for state machine on mcstate and mcinput

(define (next-state mcstate mcinput)
  (cond
    [(and (equal? mcstate "q0") (member? mcinput q0-ip)) (state-after-mip mcinput)]
    [(and (equal? mcstate "q1") (member? mcinput q1-ip)) (state-after-mip mcinput)]
    [(and (equal? mcstate "q2") (member? mcinput q2-ip)) (state-after-mip mcinput)]
    [(and (equal? mcstate "q3") (member? mcinput q3-ip)) (state-after-mip mcinput)]          
    [(and (equal? mcstate "q4") (member? mcinput q4-ip)) (state-after-mip mcinput)]
    [(and (equal? mcstate "q5") (member? mcinput q5-ip)) (state-after-mip mcinput)]
    [else "q6"]))

;; TESTS for next-state fn
(begin-for-test
  (check-equal? (next-state "q0" "q") "q1" "the next state should be 'q1'")
  (check-equal? (next-state "q2" "u") "q2" "the next state should be 'q2'")
  (check-equal? (next-state "q0" "e") "q6" "the next state should be 'q6'")
  (check-equal? (next-state "q3" "d") "q4" "the next state should be 'q4'")
  (check-equal? (next-state "q4" "d") "q6" "the next state should be 'q6'")
  (check-equal? (next-state "q4" "e") "q5" "the next state should be 'q5'")
  (check-equal? (next-state "q1" "a") "q3" "the next state should be 'q3'")
  (check-equal? (next-state "q5" "f") "q5" "the next state should be 'q5'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; error-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff there is no path (empty or non-empty) from the given
;; state to an accepting state
;; EXAMPLES:
;; (error-state? "q6") = #true
;; (error-state? "q4") = #false
;; DESIGN STRATEGY: Combine simpler functions

(define (error-state? mcstate)
  (cond [(and (equal? mcstate (next-state mcstate "d")) 
              (equal? mcstate (next-state mcstate "e"))
              (equal? mcstate (next-state mcstate "f"))) true]
        [else false]))

;; TESTS for error-state? fn
(begin-for-test
  (check-equal? (error-state? "q6") #true "it should be true")
  (check-equal? (error-state? "q4") #false "it should be false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; accepting-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true iff the given state is a final (accepting) state
;; EXAMPLES:
;; (accepting-state? "q6") = #false
;; (accepting-state? "q4") = #true
;; DESIGN STRATEGY: Combine simpler functions

(define (accepting-state? mcstate)
  (if(or (equal? mcstate "q4") (equal? mcstate "q5"))
     true
     false))

;; TESTS for accepting-state? fn
(begin-for-test
  (check-equal? (accepting-state? "q6") #false "it should be false")
  (check-equal? (accepting-state? "q4") #true "it should be true")
  (check-equal? (accepting-state? "q3") #false "it should be false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; state-after-mip: McInput -> McState
;; GIVEN: a machine input
;; RETURNS: the machine state to which it reaches when given the machine input
;; EXAMPLES:
;; (state-after-mip "d") = "q4"
;; (state-after-mip "q") = "q1"
;; DESIGN STRATEGY: Cases on machine input.

(define (state-after-mip mip)
  (cond
    [(equal? mip "q") "q1"]
    [(equal? mip "x") "q1"]
    [(equal? mip "u") "q2"]
    [(equal? mip "a") "q3"]
    [(equal? mip "b") "q3"]
    [(equal? mip "d") "q4"]
    [(equal? mip "e") "q5"]
    [(equal? mip "f") "q5"]))

;; TESTS for state-after-mip fn
(begin-for-test
  (check-equal? (state-after-mip "e") "q5" "it should be 'q5'")
  (check-equal? (state-after-mip "d") "q4" "it should be 'q4'")
  (check-equal? (state-after-mip "b") "q3" "it should be 'q3'")
  (check-equal? (state-after-mip "x") "q1" "it should be 'q1'"))


