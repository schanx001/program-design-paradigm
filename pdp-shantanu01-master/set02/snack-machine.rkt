;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snack-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A program to show the workings of a health-food machine
;; in a university.

(require rackunit)
(require "extras.rkt")
(check-location "02" "snack-machine.rkt")
(provide initial-machine
         machine-next-state
         machine-output
         machine-remaining-kale
         machine-remaining-carrots
         machine-bank)

;;==================================================================================

;; DATA DEFINITIONS:
;; A CustomerInput is one of
;; -- a PosInt        interp: insert the specified number of quarters
;; -- "kale"          interp: request a bag of kale chips
;; -- "carrots"       interp: request a bag of carrots
;; -- "change"        interp: return all the unspent money that the
;;                             customer has inserted

;; A MachineOutput is one of
;; -- "kale"           interp: machine dispenses a bag of kale chips
;; -- "carrots"        interp: machine dispenses a bag of carrot sticks
;; -- "Out of Item"    interp: machine displays "Out of Item"
;; -- a PosInt         interp: machine releases the specified number of quarters
;; -- "Nothing"        interp: the machine does nothing

(define-struct machine-state (kale carrots custcredit bankcontainer))

;; A machine-state is a (make-machine-state NonNegInt NonNegInt NonNegInt NonNegInt)
;; kale is the number of bags of kale chips
;; carrots is the number of bags of carrot sticks
;; custcredit is the number of quarters the customer inserted (1 quarter = 25 cents)
;; bankcontainer is the container that contains all the money it has kept from
;;               customers' purchases in the form of number of quarters
 

;; template
;; mstate-fn : MachineState -> ??
(define (mstate-fn ms)
  (...(machine-state-kale ms)
      (machine-state-carrots ms)
      (machine-state-custcredit ms)
      (machine-state-bankcontainer ms)))

;; initial-machine : NonNegInt NonNegInt -> MachineState
;; GIVEN: a number of bags of kale chips and carrot sticks
;; RETURNS: the state of a machine loaded with the given numbers of bags
;; of kale chips and carrot sticks, with an empty bank.
;; EXAMPLES:
;; (initial-machine 10 10) = (make-machine-state 10 10 0 0)
;; DESIGN STRATEGY: Combine simpler functions

(define (initial-machine nkale ncarrot)
  (make-machine-state nkale ncarrot 0 0))

;; TESTS for initial-machine fn
(begin-for-test
  (check-equal? (initial-machine 10 10) (make-machine-state 10 10 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; machine-next-state : MachineState CustomerInput -> MachineState
;; GIVEN: a machine state and a customer input
;; RETURNS: the state of the machine that should follow the customer's
;; input
;; EXAMPLES:
;; (machine-next-state (make-machine-state 10 10 3 0) "kale") = (make-machine-state 9 10 0 3)
;; DESIGN STRATEGY: Cases on Customer input

(define (machine-next-state ms custip)
  (cond
    [(and (integer? custip) (> custip 0))
             (make-machine-state (machine-state-kale ms)
                                 (machine-state-carrots ms)
                                  custip
                                 (machine-state-bankcontainer ms))]
    [(equal? custip "kale")    (state-on-kale ms)]
    [(equal? custip "carrots") (state-on-carrots ms)]
    [(equal? custip "change")  (state-on-change ms)]
    [else "not a valid input"]))

;; TESTS for machine-next-state fn:
(begin-for-test
;; cust input "kale" 
  (check-equal? (machine-next-state (make-machine-state 10 10 3 0) "kale")
                (make-machine-state 9 10 0 3))
;; cust input "kale"
  (check-equal? (machine-next-state (make-machine-state 9 10 0 3) "kale")
                (make-machine-state 9 10 0 3))
;; cust input 4
  (check-equal? (machine-next-state (make-machine-state 9 10 0 3) 4)
                (make-machine-state 9 10 4 3))
;; cust input "carrots"
  (check-equal? (machine-next-state (make-machine-state 9 10 4 3) "carrots")
                (make-machine-state 9 9 2 5))
;; cust input "change"
  (check-equal? (machine-next-state (make-machine-state 9 9 2 5) "change")
                (make-machine-state 9 9 0 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; machine-remaining-kale : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of bags of kale chips left in the machine
;; EXAMPLES:
;; (machine-remaining-kale (make-machine-state 9 10 2 5)) = 9
;; DESIGN STRATEGY: Used template for machine state 

(define (machine-remaining-kale ms)
  (machine-state-kale ms))

;; TESTS for machine-remaining-kale fn
(begin-for-test
  (check-equal? (machine-remaining-kale (make-machine-state 9 10 2 5)) 9))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; machine-remaining-carrots : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of bags of carrots left in the machine
;; EXAMPLES:
;; (machine-remaining-carrots (make-machine-state 9 10 2 5)) = 10
;; DESIGN STRATEGY: Used template for machine state 

(define (machine-remaining-carrots ms)
  (machine-state-carrots ms))

;; TESTS for machine-remaining-carrots fn
(begin-for-test
  (check-equal? (machine-remaining-carrots (make-machine-state 9 10 2 5)) 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; machine-bank : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the amount of money in the machine's bank, in cents
;; EXAMPLES:
;; (machine-bank (make-machine-state 9 10 2 5)) = 125
;; DESIGN STRATEGY: Used template for machine state

(define (machine-bank ms)
  (* (machine-state-bankcontainer ms) 25))

;; TESTS for machine-bank fn
(begin-for-test
  (check-equal? (machine-bank (make-machine-state 9 10 2 5)) 125))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; state-on-kale : MachineState -> MachineState
;; GIVEN: a machine state 
;; WHERE: customer input is "kale"
;; RETURNS: a machine state which is based on the customer input
;; EXAMPLES:
;; (state-on-kale (make-machine-state 9 10 3 5)) = (make-machine-state 8 10 0 8)
;; DESIGN STRATEGY: Used template for machine state

(define (state-on-kale ms)
  (if(> (machine-remaining-kale ms) 0)
     (if(>= (machine-state-custcredit ms) 3)
        (make-machine-state
         (-(machine-state-kale ms) 1)
         (machine-state-carrots ms)
         (- (machine-state-custcredit ms) 3)
         (+ (machine-state-bankcontainer ms) 3))
        ms)
     ms))

;; TESTS for state-on-kale fn
(begin-for-test
  (check-equal? (state-on-kale (make-machine-state 9 10 3 5)) (make-machine-state 8 10 0 8))
;; input "kale" when custcredit is 0
  (check-equal? (state-on-kale (make-machine-state 8 10 0 8)) (make-machine-state 8 10 0 8))
;; input "kale" when kale qty is 0
  (check-equal? (state-on-kale (make-machine-state 0 10 0 8)) (make-machine-state 0 10 0 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; state-on-carrots : MachineState -> MachineState
;; GIVEN: a machine state 
;; WHERE: customer input is "carrots"
;; RETURNS: a machine state which is based on the customer input
;; EXAMPLES:
;; DESIGN STRATEGY: Used template for machine state

(define (state-on-carrots ms)
  (if(> (machine-remaining-carrots ms) 0)
     (if(>= (machine-state-custcredit ms) 2)
     (make-machine-state
      (machine-state-kale ms)
      (- (machine-state-carrots ms) 1)
      (- (machine-state-custcredit ms) 2)
      (+ (machine-state-bankcontainer ms) 2))
     ms)
     ms))

;; TESTS for state-on-carrots fn
(begin-for-test
  (check-equal? (state-on-carrots (make-machine-state 9 10 3 5)) (make-machine-state 9 9 1 7))
;; input "carrot" when custcredit is 0
  (check-equal? (state-on-carrots (make-machine-state 8 9 0 8)) (make-machine-state 8 9 0 8))
;; input "carrot" when kale qty is 0
  (check-equal? (state-on-carrots (make-machine-state 0 0 0 8)) (make-machine-state 0 0 0 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; state-on-change : MachineState -> MachineState
;; GIVEN: a machine state 
;; WHERE: customer input is "change"
;; RETURNS: a machine state which is based on the customer input
;; EXAMPLES:
;; (state-on-change (make-machine-state 9 10 3 5)) = (make-machine-state 9 10 0 5)
;; DESIGN STRATEGY: Used template for machine state

(define (state-on-change ms)
  (if(>(machine-state-custcredit ms) 0)
     (make-machine-state
      (machine-state-kale ms)
      (machine-state-carrots ms)
      (- (machine-state-custcredit ms) (machine-state-custcredit ms))
      (machine-state-bankcontainer ms)
      )
     ms))

;; TESTS for state-on-change fn
(begin-for-test
;; input "change" when custcredit is 0
  (check-equal? (state-on-change (make-machine-state 9 10 3 5)) (make-machine-state 9 10 0 5))
;; input "change" when kale qty is 0
  (check-equal? (state-on-change (make-machine-state 8 9 0 8)) (make-machine-state 8 9 0 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; machine-output : MachineState CustomerInput -> MachineOutput
;; GIVEN: a machine state and a customer input
;; RETURNS: a MachineOutput that describes the machine's response to the
;; customer input
;; EXAMPLES:
;; (machine-output (make-machine-state 10 10 3 0) "kale") = "kale"
;; (machine-output (make-machine-state 0 10 3 0) "kale") = "Out of Item"
;; DESIGN STRATEGY: Cases on customer input

(define (machine-output ms custip)
  (cond
    [(equal? custip "change")
     (if(> (machine-state-custcredit ms) 0)
        (machine-state-custcredit ms)
        "Nothing")]
    [(equal? custip "kale")
     (if(> (machine-state-kale ms) 0)
        (if(>= (machine-state-custcredit ms) 3)
           "kale"
           "Nothing")
        "Out of Item")]
    [(equal? custip "carrots")
     (if(> (machine-state-carrots ms) 0)
        (if(>= (machine-state-custcredit ms) 2)
           "carrots"
           "Nothing")
        "Out of Item")]
    [(and (integer? custip) (> custip 0)) "Nothing"]
    [else "not a valid input"]))

;; TESTS for machine-output fn
(begin-for-test
  ;; input "kale"
  (check-equal? (machine-output (make-machine-state 10 10 3 0) "kale") "kale")
  ;; input "carrots"
  (check-equal? (machine-output (make-machine-state 10 10 3 0) "carrots") "carrots")
  ;; input "kale" when qty 0
  (check-equal? (machine-output (make-machine-state 0 10 3 0) "kale") "Out of Item")
  ;; input "carrots" when qty 0
  (check-equal? (machine-output (make-machine-state 10 0 3 0) "carrots") "Out of Item")
  ;; input "kale" when cust credit 0
  (check-equal? (machine-output (make-machine-state 10 10 0 0) "kale") "Nothing")
  ;; input "carrots" when cust credit 0
  (check-equal? (machine-output (make-machine-state 10 10 0 0) "carrots") "Nothing")
  ;; input "change"
  (check-equal? (machine-output (make-machine-state 10 10 3 0) "change") 3)
  ;; input "change" when 0
  (check-equal? (machine-output (make-machine-state 10 10 0 0) "change") "Nothing")
  ;; input number of quarters
  (check-equal? (machine-output (make-machine-state 10 10 3 0) 3) "Nothing"))

