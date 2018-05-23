;; Cubelets program set 10


#lang racket
(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "WidgetWorks.rkt")
(check-location "10" "q2.rkt")

(provide cubelets-init
         make-block
         SBlock<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To run the program:
;; (run rate) -> (run 0.25)

;; Press "b" for blocks

;; SBlockFactory consists of the sblocks. Initially it will be empty.

;;; CONSTANTS

(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)
(define NEW-BLOCK-EVENT "b")
(define BLOCK-LENGTH 20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data Definitions

;; A SBlockFactory is an object whose class implements the SWidget<%>
;; interface.

;; A SBlock is an object whose class implements the SBlocks<%>
;; interface.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERFACES

;; A SBlock is an object of any class that implements SBlock<%>

(define SBlock<%> 
  (interface
      
      ;; The interface SBlock<%> inherits from the interface Widget<%>.
      ;; This means that any class that implements SBlock<%> must implement
      ;; all the methods from SWidget<%> plus all the methods defined here.
      (SWidget<%>)
    
    
    ;; Note: the Widgets of the space-invader-examples don't respond
    ;; to mouse "move" events, but some of our toys do.  So we add an
    ;; after-move method to the interface.
    
    
    ;; -> ListOfSBlock
    ;; RETURNS: the teammates of this sblock
    get-team
    
    ;;  SBlock -> Void
    ;; EFFECT: adds the given sblock to this block's team
    add-teammate
    
    ;; -> Integer
    ;; RETURNS: the x or y coordinates of this sblock
    sblock-x
    sblock-y
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cubelets-init : -> Container
;; GIVEN: no arguments
;; RETURNS: a Container, initially with no blocks, which when run, will
;; run in a 600x500 canvas and process the events in the description above.

(define (cubelets-init)
  (local
    ((define initial-block-factory (make-block-factory))
     (define initial-container (container-init CANVAS-WIDTH CANVAS-HEIGHT))
     (define fact-in-cont (send initial-container
                                add-stateful-widget
                                initial-block-factory)))
    initial-container))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run: PosNum -> Void
;; GIVEN: a frame rate (in seconds/tick)
;; EFFECT: This function creates a Container, and places a SBlockFactory with
;; no blocks in that Container.  The function may or may not put other
;; Widgets and SWidgets in the container, depending on the
;; implementation. The function then runs the Container at the given
;; frame rate using WidgetWorks.

(define (run rate)
  (send (cubelets-init) run rate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SBlockFactory% class

;; Constructor template for SBlockFactory%:
;; (new SBlockFactory% [blocks ListOfSBlock] [lmx Integer]
;; [lmy Integer])
;; INTERPRETATION:
;; An object of class SBlockFactory% represents a Block Factory
;; where new Block will be created.

(define SBlockFactory%
  (class* object% (SWidget<%>)
    
    (init-field [blocks empty]) ;  ListOfSBlock
    
    ;; mx and my are the last coordinates when the mouse button down or up
    ;; event took place
    (init-field [lmx (/ CANVAS-WIDTH 2)] [lmy (/ CANVAS-HEIGHT 2)])
    
    (super-new)
    
    ;;-----------------------------------------------------------------------------
    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this widget to the state it should have
    ;; following a tick. 
    ;; Use HOFC map on the blocks
    
    (define/public (after-tick)
      (set! blocks
            (map
             ;; SBlock -> Void
             (lambda (block) (begin (send block after-tick) block))
             blocks)))
    
    ;;-----------------------------------------------------------------------------
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;; painted on it.
    ;; Use HOFC foldr on blocks    
    
    (define/public (add-to-scene s)
      (foldr
       ;; SBlock -> Scene
       (lambda (block scene)
         (send block add-to-scene scene))
       s
       blocks))
    
    ;;-----------------------------------------------------------------------------
    ;; after-key-event : KeyEvent -> Void
    ; GIVEN: a key event
    ; EFFECT: updates this widget to the state it should have
    ; following the given key event
    ;; STRATEGY: Cases on kev
    ;; "b" create new block
    ;; other keystrokes are passed on to the objects in the sblockfactory.
    
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-BLOCK-EVENT)
         (set! blocks
               (cons (make-block lmx lmy empty) blocks)
               )]
        [else
         (set! blocks
               (map
                ;; SBlock -> Void
                (lambda (block) (begin (send block after-key-event kev) block))
                blocks)
               )]))
    
    
    ;;--------------------------------------------------------------------------------
    
    ;; after-button-down: Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this widget to the state it should have
    ; following the button down mouse event at the given location.
    
    (define/public (after-button-down mx my)
      (begin
        (set! lmx mx)
        (set! lmy my)
        (set! blocks
              (map
               ;; SBlock -> Void
               (lambda (block) (begin (send block after-button-down mx my) block))
               blocks))))
    
    ;;--------------------------------------------------------------------------------
    
    ;; after-button-up: Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this widget to the state it should have
    ; following the button up mouse event at the given location.
    
    (define/public (after-button-up mx my)
      (begin
        (set! lmx mx)
        (set! lmy my)
        (set! blocks
              (map
               ;; SBlock -> Void
               (lambda (block) (begin (send block after-button-up mx my) block))
               blocks))))
    
    ;;--------------------------------------------------------------------------------
    
    ;; after-drag: Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this widget to the state it should have
    ; following the drag mouse event at the given location.
    
    (define/public (after-drag mx my)
      (begin
        (set! blocks
              (map
               ;; SBlock -> SBlock
               (lambda (block) (begin (send block after-drag mx my) block))
               blocks))
        (for-each
         ;; SBlock -> Void
         (lambda (b) (if (send b is-selected?)
                         (check-overlap b (remove b blocks))
                         1))
         blocks)
        ))
    
    ;;--------------------------------------------------------------------------------
    
    ;; check-overlap: SBlock ListOfSBlock -> Void
    ;; EFFECT: updates the blocks of each team with new team members
    ;; if an intersection has occurred between any two blocks
    
    (define (check-overlap b blocks)
      (local
        ((define bx (send b sblock-x))
         (define by (send b sblock-y))
         (define half-length (/ BLOCK-LENGTH 2)))
        (for-each
         ;; SBlock -> Void
         (lambda (bl) (get-overlapped-teammates bx by b bl half-length))
         blocks)))
    
    ;;--------------------------------------------------------------------------------
    
    ;; get-overlapped-teammates: Integer Integer SBlock SBlock Integer -> Void
    ;; EFFECT: checks for overlap and updates the team of each block
    
    (define (get-overlapped-teammates bx by b bl half-length)
      (local
        ((define blx (send bl sblock-x))
         (define bly (send bl sblock-y))
         (define overlap? (overlapped? bx by blx bly half-length)))
        (if overlap?
            (get-complete-team b bl)
            1)))
    
    ;;--------------------------------------------------------------------------------
    
    ;; get-complete-team: SBlock SBlock -> Void
    ;; EFFECT: updates each block's team with the new team members formed as
    ;; a result of the intersection between two blocks.
    
    (define (get-complete-team b bl)
      (local
        ((define complete-team (set-union (list b bl)
                                          (send b get-team)
                                          (send bl get-team))))
        (for-each
         ;; SBlock -> Void
         (lambda (ct) (begin
                        (for-each
                         ;; SBlock -> Void
                         (lambda (nt)
                           (send ct add-teammate nt))
                         (remove ct complete-team))))
         complete-team)))
    ;;--------------------------------------------------------------------------------
    
    ;; overlapped?: Integer Integer Integer Integer Integer -> Boolean
    ;; RETURNS: true iff the this block intersects any other block
    
    (define (overlapped? bx by blx bly half-length)
      (and (or (<= (- blx half-length) (+ bx half-length) (+ blx half-length))
               (<= (- blx half-length) (- bx half-length) (+ blx half-length)))
           (or (<= (- bly half-length) (+ by half-length) (+ bly half-length))
               (<= (- bly half-length) (- by half-length) (+ bly half-length)))))
    
    ;;--------------------------------------------------------------------------------
    
    ;; after-move:; Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this widget to the state it should have
    ;; following the specified mouse event at the given location.
    ;; NOTE: sblock has no behavior. returning nonsense values to
    ;; aid in debugging. 
    
    (define/public (after-move mx my)
      1)
    
    ;;--------------------------------------------------------------------------------
    
    ;; -> ListOfSBlock
    
    (define/public (for-test:blocks)
      blocks)
    ))

;-------------------------------------------------------------------------------

;; make-block-factory: -> SBlockFactory
;; RETURNS: a new object of class SBlockFactory% with no sblocks in it.

(define (make-block-factory)
  (new SBlockFactory%))

;-------------------------------------------------------------------------------
;; TESTS for SBlockFactory% :

(begin-for-test
  (local ((define Sblockfactory (make-block-factory))
          (define Sblockfactory1 (new SBlockFactory%))
          (define Sblock (new SBlock%
                              [x (/ CANVAS-WIDTH 2)]
                              [y (/ CANVAS-HEIGHT 2)]
                              [team empty]))
          (define EMPTY-CANVAS (empty-scene CANVAS-WIDTH
                                            CANVAS-HEIGHT)))
    (send Sblockfactory after-tick)
    (check-equal? (send Sblockfactory for-test:blocks)
                  empty
                  "")
    (send Sblockfactory after-key-event "b")
    (send Sblockfactory after-move 250 300)
    (send Sblockfactory after-key-event "k")
    
    (send Sblockfactory after-tick)
    ;; moving left to right and top to bottom
    (send Sblockfactory add-to-scene EMPTY-CANVAS)
    (check-equal? (send Sblockfactory for-test:blocks)
                  (send Sblockfactory for-test:blocks)
                  "it should list with one block in it")
    (send Sblockfactory after-button-down 300 250)
    (send Sblockfactory after-drag 350 300)
    (send Sblockfactory after-key-event "b")
    (send Sblockfactory after-button-up 350 250)
    
    ;(send Sblockfactory for-test:blocks)
    (send Sblockfactory after-button-down 300 250)
    (send Sblockfactory after-drag 340 300)
    
    (send Sblockfactory after-button-up 300 250)
    
    ;; moving right to left and bottom to top
    (send Sblockfactory1 after-key-event "b")
    (send Sblockfactory1 after-button-down 300 250)
    (send Sblockfactory1 after-drag 250 200)
    
    (check-equal? (send (first (send Sblockfactory1 for-test:blocks))
                        for-test:ox)
                  0)
    (send Sblockfactory1 after-button-up 250 200)
    (send Sblockfactory1 after-button-down 300 250)
    
    (send Sblockfactory1 after-button-up 300 250)
    
    (send Sblockfactory1 after-key-event "b")
    (send Sblockfactory1 after-button-down 300 250)
    
    (send Sblockfactory1 after-drag 265 215)    
    (send Sblockfactory1 after-button-up 265 215)
    
    ; if a block doesn't overlaps
    (send Sblockfactory1 after-button-down 300 250)
    (send Sblockfactory1 after-button-up 300 250)
    (send Sblockfactory1 after-key-event "b")
    (send Sblockfactory1 after-button-down 300 250)
    (send Sblockfactory1 after-drag 350 215)    
    (send Sblockfactory1 after-button-up 350 215)
    
    (check-equal? (send (first (send Sblockfactory1 for-test:blocks))
                        for-test:selected?)
                  false
                  "it should be false since no block is selected")
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The SBlock% class

;; Constructor template for SBlock%:
;; (new SBlock% [x Int][y Int][team ListOfSBlock])
;; Interpretation: An object of class SBlock% represents a Block.

(define SBlock%
  (class* object% (SBlock<%>)
    
    (init-field [team empty]) ;  ListOfSBlock
    
    ;; the x and y position of the center of the SBlock
    (init-field [x 0] [y 0])
    
    ;; selected? denotes if the block is selected or not
    (init-field [selected? false])
    
    ;; the mx and my offsets which is the distance of mouse from the
    ;; center of the block are set when Block selected
    (init-field [ox 0] [oy 0])
    
    (super-new)
    
    ;;-----------------------------------------------------------------------------
    ;; after-tick : -> Void
    ;; EFFECT: sblock has no behavior. returning nonsense values to
    ;; aid in debugging.
    
    (define/public (after-tick)
      1)
    
    ;;-----------------------------------------------------------------------------
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;; painted on it.
    
    (define/public (add-to-scene s)
      (if selected?
          (place-image (rectangle BLOCK-LENGTH BLOCK-LENGTH "outline" "red") x y s)
          (place-image (rectangle BLOCK-LENGTH BLOCK-LENGTH "outline" "green") x y s)))
    
    ;;-----------------------------------------------------------------------------
    ;; after-key-event: KeyEvent -> Void
    ;; NOTE: SBlock has no behavior for this method
    
    (define/public (after-key-event kev)
      1)
    
    ;;--------------------------------------------------------------------------------
    
    ;; after-button-down: Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this widget to the state it should have
    ;; following the button down mouse event at the given location.
    
    (define/public (after-button-down mx my)
      (if (in-block? mx my)
          (begin
            (set! selected? true)
            (set! ox (- mx x))
            (set! oy (- my y)))
          1))
    
    ;;--------------------------------------------------------------------------------
    
    ;; after-button-up: Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this widget to the state it should have
    ;; following the button up mouse event at the given location.
    
    (define/public (after-button-up mx my)
      (set! selected? false))
    
    ;;--------------------------------------------------------------------------------
    
    ;; after-drag: Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this widget to the state it should have
    ;; following the drag mouse event at the given location.
    
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (for-each
             ;; SBlock -> Void
             (lambda (b) (local
                           ((define old-mx (+ x ox))
                            (define old-my (+ y oy))
                            (define bx (send b sblock-x))
                            (define by (send b sblock-y))
                            (define tx (- old-mx bx))
                            (define ty (- old-my by)))
                           (begin
                             (send b update-x (- mx tx))
                             (send b update-y (- my ty)))))
             team)
            (set! x (- mx ox))
            (set! y (- my oy)))1))
    
    ;;--------------------------------------------------------------------------------
    
    ;; after-move: Integer Integer -> Void
    ;; NOTE: SBlock has no behavior for this method
    
    (define/public (after-move mx my)
      1)
    
    ;;--------------------------------------------------------------------------------
    
    ;; in-block?: Integer Integer -> Boolean
    ;; RETURNS: true iff the mouse pointer coordinates are inside/on the block
    
    (define (in-block? mx my)
      (local
        ((define HALF-LENGTH (/ BLOCK-LENGTH 2)))
        (and (<= (- x HALF-LENGTH) mx (+ x HALF-LENGTH))
             (<= (- y HALF-LENGTH) my (+ y HALF-LENGTH)))))
    
    ;;--------------------------------------------------------------------------------
    
    ;; add-teammate: SBlock -> Void
    ;; EFFECT: adds the given sblock to this block's team
    
    (define/public (add-teammate b)
      (set! team (set-union (list b) team)))
    
    ;;--------------------------------------------------------------------------------
    
    ;; update-x: Integer -> Void
    ;; EFFECT: updates the x coordinate of this sblock
    
    (define/public (update-x nx)
      (set! x nx))
    
    ;;--------------------------------------------------------------------------------
    
    ;; update-x: Integer -> Void
    ;; EFFECT: updates the y coordinate of this sblock
    
    (define/public (update-y ny)
      (set! y ny))
    
    ;;--------------------------------------------------------------------------------
    
    ;; is-selected?: -> Boolean
    ;; Returns: true iff the block is selected
    
    (define/public (is-selected?)
      selected?)
    
    ;---------------------------------------------------------------------------------
    
    ;; get-team : -> ListOfSBlock
    ;; RETURNS: the teammates of this sblock
    
    (define/public (get-team)
      team)
    
    ;---------------------------------------------------------------------------------
    
    ;; sblock-x : -> Integer
    ;; sblock-y : -> Integer
    ;; RETURNS: the x or y coordinates of this sblock
    
    (define/public (sblock-x)
      x)
    (define/public (sblock-y)
      y)
    
    ;---------------------------------------------------------------------------------
    ;; Testing purpose functions
    
    (define/public (for-test:selected?)
      selected?)
    
    (define/public (for-test:ox)
      ox)
    
    (define/public (for-test:oy)
      oy)))

;;------------------------------------------------------------------------------

;; make-block : NonNegInt NonNegInt ListOfSBlock -> SBlock
;; GIVEN: an x and y position, and a list of blocks
;; WHERE: the list of blocks is the list of blocks already on the playground.
;; RETURNS: a new block, at the given position, with no teammates
;; NOTE: it is up to you as to whether you use the third argument or
;; not.  Some implementations may use the third argument; others may not.

(define (make-block x y los)
  (new SBlock% [x x] [y y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS for SBlocks%:

(begin-for-test
  (local ((define Sblock1 (new SBlock%))
          (define Sblock (make-block 300 250 (list Sblock1)))
          (define img (empty-scene CANVAS-WIDTH
                                   CANVAS-HEIGHT)))
    
    (send Sblock after-button-down 300 250)
    (check-equal? (send Sblock add-to-scene img)
                  (place-image (rectangle BLOCK-LENGTH BLOCK-LENGTH "outline" "red")
                               300
                               250 img))
    (send Sblock after-drag 300 250)
    (check-equal? (send Sblock after-move 300 250)
                  1)
    (check-equal? (send Sblock for-test:ox)
                  0)
    (check-equal? (send Sblock for-test:oy)
                  0)
    ))