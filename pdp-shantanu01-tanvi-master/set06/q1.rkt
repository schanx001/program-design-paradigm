;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;q1.rkt
;; PURPOSE: design and implement a system for a graphical interface for trees.
;; start with (run 0)

(check-location "06" "q1.rkt")
(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)

(provide
 initial-world
 run
 world-after-mouse-event
 world-after-key-event
 world-to-trees
 tree-to-root
 tree-to-sons
 node-to-center
 node-to-selected?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct world (lon))

;; A World is a (make-world LON)
;; INTERPRETATION: 
;;   -- lon represents a ListOfNodes
;;
;; TEMPLATE:
;; world-fn : WorldState -> ??
;; (define (world-fn w)
;;  (... (world-lon w)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-struct node (x y shape selected? lon pointer-x pointer-y))

;; A Node is a
;; (make-node NonNegInt NonNegInt Shape Boolean ListofNodes Integer Integer)
;;
;; INTERPRETATION:
;;
;; -- x and y are the center coordinates of the node
;; -- shape is a type of figure of the node (in this case a circle or a square)
;; -- selected? returns true if the node is selected, else false
;; -- lon represents the children of the node which is a ListOfNodes
;; -- pointer-x and pointer-y are the coordinates of the mouse pointer
;;
;; TEMPLATE:
;; (define node-fn n)
;; ( ... (node-x n)
;;       (node-y n)
;;       (node-shape n)
;;       (node-selected? n)
;;       (lon-fn (node-lon p))
;;       (node-pointer-x n)
;;       (node-pointer-y n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ListOfNodes is one of:
;; -- empty
;; -- (cons Node ListOfNodes)
;;
;; INTERP:
;; empty             -- represents the sequence with no Node
;; (cons node lon)   -- represents the sequence whose first element is node
;;                      and the rest of sequence is represented by lon
;;
;; TEMPLATE:
;; lon-fn : LON-> ??
;; (define (lon-fn lon
;;   (cond
;;     [(empty? lon) ...]
;;     [else (...
;;             (node-fn (first lon))
;;             (lon-fn (rest lon)))]))

;; HALTING MEASURE: Number of Nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Tree is a:
;; -- Node
;; i.e(make-node NonNegInt NonNegInt Shape Boolean ListOfTree Integer Integer)

;; TEMPLATE:
;; (define tree-fn n)
;; ( ... (node-x n)
;;       (node-y n)
;;       (node-shape n)
;;       (node-selected? n)
;;       (lot-fn (node-lon p))
;;       (node-pointer-x n)
;;       (node-pointer-y n)))

;; A ListOfTree(LOT) is one of:
;; --empty
;; --(cons Tree ListOfTree)

;; A ListofTree(LOT) is same as ListofNodes

;; TEMPLATE:
;; lot-fn : LOT-> ??
;; (define (lot-fn lot
;;   (cond
;;     [(empty? lot) ...]
;;     [else (...
;;             (tree-fn (first lot))
;;             (lot-fn (rest lot)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Shape is one of -
;; -- SHAPE-CIRCLE ("circle")
;; -- SHAPE-SQUARE ("square")

; INTERPRETATION: self evident
;
;; TEMPLATE
;; shape-fn : Shape -> ??
;(define (shape-fn s)
;  (cond
;    [(string=? s SHAPE-CIRCLE)    
;     ...]
;    [(string=? s SHAPE-SQUARE)
;     ...]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A MouseEvent is one of
;; --"button-up"
;; --"drag"
;; --"button-down"

;; mev-fn: MouseEvent -> ??
;; (define (mev-fn mevent)
;;  (cond
;;     [(string=? mevent "button-up")...]
;;     [(string=? mevent "button-down")...]
;;     [(string=? mevent "drag")...]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A KeyEvent is one of
;; --"c"
;; --"s"
;; --"d"

;; (define (kev-fn kev)
;;  (cond
;;     [(string=? kev "c")...]
;;     [(string=? kev "s")...]
;;     [(string=? kev "d")...]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; END OF DATA DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

(define RADIUS 20)
(define SIDE 40)
(define PLACEMENT-DIST (* 3 RADIUS))

(define SHAPE-CIRCLE "circle")
(define SHAPE-SQUARE "square")

(define SHAPE-COLOR "green")
(define LINE-COLOR "blue")

(define INITIAL-MX 0)
(define INITIAL-MY 0)

(define DEFAULT-MEV "button-up")

(define SELECTED-CIRCLE (circle RADIUS "solid" SHAPE-COLOR))
(define UNSELECTED-CIRCLE (circle RADIUS "outline" SHAPE-COLOR))

(define SELECTED-SQUARE (rectangle SIDE SIDE "solid" SHAPE-COLOR))
(define UNSELECTED-SQUARE (rectangle SIDE SIDE "outline" SHAPE-COLOR))

(define NEW-CIRCLE
  (make-node 250 20 SHAPE-CIRCLE false  empty INITIAL-MX INITIAL-MY))
(define NEW-SQUARE
  (make-node 250 20 SHAPE-SQUARE false  empty INITIAL-MX INITIAL-MY))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; MAIN FUNCTION.

;; run :  Any -> World
;; GIVEN: any value
;; EFFECT: runs a copy of an initial world
;; RETURNS: the final state of the world.  The given value is ignored.

(define (run x)
  (big-bang (initial-world 0)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> World
;; GIVEN: any value
;; RETURNS: an initial world.  The given value is ignored.
;; EXAMPLES:
;;   initial-world(0) -> (make-world empty)
;; STRATEGY: Using template for World

(define (initial-world x)
  (make-world empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; GIVEN: a world to be drawn on the canvas
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE:
;; (world-to-scene (make-world (list
;;                     (make-node 250 20 SHAPE-SQUARE #false '() 0 0)) ->
;;  gives a scene with an unselected square drawn on the canvas at 250,20
;; STRATEGY: Use template for World on w


(define (world-to-scene w)
  (cond
    [(empty? (world-lon w)) EMPTY-CANVAS]
    [else (place-nodes (world-lon w) EMPTY-CANVAS)]))

;; SAMPLE

(define world2 (make-world
                (list
                 (make-node 250 20 SHAPE-SQUARE #false '() 0 0)
                 (make-node 239 113 SHAPE-CIRCLE #false                  
                            (list
                             (make-node 179 173 SHAPE-SQUARE #false '() 0 0)
                             (make-node 239 173 SHAPE-CIRCLE #false '() 0 0))
                            0
                            0))))

(define world3 (make-world
                (list
                 (make-node 250 20 SHAPE-SQUARE #false '() 0 0)
                 (make-node 239 113 SHAPE-CIRCLE #true            
                            (list
                             (make-node 179 173 SHAPE-SQUARE #true '() 0 0)
                             (make-node 239 173 SHAPE-CIRCLE #false '() 0 0))
                            0
                            0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; place-nodes: ListOfNodes Scene -> Scene
;; GIVEN: A list of nodes and a scene
;; RETURNS: A scene that portrays the given world with a list of nodes.
;; EXAMPLE:
;;(place-nodes (list(make-node 134 224 SHAPE-CIRCLE #f '() 0 0)) EMPTY-CANVAS)->
;; (place-image (circle 20 "outline" "green") 134 224)
;;
;; STRATEGY: Use HOF foldr on lon
;; HALTING MEASURE: length of lon

;(define (place-nodes lon s)
;  (cond
;    [(empty? lon) s]
;    [else (place-node (first lon) (place-nodes (rest lon) s))]))

(define (place-nodes lon s)
  (foldr
   #|
    Node LON -> LON
    GIVEN: A node and a list of nodes
    RETURNS: A list of nodes placed on the canvas
   |#
   (lambda (elt list) (place-node elt list))
   s
   lon))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; place-node: Node Scene -> Scene
;; GIVEN: A node and a scene
;; RETURNS: A scene that portrays the given world with the node.
;; EXAMPLE:
;;(place-node (make-node 134 224 SHAPE-CIRCLE #f '() 0 0)) EMPTY-CANVAS)->
;; (place-image (circle 20 "outline" "green") 134 224)
;; STRATEGY: Use template for Node on n

(define (place-node n s)
  (cond
    [(node-selected? n) (place-selected-node n s)]
    [else (place-unselected-node n s)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; place-selected-node: Node Scene -> Scene
;; GIVEN: A selected node and a scene
;; RETURNS: A scene that portrays the given world with the selected node.
;; EXAMPLE:
;;(place-selected-node (make-node 134 224 SHAPE-CIRCLE #t '() 0 0))
;;  EMPTY-CANVAS)-> (place-image (circle 20 "solid" "green") 134 224)
;; 
;; STRATEGY: Cases for Shape on shape of node n

(define (place-selected-node n s)
  (cond
    [(equal? (node-shape n) SHAPE-CIRCLE) (place-shape SELECTED-CIRCLE n s)]
    [(equal? (node-shape n) SHAPE-SQUARE) (place-shape SELECTED-SQUARE n s)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; place-shape: Image Node Scene -> Scene
;; GIVEN: An image, a node and a scene 
;; RETURNS: A scene that portrays the given world with the given image and the
;; node along with the attached lines for its children.
;; EXAMPLE:
;;(place-shape NEW-CIRCLE
;;   (make-node 134 284 SHAPE-CIRCLE #false '() 0 0) EMPTY-CANVAS) ->
;; (place-image (circle 20 "outline" "green") 134 284 EMPTY-CANVAS) 
;; 
;; STRATEGY: Use template for Node on n

(define (place-shape shapeimage n s)
  (place-image  shapeimage
                (node-x n)
                (node-y n)
                (if (empty? (node-lon n))
                    s
                    (place-all-lines (node-lon n) (node-x n) (node-y n) s))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; place-unselected-node: Node Scene -> Scene
;; GIVEN: An unselected node and a scene
;; RETURNS: A scene that portrays the given world with the unselected node.
;; EXAMPLE:
;;(place-unselected-node (make-node 134 224 SHAPE-CIRCLE #f '() 0 0))
;;  EMPTY-CANVAS)-> (place-image (circle 20 "outline" "green") 134 224)
;; 
;; STRATEGY: Cases for Shape on shape of node n


(define (place-unselected-node n s)
  (cond
    [(equal? (node-shape n) SHAPE-CIRCLE) (place-shape UNSELECTED-CIRCLE n s)]
    [(equal? (node-shape n) SHAPE-SQUARE) (place-shape UNSELECTED-SQUARE n s)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; place-all-lines: ListOfNodes NonNegInt NonNegIt Scene -> Scene
;; GIVEN: A list of nodes, x and y coordinates of parent node and a scene
;; RETURNS: A scene that portrays the lines from the parent to all the child
;;          nodes.
;; EXAMPLE: see test cases below for world-to-scene 
;; STRATEGY: Use HOF foldr on lon
;; HALTING MEASURE: Number of Nodes

;(define (place-all-lines lon x y s)
;  (cond
;    [(empty? lon) s]
;    [else (place-line (first lon) x y
;                      (place-all-lines (rest lon) x y s))]))

(define (place-all-lines lon x y s)
  (foldr
   #|
    Node LON -> LON
    GIVEN: A node and a list of nodes
    RETURNS: A list of nodes after placing lines for all the child nodes
   |#
   (lambda (elt list) (place-line elt x y list))
   s
   lon))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; place-line: Node NonNegInt NonNegIt Scene -> Scene
;; GIVEN: A node, x and y coordinates of parent node and a scene
;; RETURNS: A scene that portrays the line from the parent to the child node
;; EXAMPLE: see test cases below for world-to-scene 
;; STRATEGY: Use template for Node on n

(define (place-line n x y s)
  (scene+line (place-node n s) x y (node-x n) (node-y n) LINE-COLOR))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sample:

(define world-output (place-nodes (world-lon world2) EMPTY-CANVAS))
(define world-output3 (place-nodes (world-lon world3) EMPTY-CANVAS))

;; TESTS for world-to-scene:

(begin-for-test
  (check-equal? (world-to-scene world2)
                world-output
                "the function should return the scene representing all the
                 nodes in world")
  (check-equal? (world-to-scene world3)
                world-output3
                "the function should return the scene representing all
                 the nodes in world")
  (check-equal? (world-to-scene (initial-world 0))
                EMPTY-CANVAS
                "the function should return empty canvas"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a World, a location, and a MouseEvent
;; RETURNS: the state of the world as it should be following the given mouse 
;;  event at that location.
;; EXAMPLES: refer tests
;; DESIGN STRATEGY: Cases on MouseEvent mev


(define (world-after-mouse-event  w mx my mev)
  (cond
    [(mouse=? mev "button-down") (world-after-button-down w mx my)]
    [(mouse=? mev "drag") (world-after-drag w mx my)]
    [(mouse=? mev "button-up") (world-after-button-up w mx my)]
    [else w]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SAmples:

(define w1-button-down (make-world
                        (list (make-node 250 20 SHAPE-SQUARE #false '() 0 0))))
(define w1-output-inside (make-world
                          (list
                           (make-node 250 20 SHAPE-SQUARE #true '() 250 20))))

;; TESTS for world-after-mouse-event:

(begin-for-test
  (check-equal? (world-after-mouse-event w1-button-down 250 20 "button-down")
                w1-output-inside
                "button down inside the shape should select it"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-button-down: World Integer Integer -> World
;; GIVEN: A world and the mouse pointer coordinates
;; RETURNS: The state of the world after button-down event
;; EXAMPLES: (world-after-button-down w1-button-down 250 20) ->
;;  (make-world (list (make-node 250 20 "square" #true '() 250 20)))
;;
;; STRATEGY: Use template for World on w

(define (world-after-button-down w mx my)
  (make-world (lon-after-button-down (world-lon w) mx my)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-drag: World Integer Integer -> World
;; GIVEN: A world and the mouse pointer coordinates
;; RETURNS: The state of the world after mouse drag event
;; EXAMPLES: (world-after-drag w3-drag 350 20) ->
;;  (make-world (list (make-node 350 20 "circle" #true '() 350 20)))
;;
;; STRATEGY: Use template for World on w

(define (world-after-drag w mx1 my1)
  (make-world (lon-after-drag (world-lon w) mx1 my1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-button-up: World Integer Integer -> World
;; GIVEN: A world and the mouse pointer coordinates
;; RETURNS: The state of the world after mouse button-up event 
;; EXAMPLES: (world-after-button-up w2-button-up 250 20) ->
;;  (make-world (list (make-node 250 20 "circle" #false '() 0 0)))
;;
;; STRATEGY: Use template for World on w

(define (world-after-button-up w mx1 my1)
  (make-world (lon-after-button-up (world-lon w) mx1 my1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lon-after-button-down : LON Integer Integer -> LON
;; GIVEN: a LON and mouse pointer co-ordinates(location)
;; RETURNS: the state of the list of nodes as it should be following button
;;         down at that location.
;; EXAMPLES: (lon-after-button-down
;;  (list (make-node 250 20 "circle" #false '() 0 0)) 250 20) ->
;;  (list (make-node 250 20 "circle" #true '() 250 20))
;;
;; STRATEGY: Use HOF foldr on lon
;; HALTING MEASURE: length of lon


(define (lon-after-button-down lon mx my)
  (foldr
   #| Node LON -> LON
    ; GIVEN: A Node and a list of nodes
    ; RETURNS: A list of nodes after button-down event
    |#
   (lambda (elt list) (if (in-shape? elt mx my)
                          (cons (node-after-button-down elt mx my)
                                list)
                          (cons (make-node (node-x elt)
                                           (node-y elt)
                                           (node-shape elt)
                                           (node-selected? elt)
                                           (lon-after-button-down
                                            (node-lon elt) mx my)
                                           mx
                                           my)
                                list))) empty lon))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; node-after-button-down : Node Integer Integer -> Node
;; GIVEN: a Node and a location of mouse pointer
;; RETURNS: a node as it should be following button down
;;          at that location.
;; EXAMPLES: (node-after-button-down
;;  (make-node 250 20 "circle" #false '() 0 0) 250 20) ->
;;  (make-node 250 20 "circle" #true '() 250 20)
;;
;; STRATEGY: Use template for Node on n

(define (node-after-button-down n mx1 my1)
  (make-node (node-x n)
             (node-y n)
             (node-shape n)
             true
             (lon-after-button-down (node-lon n) mx1 my1)
             mx1
             my1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lon-after-drag: LON Integer Integer -> LON
;; GIVEN: a list of nodes and mouse pointer location
;; RETURNS: a list of nodes following the drag event of the mouse.
;; EXAMPLES:
;; (lon-after-drag (list (make-node 250 20 "circle" #true '() 0 0)) 350 20))
;; = (list (make-node 350 20 "circle" #true '() 0 0)) 350 20)
;; DESIGN STRATEGY: Using HOF foldr on lon
;; HALTING MEASURE: length of lon

(define (lon-after-drag lon mx my)
  (foldr
   #| Node LON -> LON
    ; GIVEN: A Node and a list of nodes
    ; RETURNS: A list of nodes after mouse drag event
    |#
   (lambda (elt list) (if (node-selected? elt)
                          (cons (node-after-drag elt mx my)
                                list)
                          (cons (make-node (node-x elt)
                                           (node-y elt)
                                           (node-shape elt)
                                           (node-selected? elt)
                                           (lon-after-drag
                                            (node-lon elt) mx my)
                                           (node-pointer-x elt)
                                           (node-pointer-y elt))
                                list)))
   empty
   lon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; node-after-drag: Node Integer Integer -> Node
;; GIVEN: a Node and mouse pointer location
;; RETURNS: a node following the drag event of the mouse.
;; EXAMPLES: (node-after-drag (make-node 350 20 "circle" #true '() 0 0)) 350 20)
;; =(make-node 350 20 "circle" #true '() 0 0))
;; DESIGN STRATEGY: Using template for Node on n

(define (node-after-drag n mx1 my1)
  (make-node
   (- mx1 (- (node-pointer-x n) (node-x n)))
   (- my1 (- (node-pointer-y n) (node-y n)))
   (node-shape n)
   (node-selected? n)
   (lon-after-drag-next (node-lon n) mx1 my1)
   mx1
   my1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lon-after-drag-next: LON Integer Integer -> LON
;; GIVEN: a list of nodes and mouse pointer location
;; RETURNS: a list of nodes following the drag event for the children of dragged
;;           node
;; EXAMPLES: (lon-after-drag-next
;; (list (make-node 250 20 "circle" true '() 250 20) 350 20))
;; =(list (make-node 350 20 "circle" #true '() 350 20))
;; DESIGN STRATEGY: Using HOF foldr on lon
;; HALTING MEASURE: length of lon

(define (lon-after-drag-next lon mx1 my1)
  (foldr
   #| Node LON -> LON
    ; GIVEN: A Node and a list of nodes
    ; RETURNS: A list of nodes after drag event
    |#
   (lambda (elt list) (cons
                       (node-after-drag elt mx1 my1)
                       list))
   empty
   lon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lon-after-button-up: LON Integer Integer -> LON
;; GIVEN: a list of nodes and mouse pointer location
;; RETURNS: a list of nodes following the button-up event of the mouse
;; EXAMPLES:(lon-after-button-up
;;  (list (make-node 250 20 "circle" #true '() 0 0)) 250 20) ->
;;  (list (make-node 250 20 "circle" #false '() 250 20))
;; DESIGN STRATEGY: Using HOF foldr on lon
;; HALTING MEASURE: length of lon

(define (lon-after-button-up lon mx1 my1)
  (foldr
   #| Node LON -> LON
    ; GIVEN: A Node and a list of nodes
    ; RETURNS: A list of nodes after button-up event
    |#
   (lambda (elt list) (cons
                       (node-after-button-up elt mx1 my1)
                       list))
   empty
   lon))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; node-after-button-up: Node Integer Integer -> Node
;; GIVEN: a list of nodes and mouse pointer location
;; RETURNS: a node following the button-up event of the mouse
;; EXAMPLES: (node-after-button-up
;;  (make-node 250 20 "circle" #true '() 0 0) 250 20) ->
;;  (make-node 250 20 "circle" #false '() 0 0)
;; DESIGN STRATEGY: Using template for Node on n

(define (node-after-button-up n mx1 my1)
  (make-node (node-x n)
             (node-y n)
             (node-shape n)
             false
             (lon-after-button-up (node-lon n) mx1 my1)
             INITIAL-MX
             INITIAL-MY))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in-shape?: Node Integer Integer -> Boolean
;; GIVEN: a node and mouse pointer location
;; RETURNS: true iff the mouse pointer is inside the shape's boundary
;; EXAMPLES: Refer test case below
;; DESIGN STRATEGY: Cases for Shape on (node-shape n)

(define (in-shape? n mx1 my1)
  (cond
    [(string=? (node-shape n) SHAPE-CIRCLE) (in-circle? n mx1 my1)]
    [(string=? (node-shape n) SHAPE-SQUARE) (in-square? n mx1 my1)]))

;; TESTS:

(begin-for-test
  (check-equal? (in-shape?
                 (make-node 250 20 SHAPE-CIRCLE #true '() 250 20) 240 20)
                true
                "Function should return true as mouse pointer is inside node"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in-circle?: Node Integer Integer -> Boolean
;; GIVEN: a node and mouse pointer location
;; RETURNS: true iff the mouse pointer is inside the shape's boundary
;;          where shape is a circle
;; EXAMPLES:
;;(in-circle? (make-node 250 20 SHAPE-CIRCLE #true '() 250 20) 240 20) -> true
;; DESIGN STRATEGY: Using template for Node on n

(define (in-circle? c mx1 my1)
  (< (sqrt (+ (sqr (- mx1 (node-x c)))
               (sqr (- my1 (node-y c)))))
      RADIUS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in-square?: Node Integer Integer -> Boolean
;; GIVEN: a node and mouse pointer location
;; RETURNS: true iff the mouse pointer is inside the shape's boundary
;;          where shape is a square
;; EXAMPLES:
;; (in-square? (make-node 250 20 SHAPE-SQUARE #true '() 250 20) 240 20) -> true
;; DESIGN STRATEGY:  Using template for Node on n


(define (in-square? n mx1 my1)
  (and
   (< 
    (- (node-x n) (/ SIDE 2))
    mx1
    (+ (node-x n) (/ SIDE 2)))
   (< 
    (- (node-y n) (/ SIDE 2))
    my1
    (+ (node-y n) (/ SIDE 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a World and a key event
;; RETURNS: the state of the world as it should be following the given key event
;; EXAMPLES: (world-after-key-event (make-world empty) "c")
;;           =(make-world (list (make-node 250 20 SHAPE-CIRCLE #false '() 0 0)))
;; STRATEGY: Cases on KeyEvent kev

(define (world-after-key-event w kev)
  (cond
    [(key=? "c" kev) (lon-after-key-event lon-after-c-event w kev)]     
    [(key=? "s" kev) (lon-after-key-event lon-after-s-event w kev)]
    [(key=? "d" kev) (world-after-del-key w kev)]
    [else w]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; KeyEveFn is a function which has the following contract:
;; LON -> LON
;; A KeyEveFn can be lon-after-c-event or lon-after-s-event

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lon-after-key-event: KeyEveFn World KeyEvent -> World
;; GIVEN: A KeyEveFn, a world and a key event
;; RETURNS: the state of the world as it should be following the given key event
;; EXAMPLES: refer test cases 
;; DESIGN STRATEGY: Using template for World on w


(define (lon-after-key-event key-eve-fn w kev)
  (cond
    [(empty? (world-lon w)) (make-world (create-node-no-node-selected w kev))]
    [(any-node-selected? (world-lon w)) (make-world (key-eve-fn (world-lon w)))]
    [else (make-world (create-node-no-node-selected w kev))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-del-key: World KeyEvent -> World
;; GIVEN: A World and a key event
;; RETURNS: a world state as it should be following the delete key event
;; EXAMPLES: refer test cases 
;; DESIGN STRATEGY: Using template for World on w

(define (world-after-del-key w kev)
  (cond
    [(empty? (world-lon w)) w]
    [(any-node-selected? (world-lon w))
     (make-world (lon-after-delete-event (world-lon w)))]
    [else w]))

;; SAMPLE:

(define world1-output (make-world
                       (list
                        (make-node 250 20 SHAPE-SQUARE #false '() 0 0)
                        (make-node
                         387
                         88
                         SHAPE-SQUARE
                         #false
                         (list
                          (make-node 327 148 SHAPE-SQUARE #false '() 0 0)
                          (make-node 387 148 SHAPE-CIRCLE #false '() 0 0))
                         0
                         0)
                        (make-node
                         236
                         100
                         SHAPE-CIRCLE
                         #false
                         (list (make-node 236 160 SHAPE-CIRCLE #false '() 0 0))
                         0
                         0))))


;; TESTS for world-after-key-event:

(begin-for-test
  (check-equal? (world-after-key-event (make-world empty) "c")
                (make-world
                 (list (make-node 250 20 SHAPE-CIRCLE #false '() 0 0)))
                "function should return the world with a new circle added")
  (check-equal? (world-after-key-event world1 "s")
                world1-output
                "function should return the world with new square added"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lon-after-delete-event: LON -> LON
;; GIVEN: A list of nodes
;; RETURNS: a list of nodes following the delete event
;; EXAMPLES: refer test cases
;; DESIGN STRATEGY: Using HOF foldr on lon

(define (lon-after-delete-event lon)
  (foldr
   #|
     Node -> LON
     GIVEN: A Node
     RETURNS: A list of nodes after the delete event
    |#
   (lambda (elt list) (if (node-selected? elt)
                          (append (lon-after-delete-event (node-lon elt)) list)
                          (cons (check-delete-for-children elt)list)))
   empty lon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-delete-for-children: Node -> Node
;; GIVEN: A Node to check if any of its children is selected for deletion
;; RETURNS: a node with its children updated following the delete event
;; EXAMPLES: refer test cases
;; DESIGN STRATEGY: Using template for Node on elt

(define (check-delete-for-children elt)
  (make-node (node-x elt)
             (node-y elt)
             (node-shape elt)
             (node-selected? elt)
             (lon-after-delete-event (node-lon elt))
             (node-pointer-x elt)
             (node-pointer-y elt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     

;; create-node-no-node-selected: World KeyEvent -> LON
;; GIVEN: A World and a KeyEvent, which is one of "c" or "s"
;; RETURNS: a list of node updated following the key event on world
;; EXAMPLES:
;; (create-node-no-node-selected (make-world empty) "c") ->
;;   (list (make-node 250 20 SHAPE-CIRCLE #false '() 0 0))
;; DESIGN STRATEGY: Cases on KeyEvent kev

(define (create-node-no-node-selected w kev)
  (cond
    [(key=? "c" kev) (cons NEW-CIRCLE (world-lon w))]
    [(key=? "s" kev) (cons NEW-SQUARE (world-lon w))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; any-node-selected?: LON -> Boolean
;; GIVEN: A list of nodes to check if any node is selected
;; RETURNS: true iff any node is selected
;; EXAMPLES:
;; (any-node-selected? (list (make-node 250 20 SHAPE-CIRCLE #false '() 0 0))->f
;; DESIGN STRATEGY: Using HOF ormap on lon
;; HALTING MEASURE: length of lon

(define (any-node-selected? lon)
  (ormap
   #|
   Node -> LON
   GIVEN: A Node
   RETURNS: true if the node is selected else false
  |#
   (lambda (elt)
     (or
      (node-selected? elt)
      (any-node-selected? (node-lon elt))))
   lon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lon-after-c-event: LON -> LON
;; GIVEN: A list of nodes
;; RETURNS: a list of nodes following the circle key event
;; EXAMPLES:
;; (lon-after-c-event (list (make-node 350 20 SHAPE-CIRCLE #false '() 0 0)) ->
;; (list (make-node 250 20 SHAPE-CIRCLE #false '() 0 0)
;;   (make-node 350 20 SHAPE-CIRCLE #false '() 0 0))
;; DESIGN STRATEGY: Using HOF foldr on lon
;; HALTING MEASURE: length of lon

(define (lon-after-c-event lon)
  (foldr
   #|
    Node LON -> LON
    GIVEN: A node and a lst of nodes
    RETURNS: A list of nodes after pressing the "c" key
   |#
   (lambda (elt list)
     (if (node-selected? elt)
         (cons (node-after-circle-event elt) list)
         (cons (create-node elt lon-after-c-event)
               list)))
   empty
   lon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lon-After-Create-Event-Fn is a function which has following contract:
;; LON -> LON
;; It can take lon-after-c-event or lon-after-s-event as a value.

;; create-node: Node Lon-After-Create-Event-Fn -> Node
;; GIVEN: A node and a Lon-After-Create-Event-Fn
;; RETURNS: a node with its child elements updated following the create event
;; EXAMPLE: refer tests
;; DESIGN STRATEGY: Using template for Node on elt

(define (create-node elt lon-after-create-event)
  (make-node (node-x elt)
             (node-y elt)
             (node-shape elt)
             (node-selected? elt)
             (lon-after-create-event (node-lon elt))
             (node-pointer-x elt)
             (node-pointer-y elt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lon-after-s-event: LON -> LON
;; GIVEN: A list of nodes
;; RETURNS: a list of nodes following the square key event
;; EXAMPLES:
;; (lon-after-s-event (list (make-node 350 20 SHAPE-CIRCLE #false '() 0 0)) ->
;; (list (make-node 250 20 SHAPE-SQUARE #false '() 0 0)
;;   (make-node 350 20 SHAPE-CIRCLE #false '() 0 0))
;; DESIGN STRATEGY: Using HOF foldr on lon
;; HALTING MEASURE: length of lon

(define (lon-after-s-event lon)
  (foldr
   #|
    Node LON -> LON
    GIVEN: A node and a list of nodes
    RETURNS: A LON after pressing the "s" key
   |#
   (lambda (elt list)
     (if (node-selected? elt)
         (cons (node-after-square-event elt) list)
         (cons (create-node elt lon-after-s-event)
               list)))
   empty
   lon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; node-after-circle-event: Node -> Node
;; GIVEN: A node
;; RETURNS: a node with a circle created as one of its child
;; EXAMPLES:
;; (node-after-circle-event (make-node 250 20 SHAPE-SQUARE #false '() 0 0)) ->
;;  (make-node 250 20 "square" #false
;;   (list (make-node 250 80 "circle" #false '() 0 0)) 0 0)
;; DESIGN STRATEGY: Using template for Node on n

(define (node-after-circle-event n)
  (make-node (node-x n)
             (node-y n)
             (node-shape n)
             (node-selected? n)
             (create-child-node n SHAPE-CIRCLE)
             (node-pointer-x n)
             (node-pointer-y n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; node-after-square-event: Node -> Node
;; GIVEN: A node
;; RETURNS: a node with a square created as one of its child
;; EXMAPLES:
;; (node-after-square-event (make-node 250 20 SHAPE-SQUARE #false '() 0 0)) ->
;; (make-node 250 20 "square" #false (list
;;   (make-node 250 80 "square" #false '() 0 0)) 0 0)
;; DESIGN STRATEGY: Using template for Node on n

(define (node-after-square-event n)
  (make-node (node-x n)
             (node-y n)
             (node-shape n)
             (node-selected? n)
             (create-child-node n SHAPE-SQUARE)
             (node-pointer-x n)
             (node-pointer-y n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create-child-node: Node Shape -> LON
;; GIVEN: A node and a shape
;; RETURNS: a list of nodes which are children nodes
;; EXAMPLES:
;; (create-child-node(make-node 250 20 SHAPE-SQUARE #false '() 0 0)SHAPE-CIRCLE)
;; -> (list (make-node 250 80 "circle" #false '() 0 0))
;; DESIGN STRATEGY: Using template for Node on n

(define (create-child-node n s)
  (cond
    [(empty? (node-lon n)) (create-first-child-node n s)]
    [else (create-another-child-node n s)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create-first-child-node: Node Shape -> LON
;; GIVEN: A node and a shape
;; RETURNS: a list of nodes which are children nodes
;;          where parent doesn't have any children
;; EXAMPLES:
;; (create-first-child-node(make-node 250 20 SHAPE-SQUARE #false '() 0 0)
;;                          SHAPE-CIRCLE)
;; -> (list (make-node 250 80 "circle" #false '() 0 0))
;; DESIGN STRATEGY: Using template for Node on n

(define (create-first-child-node n s)
  (cons (make-node (node-x n)
                   (+ (node-y n) PLACEMENT-DIST)
                   s
                   false
                   empty
                   (node-pointer-x n)
                   (node-pointer-y n))
        (node-lon n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; create-another-child-node: Node Shape -> LON
;; GIVEN: A node and a shape
;; RETURNS: a list of nodes which are children nodes
;;          where parent already has a first child
;; EXAMPLES:
;; (create-another-child-node
;;   (make-node 250 20 SHAPE-SQUARE #false
;;      (list (make-node 250 80 "circle" #false '() 0 0)) 0 0) SHAPE-CIRCLE) ->
;; (list (make-node 190 80 "circle" #false '() 0 0)
;;       (make-node 250 80 "circle" #false '() 0 0))
;; DESIGN STRATEGY: Using template for Node on n


(define (create-another-child-node n s)
  (cons (make-node (- (get-x-leftmost-node (leftmost-node (node-lon n)))
                      PLACEMENT-DIST)
                   (+ (node-y n) PLACEMENT-DIST)
                   s
                   false
                   empty
                   (node-pointer-x n)
                   (node-pointer-y n))
        (node-lon n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; X is the x-co-ordinate of the node

;; A ListOfX is one of:
;; -- empty
;; -- (cons (node-x n) ListOfX)
;;
;; INTERP:
;; empty                  represents the sequence with no X
;; (cons (node-x n) lox)  represents the sequence whose first element is x
;;                        and the rest of sequence is represented by lox
;;
;; TEMPLATE:
;; lox-fn : LOX-> ??
;; (define (lox-fn lox
;;   (cond
;;     [(empty? lox) ...]
;;     [else (...
;;             ((node-x (first lox))
;;             (lox-fn (rest lox)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; leftmost-node: LON -> LOX
;; GIVEN: A list of nodes
;; RETURNS: a list of x-coordinates of the list of nodes
;; EXAMPLES:
;;  (leftmost-node (list (make-node 190 80 "circle" #false '() 0 0)
;;                       (make-node 250 80 "circle" #false '() 0 0))) ->
;;  (list 190 250)
;; STRATEGY: Use HOF foldr on lon

(define (leftmost-node lon)
  (foldr
   #|
    Node LON -> LON
    GIVEN: A node and a list of nodes
    RETURNS: A list of the x-coordinates of the nodes in LON
   |#
   (lambda (elt list) (cons (node-x elt) list))
   empty
   lon))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; get-x-leftmost-node: LOX -> X
;; GIVEN: A list of x
;; RETURNS: the first element of sorted list in ascending order
;; EXAMPLES: (get-x-leftmost-node (list 190 250)) -> 190
;; DESIGN STRATEGY: Combine using simpler function

(define (get-x-leftmost-node lox)
  (first (sort lox <)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-trees : World -> ListOfTree
;; GIVEN: a World
;; RETURNS: a list of all the trees in the given world.
;; Examples: refer tests below
;; DESIGN STRATEGY: Using template for World on w

(define (world-to-trees w)
  (world-lon w))

;; Sample

(define world1 (make-world
                (list
                 (make-node
                  387
                  88
                  SHAPE-SQUARE
                  #false
                  (list
                   (make-node 327 148 SHAPE-SQUARE #false '() 0 0)
                   (make-node 387 148 SHAPE-CIRCLE #false '() 0 0))
                  0
                  0)
                 (make-node
                  236
                  100
                  SHAPE-CIRCLE
                  #false
                  (list (make-node 236 160 SHAPE-CIRCLE #false '() 0 0))
                  0
                  0))))

(define world-output-trees (list
                            (make-node
                             387
                             88
                             SHAPE-SQUARE
                             #false
                             (list
                              (make-node 327 148 SHAPE-SQUARE #false '() 0 0)
                              (make-node 387 148 SHAPE-CIRCLE #false '() 0 0))
                             0
                             0)
                            (make-node
                             236
                             100
                             SHAPE-CIRCLE
                             #false
                             (list (make-node 236 160 SHAPE-CIRCLE #false '() 0 0))
                             0
                             0)))

;; TESTS:

(begin-for-test
  (check-equal? (world-to-trees world1)
                world-output-trees
                "the world-to-trees didn't return the correct result"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tree-to-root : Tree -> Node
;; GIVEN: a tree
;; RETURNS: the node at the root of the tree
;; EXAMPLE: Consider the tree represented as follows:
;
;                 A
;                 |
;       +---+-----+-----+
;       |   |     |     |
;       B   C     D     E
;           |           |
;         +---+      +-----+
;         |   |      |     |
;         F   G      H     I
;
;If tree-to-root is given the subtree rooted at C, it should return the
;data structure associated with node C. This data structure may or may
;not include data associated with rest of the tree, depending on
;whether you have chosen to represent nodes differently from trees.

;; DESIGN STRATEGY: Returns t

(define (tree-to-root t)
  t)

;; Sample for tests

(define tree1 (list
               (make-node
                231
                168
                SHAPE-CIRCLE
                #false
                (list
                 (make-node 171 228 SHAPE-CIRCLE #false '() 0 0)
                 (make-node
                  231
                  228
                  SHAPE-CIRCLE
                  #false
                  (list
                   (make-node 171 288 SHAPE-CIRCLE #false '() 0 0)
                   (make-node 231 288 SHAPE-SQUARE #false '() 0 0))
                  0
                  0))
                0
                0)))

;; TESTS:

(begin-for-test
  (check-equal? (tree-to-root tree1)
                tree1
                "the tree-to-root didn't return the node at the root
                 but should have returned."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tree-to-sons : Tree -> ListOfTree
;; GIVEN: a tree
;; RETURNS: the data associated with the immediate subtrees of the given
;; tree. 
;; EXAMPLE:
;
;                 A
;                 |
;       +---+-----+-----+
;       |   |     |     |
;       B   C     D     E
;           |           |
;         +---+      +-----+
;         |   |      |     |
;         F   G      H     I
;; In the situation above, if tree-to-sons is given the subtreett
;; rooted at C, it should return a list consisting of the subtree rooted
;; at F and the subtree rooted at G.
;; DESIGN STRATEGY: Use template for Node on t


(define (tree-to-sons t)
  (node-lon t))

;; Sample output :

(define tree2 (make-node 250 20 SHAPE-CIRCLE #false
                         (list
                          (make-node 250 80 SHAPE-CIRCLE #false '() 0 0)) 0 0))

(define tree-to-sons-outp (list (make-node 250 80 SHAPE-CIRCLE #false '() 0 0)))


;; TESTS :

(begin-for-test
  (check-equal? (tree-to-sons tree2)
                tree-to-sons-outp
                "tree-to-sons should return the list of immediate subtrees"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; node-to-center : Node -> Posn
;; GIVEN: A node
;; RETURNS: the center of the given node as it is to be displayed on the
;; scene.
;; EXAMPLES: refer tests below
;; DESIGN STRATEGY: Use template for Node on n

(define (node-to-center n)
  (make-posn (node-x n) (node-y n)))


;; Sample :
(define node1 (make-node 171 228 SHAPE-CIRCLE #false '() 0 0))

;; TESTS:

(begin-for-test
  (check-equal? (node-to-center node1)
                (make-posn 171 228)
                "on node1 it should have returned '(make-posn 171 228)'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; node-to-selected? : Node -> Boolean
;; GIVEN: A node
;; RETURNS: true iff the given node is selected.
;; EXAMPLES: refer tests below
;; DESIGN STRATEGY: Using template for Node on n

(define (node-to-selected? n)
  (node-selected? n))

;; TEST:

(begin-for-test
  (check-equal? (node-to-selected? (make-node 200 20 "circle" true '() 0 0))
                true
                "the node-to-selected? should have returned true"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SAMPLES:

(define w1-output-outside (make-world
                           (list
                            (make-node 250 20 SHAPE-SQUARE #false '() 350 20))))
(define w2-button-up (make-world
                      (list
                       (make-node 250 20 SHAPE-SQUARE #true '() 350 20))))
(define w2-output-buttonup (make-world
                            (list
                             (make-node 250 20 SHAPE-SQUARE #false '() 0 0))))
(define w3-drag (make-world
                 (list
                  (make-node 250 20 SHAPE-CIRCLE #true '() 250 20))))

(define w3-output-drag (make-world
                        (list
                         (make-node 350 20 SHAPE-CIRCLE #true '() 350 20))))

(define w6 (make-world
            (list (make-node 250 80 SHAPE-CIRCLE #false '() 250 20))))

(define w6-output-drag
  (make-world
   (list (make-node 250 80 SHAPE-CIRCLE #false '() 250 20))))

(define w8-drag (make-world
                 (list (make-node 250
                                  20
                                  SHAPE-CIRCLE
                                  #true
                                  (list (make-node 250
                                                   80
                                                   SHAPE-CIRCLE
                                                   #true
                                                   '()
                                                   250
                                                   20))
                                  250
                                  20))))
(define w8-drag-output (make-world
                        (list
                         (make-node
                          350
                          20
                          "circle"
                          #true
                          (list (make-node 350 80 "circle" #true '() 350 20))
                          350
                          20))))

(define world4 (make-world
                (list (make-node 250 20 SHAPE-CIRCLE #true '() 0 0))))
(define world4-output (make-world
                       (list
                        (make-node
                         250
                         20
                         SHAPE-CIRCLE
                         #true
                         (list (make-node 250 80 SHAPE-CIRCLE #false '() 0 0))
                         0
                         0))))

(define world5-output (make-world
                       (list
                        (make-node 250 20 SHAPE-CIRCLE #false '() 0 0)
                        (make-node 250 20 SHAPE-SQUARE #false '() 0 0)
                        (make-node
                         387
                         88
                         SHAPE-SQUARE
                         #false
                         (list
                          (make-node 327 148 SHAPE-SQUARE #false '() 0 0)
                          (make-node 387 148 SHAPE-CIRCLE #false '() 0 0))
                         0
                         0)
                        (make-node
                         236
                         100
                         SHAPE-CIRCLE
                         #false
                         (list (make-node 236 160 SHAPE-CIRCLE #false '() 0 0))
                         0
                         0))))

(define world7 (make-world (list (make-node 250
                                            20
                                            SHAPE-CIRCLE
                                            #false
                                            (list
                                             (make-node 327 148
                                                        SHAPE-SQUARE
                                                        #true '()
                                                        0 0))
                                            0
                                            0))))

(define world7-output (make-world (list (make-node 250 20
                                                   SHAPE-CIRCLE
                                                   #false '()
                                                   0 0))))

(define world8 (make-world
                (list
                 (make-node 250
                            20
                            SHAPE-CIRCLE
                            #false
                            (list (make-node 327
                                             148
                                             SHAPE-SQUARE
                                             #true
                                             (list (make-node 327
                                                              148
                                                              SHAPE-SQUARE
                                                              #false
                                                              '()
                                                              0 0)) 0 0))0 0))))

(define world8-output (make-world
                       (list
                        (make-node
                         250
                         20
                         SHAPE-CIRCLE
                         #false
                         (list
                          (make-node
                           327
                           148
                           SHAPE-SQUARE
                           #true
                           (list
                            (make-node 267 208 SHAPE-CIRCLE #false '() 0 0)
                            (make-node 327 148 SHAPE-SQUARE #false '() 0 0))
                           0
                           0))
                         0
                         0))))

(define world8-on-s (make-world
                     (list
                      (make-node
                       250
                       20
                       SHAPE-CIRCLE
                       #false
                       (list
                        (make-node
                         327
                         148
                         SHAPE-SQUARE
                         #true
                         (list
                          (make-node 267 208 SHAPE-SQUARE #false '() 0 0)
                          (make-node 327 148 SHAPE-SQUARE #false '() 0 0))
                         0
                         0))
                       0
                       0))))



;; TESTS:

(begin-for-test
  (check-equal? (world-after-mouse-event w1-button-down 350 20 "button-down")
                w1-output-outside
                "button down outside the shape shouldn't select it")
  (check-equal? (world-after-mouse-event w2-button-up 250 20 "button-up")
                w2-output-buttonup
                "button up failed to unselect node")
  (check-equal? (world-after-mouse-event w3-drag 350 20 "drag")
                w3-output-drag
                "drag failed to drag selected node")
  (check-equal? (world-after-mouse-event w6 350 20 "drag")
                w6-output-drag
                "drag failed to drag selected node")
  (check-equal? (world-after-mouse-event (initial-world "yo") 350 20 "move")
                (initial-world true)
                "other mouse events should leave the world unchanged,but didn't")
  (check-equal? (world-after-mouse-event w8-drag 350 20 "drag")
                w8-drag-output
                "drag event failed to drag the selected node and its subtree")
  (check-equal? (world-after-key-event world4 "c")
                world4-output
                "function should add a new circle to the world")
  (check-equal? (world-after-key-event world1-output "c")
                world5-output
                "function should return the world with new circle added")
  (check-equal? (world-after-key-event world4 "d")
                (make-world '())
                "Function should return the world as it is as no node is present")
  (check-equal? (world-after-key-event world7 "d")
                world7-output
                "function should return the world with the selected node deleted")
  (check-equal? (world-after-key-event world8 "c")
                world8-output
                "function should return the world with a new circle")
  (check-equal? (world-after-key-event world8 "s")
                world8-on-s
                "function should return world with a new square added")
  (check-equal? (world-after-key-event world8 "p")
                world8
                "function should return the world as it is")
  (check-equal? (world-after-key-event (make-world empty) "d")
                (make-world empty)
                "function should return the world as it is")
  (check-equal? (world-after-key-event world7-output "d")
                world7-output
                "function should return the world with selected node deleted"))