#lang racket

(require "solver.rkt" "graph.rkt")

(provide
 k-coloring      ; (->* (graph/c natural-number/c) [(is-a? solver%)] (or/c #f coloring/c))
 valid-coloring? ; (-> graph/c coloring/c boolean?)
)

; Returns true iff the given coloring is correct for the specified graph.
(define (valid-coloring? graph coloring)
  (and (coloring/c coloring)
       (= (color-size coloring) (node-count graph))
       (for*/and ([(e n) (in-indexed graph)] [child e])
         (not (= (color-ref coloring n) (color-ref coloring child))))))

; Returns a coloring/c if the given graph can 
; be colored with k colors.  Otherwise returns #f.
(define (k-coloring graph k)
  (define first-const (first-constraint (node-count graph) k 0))
  (define last-const (second-constraint (edges graph) first-const))
  (solve (append first-const last-const)))

; Helper to create the first constraint
(define (make-list-of-constraints startNum kLeft)
  (cond
    [(= kLeft 0) '()]
    [else (cons startNum (make-list-of-constraints (+ startNum 1) (- kLeft 1)))]))

; Create an incremental list of lists of numbers representing the first constraint
; I.e., all nodes should have a color assigned to it
(define (first-constraint nodes k nodeCounter)
  (cond
    [(= nodes nodeCounter) '()]
    [else (cons (make-list-of-constraints (+ (* nodeCounter k) 1) k) (first-constraint nodes k (+ 1 nodeCounter)))]))


; Takes in two lists and creates a list contraints for each color pair
(define (unique-color-constraint listA listB)
  (cond
    [(empty? listA) '()]
    [else (cons (list (* -1 (first listA)) (* -1 (first listB) )) (unique-color-constraint (rest listA) (rest listB)))]))

; Creates a constraint that makes sure each neighboring node does not contain the same color
(define (second-constraint edge-stream first-constraint-list)
  (cond
    [(stream-empty? edge-stream) '()]
    [else (list (unique-color-constraint (list-ref first-constraint-list (first (stream-first edge-stream))) (list-ref first-constraint-list (second(stream-first edge-stream) )) ) (second-constraint (stream-rest edge-stream) first-constraint-list))]))