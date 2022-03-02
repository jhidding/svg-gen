(import (rnrs io ports)
        (ice-9 format)
        (srfi srfi-1))

(define range
  (case-lambda
    ((b) (range 0 b 1))
    ((a b) (range a b 1))
    ((a b step)
     (do ((x a (+ x step))
          (r '() (cons x r)))
         ((>= x b) (reverse r))))))

(define style-sheet
  (call-with-input-file "examples/style.css" get-string-all))

(define (map-element x y)
  `((g transform: ,(format #f "translate(~a ~a)" x y))
    (g class: "black")
      (line class: "arrow" x1: "25" y1: "50" x2: "25" y2: "145" /)
      (path class: "arrow-head" d: "M 20 135 Q 25 150 30 135 L 25 150 z" /)
    (/g)
    (rect class: "box blue" x: "0" y: "0" width: "50" height: "50" rx: "5" ry: "5" /)
    (rect class: "box yellow" x: "0" y: "150" width: "50" height: "50" rx: "5" ry: "5" /)
    (/g)))

`((?xml version: "1.0" standalone: "no" ?)
  (svg viewBox: "0 0 485 210" xmlns: "http://www.w3.org/2000/svg")
    (style) ,style-sheet (/style)
    (rect class: "paper" x: "0" y: "0" width: "500" height: "400" /)
    ,@(append-map (lambda (x) (map-element (+ 5 (* 70 x)) 5))
                  (range 7))
  (/svg))