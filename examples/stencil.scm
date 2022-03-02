(import (ice-9 format)
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

(define-record-type pt
  (fields x y))

(define (:+ a b)
  (make-pt (+ (pt-x a) (pt-x b)) (+ (pt-y a) (pt-y b))))

(define (:- a b)
  (make-pt (- (pt-x a) (pt-x b)) (- (pt-y a) (pt-y b)))) 

(define (:* a s)
  (make-pt (* (pt-x a) s) (* (pt-y a) s)))

(define (normalize p)
  (let ((l (sqrt (+ (* (pt-x p) (pt-x p)) (* (pt-y p) (pt-y p))))))
    (make-pt (/ (pt-x p) l) (/ (pt-y p) l))))

(define (rot-90 p)
  (make-pt (pt-y p) (- (pt-x p))))

(define (arrow x1 y1 x2 y2)
  (let* ((end       (make-pt x2 y2))
         (direction (normalize (:- end (make-pt x1 y1))))
         (head-c    (:- end (:* direction 15)))
         (head-m    (:- end (:* direction 5)))
         (head-r    (:+ head-c (:* (rot-90 direction) 4)))
         (head-l    (:- head-c (:* (rot-90 direction) 4))))
  `((line class: "arrow" x1: ,x1 y1: ,y1
        x2: ,(pt-x head-m) y2: ,(pt-y head-m) /)
    (path class: "arrow-head" d:
      ,(format #f "M ~a ~a Q ~a ~a ~a ~a L ~a ~a z"
               (pt-x head-r) (pt-y head-r)
               (pt-x head-m) (pt-y head-m) (pt-x head-l) (pt-y head-l)
               x2 y2) /))))

(define (box color x y)
  `((rect class: ,(format #f "box ~s" color) x: ,x y: ,y
     width: 50 height: 50 rx: 5 ry: 5 /)))

`((?xml version: "1.0" standalone: "no" ?)
  (svg viewBox: "0 0 485 210" xmlns: "http://www.w3.org/2000/svg")
    (style) ,style-sheet (/style)
    (rect class: "paper" x: "0" y: "0" width: "500" height: "400" /)
    ,@(append-map (lambda (x) (box 'blue (+ 5 (* 70 x)) 5))
                  (range 7))
    ,@(append-map (lambda (x) (box 'yellow (+ 5 (* 70 x)) 155))
                  (range 7))
    (g class: "gray")
    ,@(append-map (lambda (a)
        (append-map (lambda (b)
          (if (and (>= (+ a b) 0) (< (+ a b) 7))
            (arrow (+ 30 (* 70 (+ a b))) 55 (+ 30 (* 70 a)) 155)
            '()))
          (range -1 2)))
        (range 7))
    (/g)
    (g class: "black")
    ,@(append-map (lambda (b)
          (arrow (+ 30 (* 70 (+ 2 b))) 55 (+ 30 (* 70 2)) 155))
        (range -1 2))
    (/g)
  (/svg))