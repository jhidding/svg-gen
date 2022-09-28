; ~\~ language=Scheme filename=examples/pythagoras.scm
; ~\~ begin <<README.md|pythagoras>>[init]
(import (rnrs (6))
        (srfi srfi-13))

(define (strip-css-comments text)
  (let loop ((result '())
             (text text))
    (if (zero? (string-length text))
      (apply string-append (reverse result))
      (let* ((a (string-contains text "/*"))
             (b (if a (string-contains text "*/" (+ a 2)) #f))
             (chunk (if (and a b) (substring text 0 a) text))
             (remain (if (and a b) (substring text (+ b 2) (string-length text)) "")))
        (loop (cons chunk result) remain)))))

(define style-sheet
  (strip-css-comments
    (call-with-input-file "examples/pythagoras.css" get-string-all)))

(define transform-1 "scale(0.5) translate(100 200) rotate(-45)")
(define transform-2 "scale(0.7) translate(-100 200) rotate(60)")

(define (build-tree depth t1 t2 fig)
  (let loop ((d depth)
             (content '()))
    (if (zero? d)
      content
      (loop 
        (- d 1)
        `((g class: "leaf" data-depth: d) ,@fig 
              (g transform: ,t1) ,@content (/g)
              (g transform: ,t2) ,@content (/g)
          (/g))))))

`((?xml version: "1.0" standalone: "no" ?)
  (svg viewBox: "0 0 500 390"
       xmlns: "http://www.w3.org/2000/svg"
       xmlns:xlink: "http://www.w3.org/1999/xlink")
    (style) ,style-sheet (/style)
    (g transform: "translate(320 310) scale(1 -1)")
    ,@(build-tree 10 transform-1 transform-2 '((circle cx: 0 cy: 0 r: 70 /)))
    (/g)
  (/svg))
; ~\~ end
