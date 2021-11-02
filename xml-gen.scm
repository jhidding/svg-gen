; ~\~ language=Scheme filename=xml-gen.scm
; ~\~ begin <<README.md|main>>[0]
(import (rnrs)
        (rnrs eval)
        (utility pmatch)
        (format format))
; ~\~ end
; ~\~ begin <<README.md|main>>[1]
(define (read-all)
  (do ((x (read) (read))
       (r '() (cons x r)))
      ((eof-object? x) (reverse r))))
; ~\~ end
; ~\~ begin <<README.md|main>>[2]
(define (string-ends-with? c s)
  (eq? c (string-ref s (- (string-length s) 1))))

(define (keyword? obj)
  (and (symbol? obj) (string-ends-with? #\: (symbol->string obj))))

(define (keyword->string obj)
  (let ((str (symbol->string obj)))
    (substring str 0 (- (string-length str) 1))))
; ~\~ end
; ~\~ begin <<README.md|main>>[3]
(define (kwargs->attrs lst)
  (let loop ((lst lst)
             (r   '()))
    (pmatch lst
      ((,kw ,arg . ,rest) (guard (keyword? kw))
       (loop rest
             (cons (format "{}=\"{}\"" (keyword->string kw) arg) r)))
      ((/) (reverse (cons "/" r)))
      ((,a . ,rest)
       (loop rest
             (cons (format "{}" a) r)))
      (()  (reverse r)))))
; ~\~ end
; ~\~ begin <<README.md|main>>[4]
(define (xmlize expr)
  (pmatch expr
    ((,tag)           (string-append "<" (symbol->string tag) ">"))
    ((,tag . ,kwargs) (string-append "<" (symbol->string tag) " " (string-join (kwargs->attrs kwargs) " ") ">"))
    (,a               a)))
; ~\~ end
; ~\~ begin <<README.md|main>>[5]
(let* ((src  (read-all))
       (expr (eval (cons 'begin src)
                   (environment '(rnrs) '(utility algorithms) '(format format)))))
  (display (string-join (map xmlize expr) "\n")) (newline))
; ~\~ end
