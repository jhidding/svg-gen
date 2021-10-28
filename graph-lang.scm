(import (rnrs)
        (rnrs eval)
        (utility pmatch)
        (format format))
        ;; (only (chezscheme) pretty-print))

(define (read-all)
  (do ((x (read) (read))
       (r '() (cons x r)))
      ((eof-object? x) (reverse r))))

(define (string-ends-with? c s)
  (eq? c (string-ref s (- (string-length s) 1))))

(define (keyword? obj)
  (and (symbol? obj) (string-ends-with? #\: (symbol->string obj))))

(define (keyword->string obj)
  (let ((str (symbol->string obj)))
    (substring str 0 (- (string-length str) 1))))

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

(define (xmlize expr)
  (pmatch expr
    ((,tag)           (string-append "<" (symbol->string tag) ">"))
    ((,tag . ,kwargs) (string-append "<" (symbol->string tag) " " (string-join (kwargs->attrs kwargs) " ") ">"))
    (,a               a)))

(let* ((src  (read-all))
       (expr (eval (cons 'begin src)
                   (environment '(rnrs) '(utility algorithms) '(format format)))))
  (display (string-join (map xmlize expr) "\n")))
