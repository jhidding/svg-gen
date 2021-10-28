(import (rnrs)
        (rnrs eval))
        ;; (only (chezscheme) pretty-print))

(define (read-all)
  (do ((x (read) (read))
       (r '() (cons x r)))
      ((eof-object? x) (reverse r))))

(define (xmlize expr)
  (

(let ((src (read-all)))
  (display (eval (list 'quasiquote src)
                 (environment '(rnrs) '(lib)))))
