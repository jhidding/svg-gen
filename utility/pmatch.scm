; ~\~ language=Scheme filename=utility/pmatch.scm
; ~\~ begin <<lit/stdlib.md|utility/pmatch.scm>>[0]
(library (utility pmatch)
  (export pmatch)
  (import (rnrs (6)))

(define-syntax pmatch
  (syntax-rules (else guard)
    ((_ v (e ...) ...)
     (pmatch-aux #f v (e ...) ...))
    ((_ v name (e ...) ...)
     (pmatch-aux name v (e ...) ...))))

(define-syntax pmatch-aux
  (syntax-rules (else guard)
    ((_ name (rator rand ...) cs ...)
     (let ((v (rator rand ...)))
       (pmatch-aux name v cs ...)))
    ((_ name v)
     (begin
       (if 'name
           (begin (display "pmatch ") (display 'name) (display " failed") (newline)
                  (display v) (newline))
           (begin (display "pmatch failed") (newline) (display v) (newline)))
       (error 'pmatch "match failed")))
    ((_ name v (else e0 e ...)) (begin e0 e ...))
    ((_ name v (pat (guard g ...) e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch-aux name v cs ...))))
       (ppat v pat (if (and g ...) (begin e0 e ...) (fk)) (fk))))
    ((_ name v (pat e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch-aux name v cs ...))))
       (ppat v pat (begin e0 e ...) (fk))))))

(define-syntax ppat
  (syntax-rules (? comma unquote)
    ((_ v ? kt kf) kt)
    ((_ v () kt kf) (if (null? v) kt kf))
;   ((_ v (quote lit) kt kf) (if (equal? v (quote lit)) kt kf))
    ((_ v (unquote var) kt kf) (let ((var v)) kt))
    ((_ v (x . y) kt kf)
     (if (pair? v)
       (let ((vx (car v)) (vy (cdr v)))
         (ppat vx x (ppat vy y kt kf) kf))
       kf))
    ((_ v lit kt kf) (if (equal? v (quote lit)) kt kf))))
)
; ~\~ end
