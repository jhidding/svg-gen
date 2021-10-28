; ~\~ language=Scheme filename=monads/support.scm
; ~\~ begin <<lit/stdlib.md|monads/support.scm>>[0]
(library (monads support)
  (export seq-map)
  (import (rnrs (6))
          (monads syntax))

  (define (seq-map M f . args)
    (assert (not (null? args)))
    (let loop ((a args)
               (b '()))
      (if (null? (car a))
        ((monad-return M) (reverse b))
        (seq M
          (x <- (apply f (map car a)))
          (loop (map cdr a) (cons x b))))))
)
; ~\~ end
