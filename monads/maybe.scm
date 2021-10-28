; ~\~ language=Scheme filename=monads/maybe.scm
; ~\~ begin <<lit/stdlib.md|monads/maybe.scm>>[0]
(library (monads maybe)
  (export nothing? *nothing* maybe-bind maybe-return <maybe>)

  (import (rnrs (6))
          (monads syntax))

  (define-record-type nothing)

  (define *nothing* (make-nothing))

  (define (maybe-bind value f)
    (if (nothing? value)
        value
        (f value)))

  (define maybe-return values)

  (define <maybe> (make-monad maybe-bind maybe-return))
)
; ~\~ end
