; ~\~ language=Scheme filename=utility/receive.scm
; ~\~ begin <<lit/stdlib.md|utility/receive.scm>>[0]
(library (utility receive)
  (export receive)
  (import (rnrs (6)))

  ;;; (srfi :8 receive)
  (define-syntax receive
    (syntax-rules ()
      ((_ <formals> <expr> <body> ...)
       (call-with-values
         (lambda () <expr>)
         (lambda <formals> <body> ...)))))
)
; ~\~ end
