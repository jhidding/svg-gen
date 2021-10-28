; ~\~ language=Scheme filename=monads/monads.scm
; ~\~ begin <<lit/stdlib.md|monads/monads.scm>>[0]
(library (monads monads)
  (export seq make-monad monad? monad-return monad-bind <- ::
          <maybe> maybe-bind maybe-return *nothing* nothing?
          ; <state> state-bind state-return get-state set-state
          ; reader-bind reader-return reader-ask
          ; update-state
          seq-map)

  (import (rnrs (6))
          (monads syntax)
          (monads maybe)
          ; (monads state)
          ; (monads reader)
          (monads support))
)
; ~\~ end
