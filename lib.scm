(library (lib)
  (export range)
  (import (rnrs (6)))

  (define range
    (case-lambda
      ((n) (range 0 n 1))
      ((n m) (range n m 1))
      ((n m s) (do ((x n (+ x s))
                    (r '() (cons x r)))
                   ((= x m) (reverse r))))))
)