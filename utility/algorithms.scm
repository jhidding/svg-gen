; ~\~ language=Scheme filename=utility/algorithms.scm
; ~\~ begin <<lit/stdlib.md|utility/algorithms.scm>>[0]
(library (utility algorithms)
  (export append-reverse append-map string-join unfold range iterate-n)
  (import (rnrs (6)))

  (define (append-reverse rev-head tail)
    (if (null? rev-head)
        tail
        (append-reverse
         (cdr rev-head)
         (cons (car rev-head) tail))))

  (define (append-map f . args)
    (apply append (apply map f args)))

  (define (iterate-n f x n)
    (if (zero? n)
      x
      (iterate-n f (f x) (- n 1))))

  (define range
    (case-lambda
      ((n) (range 0 n 1))
      ((n m) (range n m 1))
      ((n m s) (do ((x n (+ x s))
                    (r '() (cons x r)))
                   ((= x m) (reverse r))))))

  #|
  (define (fold-left f acc . lsts)
    (if (null? (car lsts))
        acc
        (apply
         fold-left
         f
         (apply f acc (map car lsts))
         (map cdr lsts))))

  (define (fold-right f acc . lsts)
    (reverse
     (fold-left
      (lambda (acc . args)
        (apply f (append-reverse args (list acc))))
      acc
      (map reverse lsts))))
  |#

  (define unfold
    (case-lambda
      ((p f g seed) (unfold p f g seed (lambda (x) '())))
      ((p f g seed tail-gen)
       (do ((x seed (g x))
            (result '() (cons (f x) result)))
           ((p x) (cons (tail-gen x) (reverse result)))))))

  (define (string-join lst sep)
    (do ((result ""   (string-append result sep (car lst)))
         (lst    lst  (cdr lst)))
        ((null? lst) result)))
)
; ~\~ end
