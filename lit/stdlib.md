# Utilities for Scheme
## Utility

``` {.scheme file=utility/cut.scm}
#| REFERENCE IMPLEMENTATION FOR SRFI-26 "CUT"
 | ==========================================
 |
 | Sebastian.Egner@philips.com, 5-Jun-2002.
 | adapted from the posting by Al Petrofsky <al@petrofsky.org>
 | placed in the public domain
 |
 | The code to handle the variable argument case was originally
 | proposed by Michael Sperber and has been adapted to the new
 | syntax of the macro using an explicit rest-slot symbol. The
 | code to evaluate the non-slots for cute has been proposed by
 | Dale Jordan. The code to allow a slot for the procedure position
 | and to process the macro using an internal macro is based on
 | a suggestion by Al Petrofsky. The code found below is, with
 | exception of this header and some changes in variable names,
 | entirely written by Al Petrofsky.
 |
 | compliance:
 |   Scheme R5RS (including macros).
 |
 | loading this file into Scheme 48 0.57:
 |   ,load cut.scm
 |
 | history of this file:
 |   SE,  6-Feb-2002: initial version as 'curry' with ". <>" notation
 |   SE, 14-Feb-2002: revised for <...>
 |   SE, 27-Feb-2002: revised for 'cut'
 |   SE, 03-Jun-2002: revised for proc-slot, cute
 |   SE, 04-Jun-2002: rewritten with internal transformer (no "loop" pattern)
 |   SE, 05-Jun-2002: replace my code by Al's; substituted "constant" etc.
 |     to match the convention in the SRFI-document
 |
 | (srfi-26-internal-cut slot-names combination . se)
 |   transformer used internally
 |     slot-names  : the internal names of the slots
 |     combination : procedure being specialized, followed by its arguments
 |     se          : slots-or-exprs, the qualifiers of the macro
 |#

(library (utility cut)
  (export cut cute <> <...>)

  (import (rnrs (6))
          (utility aux-keyword))

  (define-auxiliary-keywords <> <...>)

  (define-syntax srfi-26-internal-cut
    (syntax-rules (<> <...>)

      ;; construct fixed- or variable-arity procedure:
      ;;   (begin proc) throws an error if proc is not an <expression>
      ((srfi-26-internal-cut (slot-name ...) (proc arg ...))
       (lambda (slot-name ...) ((begin proc) arg ...)))
      ((srfi-26-internal-cut (slot-name ...) (proc arg ...) <...>)
       (lambda (slot-name ... . rest-slot) (apply proc arg ... rest-slot)))

      ;; process one slot-or-expr
      ((srfi-26-internal-cut (slot-name ...)   (position ...)      <>  . se)
       (srfi-26-internal-cut (slot-name ... x) (position ... x)        . se))
      ((srfi-26-internal-cut (slot-name ...)   (position ...)      nse . se)
       (srfi-26-internal-cut (slot-name ...)   (position ... nse)      . se))))

  ; (srfi-26-internal-cute slot-names nse-bindings combination . se)
  ;   transformer used internally
  ;     slot-names     : the internal names of the slots
  ;     nse-bindings   : let-style bindings for the non-slot expressions.
  ;     combination    : procedure being specialized, followed by its arguments
  ;     se             : slots-or-exprs, the qualifiers of the macro

  (define-syntax srfi-26-internal-cute
    (syntax-rules (<> <...>)

      ;; If there are no slot-or-exprs to process, then:
      ;; construct a fixed-arity procedure,
      ((srfi-26-internal-cute
        (slot-name ...) nse-bindings (proc arg ...))
       (let nse-bindings (lambda (slot-name ...) (proc arg ...))))
      ;; or a variable-arity procedure
      ((srfi-26-internal-cute
        (slot-name ...) nse-bindings (proc arg ...) <...>)
       (let nse-bindings (lambda (slot-name ... . x) (apply proc arg ... x))))

      ;; otherwise, process one slot:
      ((srfi-26-internal-cute
        (slot-name ...)         nse-bindings  (position ...)   <>  . se)
       (srfi-26-internal-cute
        (slot-name ... x)       nse-bindings  (position ... x)     . se))
      ;; or one non-slot expression
      ((srfi-26-internal-cute
        slot-names              nse-bindings  (position ...)   nse . se)
       (srfi-26-internal-cute
        slot-names ((x nse) . nse-bindings) (position ... x)       . se))))

  ; exported syntax

  (define-syntax cut
    (syntax-rules ()
      ((_ . slots-or-exprs)
       (srfi-26-internal-cut () () . slots-or-exprs))))

  (define-syntax cute
    (syntax-rules ()
      ((cute . slots-or-exprs)
       (srfi-26-internal-cute () () () . slots-or-exprs))))
)
```

### Some algorithms
``` {.scheme file=utility/algorithms.scm}
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
```

## Monads
``` {.scheme file=utility/receive.scm}
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
```

``` {.scheme file=utility/aux-keyword.scm}
#| Code snippet from Andy Keep |#
(library (utility aux-keyword)
  (export define-auxiliary-keyword
          define-auxiliary-keywords)

  (import (rnrs (6)))

  (define-syntax define-auxiliary-keyword
    (syntax-rules ()
      [(_ name)
       (define-syntax name
         (lambda (x)
           (syntax-violation #f "misplaced use of auxiliary keyword" x)))]))

  (define-syntax define-auxiliary-keywords
    (syntax-rules ()
      [(_ name* ...)
       (begin (define-auxiliary-keyword name*) ...)]))
)
```

``` {.scheme file=monads/syntax.scm}
(library (monads syntax)
  (export seq make-monad monad? monad-return monad-bind <- ::)

  (import (rnrs (6))
          (utility receive)
          (utility aux-keyword))

  (define-auxiliary-keywords <- ::)

  (define-record-type monad
    (fields bind return))

  (define-syntax seq
    (syntax-rules (<- ::)
      ;; the last expression in a sequence remains as is.
      ((_ <M> <f>)
       <f>)

      ;; (seq M (a <- expression) ...) expands to a nested
      ;; binding to a function that contains the rest of the
      ;; sequence
      ((_ <M>
          (<formals> ... <- <f>)
          <rest> ...)

       ((monad-bind <M>)
        <f>
        (lambda (<formals> ...)
          (seq <M> <rest> ...))))

      ;; (seq M (a :: expression) ...) expands to a nested
      ;; let binding
      ((_ <M>
          (<formals> ... :: <f>)
          <rest> ...)
       
       (call-with-values
         (lambda () <f>)
         (lambda (<formals> ...)
           (seq <M> <rest> ...))))

      ;; If the pattern doesn't match the (a <- expr) pattern,
      ;; the outcome of <f> is thrown away, but we still need
      ;; a lambda for bind to work on.
      ((_ <M>
          <f>
          <rest> ...)

       ((monad-bind <M>)
        <f>
        (lambda _
          (seq <M> <rest> ...))))))
)
```

``` {.scheme file=monads/monads.scm}
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
```

``` {.scheme file=monads/maybe.scm}
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
```


``` {.scheme file=monads/support.scm}
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
```

## Parsing
``` {.scheme file=parsing/text-cursors.scm}
(library (parsing text-cursors)
  (export text-cursor?

          ;; accessors
          text-cursor-string text-cursor-start text-cursor-end

          ;; creation
          make-text-cursor string->text-cursor

          ;; querying
          text-cursor-ref text-cursor-select text-cursor-peek
          text-cursor-null?

          ;; manipulation
          text-cursor-next text-cursor-flush text-cursor-forward)

  (import (rnrs (6)))

  (define-record-type text-cursor
    (fields string start end))

  (define (text-cursor-null? tc)
    (>= (text-cursor-end tc)
        (string-length (text-cursor-string tc))))

  (define (string->text-cursor text)
    (make-text-cursor text 0 0))

  (define (text-cursor-ref tc)
    (string-ref (text-cursor-string tc)
                (text-cursor-end tc)))

  (define (text-cursor-select tc)
    (substring (text-cursor-string tc)
               (text-cursor-start tc)
               (text-cursor-end tc)))

  (define (text-cursor-peek tc n)
    (text-cursor-select (text-cursor-forward tc n)))

  (define (text-cursor-next tc)
    (make-text-cursor
     (text-cursor-string tc)
     (text-cursor-start tc)
     (+ 1 (text-cursor-end tc))))

  (define (text-cursor-forward tc n)
    (make-text-cursor
     (text-cursor-string tc)
     (text-cursor-start tc)
     (min (string-length (text-cursor-string tc))
          (+ n (text-cursor-end tc)))))

  (define (text-cursor-flush tc)
    (make-text-cursor
     (text-cursor-string tc)
     (text-cursor-end tc)
     (text-cursor-end tc)))
)
```

``` {.scheme file=parsing/parsing.scm}
(library (parsing parsing)
  (export
   ;; <parsing> monad
   <parsing> parsing-bind parsing-return

   ;; elementary parsers
   item fail choice optional
   pop push cons-top update-top
   one many many* some many-char many-char* some-char* some-char
   literal sep-by satisfies
   flush ignore

   ;; characters
   char= char!= word space space? char-in integer

   ;; practical parsers
   tokenize many-end-with* enclosed look-ahead

   ;; parser record
   parser? parser-name parser-call make-parser

   ;; utility
   parse-string)

  (import (rnrs (6))

          (utility receive)
          (utility algorithms)

          (monads monads)
          (parsing text-cursors))

  (define-record-type failure
    (fields msg stack))

  (define-record-type parser
    (fields name* precedence function))

  (define (parser-call p c a)
    (if (procedure? p)
        (p c a)
        ((parser-function p) c a)))

  (define (parser-name p)
    (if (procedure? p)
        "<?>"
        (parser-name* p)))

  (define (parsing-bind parser f)
    (lambda (cursor aux)
      (receive (result cursor aux)
          (parser-call parser cursor aux)
        (if (failure? result)
            (values result cursor aux)
            (parser-call (f result) cursor aux)))))

  (define (parsing-return value)
    (lambda (cursor aux)
      (values value cursor aux)))

  (define <parsing> (make-monad parsing-bind parsing-return))

  (define (parse-string parser string)
    (let ((cursor (string->text-cursor string)))
      (receive (result cursor aux)
          (parser-call parser cursor '())
        result)))

  (define item
    (lambda (cursor aux)
      (if (text-cursor-null? cursor)
          (values (make-failure "end of text" (list item))
                  cursor
                  aux)
          (values (text-cursor-ref cursor)
                  (text-cursor-next cursor)
                  aux))))

  (define (choice parser . rest)
    (define (choice2 parser1 parser2)
      (lambda (cursor1 aux1)
        (receive (result cursor2 aux2)
            (parser-call parser1 cursor1 aux1)
          (if (failure? result)
              (parser-call parser2 cursor1 aux1)
              (values result cursor2 aux2)))))

    (fold-left choice2 parser rest))

  (define (fail msg stack)
    (lambda (cursor aux)
      (values (make-failure msg stack) cursor aux)))

  (define optional
    (case-lambda
      ((parser)         (optional parser *nothing*))
      ((parser default) (choice parser (parsing-return default)))))

  (define pop
    (case-lambda
      (() (pop reverse))
      ((transfer)
       (lambda (cursor aux)
         (values (transfer (car aux)) cursor (cdr aux))))))

  (define (push-cursor)
    (lambda (cursor aux)
      (values *nothing* cursor (cons cursor aux))))

  (define (pop-cursor)
    (lambda (cursor aux)
      (values *nothing* (car aux) (cdr aux))))

  (define (push value)
    (lambda (cursor aux)
      (values *nothing* cursor (cons value aux))))

  (define (update-top transfer)
    (lambda (cursor aux)
      (values *nothing* cursor (cons (transfer (car aux)) (cdr aux)))))

  (define (cons-top value)
    (update-top (lambda (aux) (cons value aux))))

  (define (set-aux new-aux)
    (lambda (cursor aux)
      (values *nothing* cursor new-aux)))

  (define (get-aux)
    (lambda (cursor aux)
      (values aux cursor aux)))

  (define (ignore parser)
    (seq <parsing>
         (x <- (get-aux))
         parser
         (set-aux x)))

  (define flush
    (case-lambda
      (() (flush values))
      ((transfer)
       (lambda (cursor aux)
         (values (transfer (text-cursor-select cursor))
                 (text-cursor-flush cursor)
                 aux)))))

  (define (many* parser)
    (optional
     (seq <parsing>
          (x <- parser)
          (if (not (nothing? x))
              (cons-top x) (parsing-return *nothing*))
          (many* parser))))

  (define many
    (case-lambda
      ((parser)          (many parser reverse))
      ((parser transfer) (seq <parsing>
                              (push '())
                              (many* parser)
                              (pop transfer)))))

  (define some
    (case-lambda
      ((parser)          (some parser reverse))
      ((parser transfer) (seq <parsing>
                              (push '())
                              (x <- parser)
                              (cons-top x)
                              (many* parser)
                              (pop transfer)))))

  (define (many-char* parser)
    (optional
     (seq <parsing>
          parser
          (many-char* parser))))

  (define many-char
    (case-lambda
      ((parser)          (many-char parser values))
      ((parser transfer) (seq <parsing>
                              (flush)
                              (many-char* parser)
                              (flush transfer)))))

  (define (one parser)
    (seq <parsing>
         (x <- parser)
         (flush)
         (parsing-return x)))

  (define (some-char* p)
    (seq <parsing>
         p
         (optional (some-char* p))))

  (define some-char
    (case-lambda
      ((parser) (some-char parser values))
      ((parser transfer) (seq <parsing>
                              (flush)
                              parser
                              (many-char* parser)
                              (flush transfer)))))

  (define (literal string)
    (let ((l (string-length string)))
      (lambda (cursor aux)
        (let ((text (text-cursor-peek cursor l)))
          (if (and text (string=? text string))
              (values string (text-cursor-flush
                              (text-cursor-forward cursor l)) aux)
              (values (make-failure text
                                    (list `(literal ,string)))
                      cursor aux))))))

  (define (sep-by parser sep)
    (optional
     (seq <parsing>
          (a  <- parser)
          (as <- (many (seq <parsing> sep parser)))
          (parsing-return (cons a as)))
     '()))

  (define (satisfies parser predicate)
    (seq <parsing>
         (result <- parser)
         (if (predicate result)
             (parsing-return result)
             (fail "" '()))))

  (define (char-in lst)
    (let ((lst (if (string? lst) (string->list lst) lst)))
      (lambda (char)
        (memq char lst))))

  (define (char= . cs)
    (satisfies item (char-in cs)))

  (define (char!= . cs)
    (satisfies item (lambda (c) (not ((char-in cs) c)))))

  (define space?
    (many-char (satisfies item char-whitespace?)))

  (define integer
    (some-char (satisfies item char-numeric?) string->number))

  (define space
    (some-char (satisfies item char-whitespace?)))

  (define word
    (some-char (satisfies item char-alphabetic?)))

  (define (tokenize parser)
    (seq <parsing>
         ; space?
         (x <- parser)
         space?
         (parsing-return x)))

  (define (many-end-with* parser str)
    (choice
     (literal str)
     (seq <parsing>
          parser
          (many-end-with* parser str))))

  (define (look-ahead parser ahead)
    (seq <parsing>
         (x <- parser)
         (push-cursor)
         ahead
         (pop-cursor)
         (parsing-return x)))

  (define (many-end-with-exc* parser end transfer)
    (choice
     (seq <parsing> (x <- (flush transfer)) end (parsing-return x))
     (seq <parsing> parser (many-end-with-exc* parser end transfer))))

  (define (enclosed start parser end transfer)
    (seq <parsing>
         start
         (flush)
         (many-end-with-exc* parser end transfer)))
)
```

## Pattern matching

### History
This is a new version of pmatch (August 8, 2012).

It has two important new features:
1.  It allows for a name to be given to the pmatch if an error ensues.
2.  A line from the specification has been removed. (see below).  Without
that line removed, it was impossible for a pattern to be (quote ,x),
which might be worth having especially when we write an interpreter
for Scheme, which includes quote as a language form.

Code written by Oleg Kiselyov (http://pobox.com/~oleg/ftp/)

Taken from leanTAP.scm
 http://kanren.cvs.sourceforge.net/kanren/kanren/mini/leanTAP.scm?view=log

### Explanation
A simple linear pattern matcher. It is efficient (generates code at macro-expansion time) and simple:
it should work on any R5RS (and R6RS) Scheme system.

```
(pmatch exp <clause> ...[<else-clause>])
<clause> ::= (<pattern> <guard> exp ...)
<else-clause> ::= (else exp ...)
<guard> ::= boolean exp | ()
<pattern> :: =
       ,var  -- matches always and binds the var
                pattern must be linear! No check is done
        _    -- matches always
       'exp  -- comparison with exp (using equal?)    REMOVED (August 8, 2012)
       exp   -- comparison with exp (using equal?)
       (<pattern1> <pattern2> ...) -- matches the list of patterns
       (<pattern1> . <pattern2>)  -- ditto
       ()    -- matches the empty list
```

### Tests

``` {.scheme #test-pmatch}
(require racket/match)
```

``` {.scheme .doctest #test-pmatch}
(match '(1 2 3)
  ((list a b c) b))
---
2
```

### Implementation

``` {.scheme file=utility/pmatch.scm}
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
```

## Formatting
``` {.scheme file=format/format.scm}
(library (format format)
  (export format print println formatter)
  (import (rnrs (6))
          (parsing parsing)
          (monads monads))

  (define-record-type spec
    (fields fill align sign sharp zero width precision type))

  (define identifier
    (seq <parsing>
         (satisfies item char-alphabetic?)
         (many-char* (satisfies item 
                                (lambda (c)
                                        (or (char-alphabetic? c)
                                            (char-numeric? c)
                                            ((char-in "_-") c)))))
         (flush)))

  (define argument
    (choice integer identifier))

  (define parameter
    (seq <parsing>
         (x <- argument)
         (literal "$")
         (parsing-return x)))

  (define count
    (choice parameter integer))

  (define spec-parser
    (seq <parsing>
         (fill      <- (optional (look-ahead
                                  (one item)
                                  (one (char= #\< #\^ #\>)))))
         (align     <- (optional (one (char= #\< #\^ #\>))))
         (sign      <- (optional (one (char= #\+ #\-))))
         (sharp     <- (optional (one (char= #\#))))
         (zero      <- (optional (one (char= #\0))))
         (width     <- (optional (choice count (one (char= #\*)))))
         (precision <- (optional (seq <parsing> (one (char= #\.)) count)))
         (type      <- (optional identifier))
         (parsing-return
           (make-spec fill align sign sharp zero width precision type))))

  (define escape
    (choice (seq <parsing>
                 (literal "{{")
                 (parsing-return "{"))
            (seq <parsing>
                 (literal "}}")
                 (parsing-return "}"))))

  (define clause
    (seq <parsing>
         (literal "{")
         (arg <- (optional argument))
         (fmt <- (optional (seq <parsing>
                                (literal ":")
                                spec-parser)))
         (literal "}")
         (parsing-return (cons arg fmt))))

  (define text
    (some-char (char!= #\{ #\})))

  (define format-string-parser
    (many (choice text escape clause)))

  (define (parse-format-string str)
    (parse-string
     format-string-parser
     str))

  (define (format-value spec value port)
    ;;(display "formating: ") (write value) (display ", type: ")
    ;;(write (if (nothing? spec) "<nothing>" (spec-type spec))) (newline)
    (cond
     ((nothing? spec)            (display value port))
     ((equal? (spec-type spec) "s") (write value port))
     ((equal? (spec-type spec) "f") (write (inexact value) port))
     (else (display value port))))

  (define (format-arguments fmt-lst args port)
    (let loop ((iter-args args)
               (items     fmt-lst))
      (cond
       ((null? items)         #f)

       ((string? (car items))
        (display (car items) port)
        (loop iter-args (cdr items)))

       ((pair?   (car items))
        (cond
         ((nothing? (caar items))
          (when (null? iter-args)
            (raise "format: not enough arguments"))
          (format-value (cdar items)
                        (car iter-args)
                        port)
          (loop (cdr iter-args)
                (cdr items)))

         ((number? (caar items))
          (when (>= (caar items) (length args))
            (raise "format: not enough arguments"))
          (format-value (cdar items)
                        (list-ref args (caar items))
                        port)
          (loop iter-args
                (cdr items)))

         ((string? (caar items))
          (raise "format: named arguments not supported"))

         (else (raise "format: sanity failure"))))

       (else (raise "format: sanity failure")))))

  (define (formatter str)
    (let ((fmt-lst (parse-format-string str)))
      (lambda args
        (let-values (((output g) (open-string-output-port)))
          (format-arguments fmt-lst args output)
          (g)))))

  (define (format str . args)
    (apply (formatter str) args))

  (define (print str . args)
    (display (apply (formatter str) args)))

  (define (println str . args)
    (apply print str args) (newline))
)
```
