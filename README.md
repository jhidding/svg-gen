---
title: Programmable XML from Scheme
author: Johan Hidding
---

This Scheme script translates S-expressions to XML. This lets you use the full (R6RS) Scheme language to generate any XML document. This script is fully R6RS compatible, but it does use some libraries that are specific to Guile, namely `(ice-9 format)` and `(ice-9 match)`.

```bash
guile xml-gen.scm <<EOF | xmlindent -i 2
'((html)
  (h1) "Hello, World!" (/h1)
  (p) "This is translated into HTML." (/p)
  (/html))
EOF
```

Gives output:

```html
<html>
  <h1>
    Hello, World!
  </h1>
  <p>
    This is translated into HTML.
  </p>
</html>
```

The input can be any number of Scheme expressions; they will be evaluated as if contained in a `(begin ...)` block.

I use this to create programmable SVG documents. If you want to do anything similar for a serious project, consider using something less hacky. The upshot of this implementation is that it is extremely trivial in Scheme.

## Source
I use the `match` macro to match the S-expressions to well known XML patterns. Specifically, I match for

- `(tag attr1: "value" attr2: "value") ... (/tag)`
- `(tag attr1: "value" /)`
- `(/tag)`

and translate those to their XML equivalents (incidentally, the more obscure `(?xml ... ?)` header also parses correctly).

``` {.scheme file=xml-gen.scm #main}
(import (rnrs)
        (rnrs eval)
        (ice-9 match)
        (ice-9 format))
```

We read all input from standard input. The `read-all` function returns the corresponding S-expression.

``` {.scheme #main}
(define (read-all)
  (do ((x (read) (read))
       (r '() (cons x r)))
      ((eof-object? x) (reverse r))))
```

Keywords are symbols that end with a colon.

``` {.scheme #main}
(define (string-ends-with? c s)
  (eq? c (string-ref s (- (string-length s) 1))))

(define (keyword? obj)
  (and (symbol? obj) (string-ends-with? #\: (symbol->string obj))))

(define (keyword->string obj)
  (let ((str (symbol->string obj)))
    (substring str 0 (- (string-length str) 1))))
```

The `kwargs->attrs` function translates a list of arguments to XML format. For example:

``` {.scheme}
(kwargs->attrs '(a: 1 b: 2 c: "hello"))
=> ("a=\"1\" b=\"2\" c=\"hello\")
```

``` {.scheme #main}
(define (kwargs->attrs lst)
  (let loop ((lst lst)
             (r   '()))
    (match lst
      (((? keyword? kw) arg . rest)
       (loop rest
             (cons (format #f "~a=\"~a\"" (keyword->string kw) arg) r)))
      (('/) (reverse (cons "/" r)))
      ((a . rest)
       (loop rest
             (cons (format #f "~a" a) r)))
      (()  (reverse r)))))
```

Anything that is not a list is kept as is.

``` {.scheme #main}
(define (xmlize expr)
  (match expr
    ((tag)           (string-append "<" (symbol->string tag) ">"))
    ((tag . kwargs)  (string-append "<" (symbol->string tag) " " (string-join (kwargs->attrs kwargs) " ") ">"))
    (a               a)))
```

Any `(import ...)` statements at the start are extracted and used to create the environment in which the rest of the document is evaluated.

``` {.scheme #main}
(define (run code)
  (match code
    ((('import . <imports> ) . <program>)
     (eval (cons 'begin <program>)
           (apply environment '(rnrs) <imports>)))
    (<program>
     (eval (cons 'begin <program>)
           (environment '(rnrs))))))

(let* ((src  (read-all))
       (expr (run src)))
  (display (string-join (map xmlize expr) "\n")) (newline))
```
