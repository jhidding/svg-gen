---
title: Programmable XML from Scheme
author: Johan Hidding
---

This Scheme script translates S-expressions to XML. This lets you use the full (R6RS) Scheme language to generate any XML document. You may run this using Guile or Chez Scheme.

```bash
guile -L . xml-gen.scm <<EOF | xmlindent -i 2
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
I use the `pmatch` macro to match the S-expressions to well known XML patterns. Specifically, I match for

- `(tag attr1: "value" attr2: "value") ... (/tag)`
- `(tag attr1: "value" /)`
- `(/tag)`

and translate those to their XML equivalents. To do string formatting I use a home brewn `format` utility, using string formatting symilar to that of Rust, Python, C#, i.e. using `{}` as wildcards.

``` {.scheme file=xml-gen.scm #main}
(import (rnrs)
        (rnrs eval)
        (utility pmatch)
        (format format))
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
    (pmatch lst
      ((,kw ,arg . ,rest) (guard (keyword? kw))
       (loop rest
             (cons (format "{}=\"{}\"" (keyword->string kw) arg) r)))
      ((/) (reverse (cons "/" r)))
      ((,a . ,rest)
       (loop rest
             (cons (format "{}" a) r)))
      (()  (reverse r)))))
```

Anything that is not a list is kept as is.

``` {.scheme #main}
(define (xmlize expr)
  (pmatch expr
    ((,tag)           (string-append "<" (symbol->string tag) ">"))
    ((,tag . ,kwargs) (string-append "<" (symbol->string tag) " " (string-join (kwargs->attrs kwargs) " ") ">"))
    (,a               a)))
```

``` {.scheme #main}
(let* ((src  (read-all))
       (expr (eval (cons 'begin src)
                   (environment '(rnrs) '(utility algorithms) '(format format)))))
  (display (string-join (map xmlize expr) "\n")) (newline))
```