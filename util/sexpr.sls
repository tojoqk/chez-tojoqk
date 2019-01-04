(library (tojoqk util sexpr)
  (export single?)
  (import (rnrs))

  (define (single? x)
    (and (pair? x) (null? (cdr x))))
  )
