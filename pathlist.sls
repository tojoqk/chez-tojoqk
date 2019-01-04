(library (tojoqk pathlist)
  (export pathlist
          pathlist-parent
          pathlist-base
          pathlist-parent
          pathlist-append
          pathlist-map
          pathlist->unixpath)
  (import (rnrs)
          (only (chezscheme) format)
          (only (tojoqk util string) string-split))

  (define (pathlist . paths)
    (reverse paths))

  (define (pathlist-base path)
    (car path))

  (define (pathlist-parent path)
    (if (null? path)
        '()
        (cdr path)))

  (define (pathlist-map f pl)
    (map f pl))

  (define (pathlist-append p1 p2)
    (reverse (append p2 p1)))

  (define (pathlist->unixpath path)
    (format "/~{~a~^/~}"  path))
  )

