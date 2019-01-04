(library (tojoqk util string)
  (export string-split)
  (import (rnrs))

  ;; don't use (srfi :13) to make it standalone
  (define (string-index str sep start)
    (let ([len (string-length str)]
          [p?
           (cond
            [(char? sep) (lambda (c) (char=? c sep))]
            [(procedure? sep) sep]
            [else (assertion-violation 'string-index
                                       "must be char or predicate"
                                       sep)])])
      (let loop ([i start])
        (cond
         [(>= i len) #f]
         [(p? (string-ref str i)) i]
         [else (loop (+ i 1))]))))

  (define string-split
    (case-lambda
      [(str sep)
       (string-split str sep #f)]
      [(str sep count)
       (let ([start 0] [end (string-length str)])
         (define (extract i)
           (list (substring str i end)))
         (let rec ([i 0] [c 0])
           (cond
            [(and count (= count c)) (extract i)]
            [(string-index str sep i)
             => (lambda (idx)
                  (cons (substring str i idx)
                        (rec (+ idx 1) (+ c 1))))]
            [else
             (extract i)])))]))
  )
