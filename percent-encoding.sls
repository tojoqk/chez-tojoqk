(library (tojoqk percent-encoding)
  (export percent-encode percent-decode)
  (import (rnrs)
          (only (chezscheme) format))

  (define (degit? x) (<= 48 x 57))
  (define (alpha? x)
    (or (<= 65 x 90)
        (<= 97 x 122)))
  (define (hyphen? x) (= x 45))
  (define (dot? x) (= x 46))
  (define (underscore? x) (= x 95))
  (define (tilda? x) (= x 126))

  (define (unreserved? x)
    (or (degit? x)
        (alpha? x)
        (hyphen? x)
        (dot? x)
        (underscore? x)
        (tilda? x)))

  (define percent (char->integer #\%))

  (define (percent-encode str)
    (let ([utf8 (string->utf8 str)])
      (let ([in (open-bytevector-input-port utf8)])
        (call-with-string-output-port
          (lambda (out)
            (let loop ()
              (let ([u8 (get-u8 in)])
                (cond
                 [(eof-object? u8) 'done]
                 [(unreserved? u8)
                  (put-char out (integer->char u8))
                  (loop)]
                 [else
                  (put-char out #\%)
                  (format out "~2,'0x" u8)
                  (loop)]))))))))

  (define (percent-decode str)
    (let ([in (open-bytevector-input-port (string->utf8 str))])
      (call-with-string-output-port
        (lambda (out)
          (define (decode out)
            (let ([u8 (lookahead-u8 in)])
              (cond
               [(eof-object? u8) 'done]
               [(= u8 percent)
                (get-u8 in)
                (let* ([u81 (get-u8 in)]
                       [u82 (get-u8 in)])
                  (cond
                   [(or (eof-object? u81)
                        (eof-object? u82))
                    (error 'percent-decode
                           "can't parse" str)]
                   [else
                    (put-u8 out
                            (string->number
                             (string (integer->char u81)
                                     (integer->char u82))
                             16))
                    (decode out)]))]
               [else 'done])))
          (let loop ()
            (let ([u8 (lookahead-u8 in)])
              (cond
               [(eof-object? u8) 'done]
               [(= u8 percent)
                (put-string out
                            (utf8->string
                             (call-with-bytevector-output-port decode)))]
               [else
                (get-u8 in)
                (put-char out (integer->char u8))
                (loop)])))))))
  )
