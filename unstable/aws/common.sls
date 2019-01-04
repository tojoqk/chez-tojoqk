(library (tojoqk unstable aws common)
  (export current-access-key-id
          current-secret-access-key
          current-region)
  (import (chezscheme))

  (define current-access-key-id
    (make-parameter (getenv "AWS_ACCESS_KEY_ID")))

  (define current-secret-access-key
    (make-parameter (getenv "AWS_SECRET_ACCESS_KEY")))

  (define current-region
    (make-parameter
     (cond
      [(getenv "AWS_DEFAULT_REGION") => values]
      [else "us-east-1"])))
  )
