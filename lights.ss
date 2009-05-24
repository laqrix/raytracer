(define-record <point-light> intensity origin)

(define point-light-default
  (<point-light> make
    [intensity (make-color 1 1 1)]
    [origin (make-vec 0 0 0)]))

(define-syntax point-light
  (syntax-rules ()
    [(_ clause ...)
     (<point-light> copy point-light-default clause ...)]))

(define (light-position light)
  (match light
    [`(<point-light> [origin ,x]) x]))

(define (light-intensity light)
  (match light
    [`(<point-light> [intensity ,x]) x]))
