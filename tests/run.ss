(reset-handler (lambda () (exit 1)))
(let ([dir (cd)])
  (cd "..")
  (load "main.ss")
  (cd dir))

(define (render basename)
  (let ([scene (load-scene (string-append basename ".scene"))]
        [camera (<camera> make
                  [translation (make-vec 0 0 10)]
                  [target (make-vec 0 0 0)]
                  [distance 1]
                  [view (<view> make [left -2] [right 2] [bottom -2] [top 2])])])
    (raytrace 128 128 basename MAXDEPTH camera scene image-simple)))
