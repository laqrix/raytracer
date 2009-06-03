(define-record <plane> color shader center M Mi)

(define-defaults plane ([color (make-color 0 0 0)] [shader #f]
                        [center (make-vec 0 0 0)] [M (matrix-identity 3)])
  (<plane> make [color color] [shader shader] [center center]
    [M M] [Mi (matrix-inverse M)]))

(define (plane-intersections ray center Mi)
  (let ([origin (mat-vec-mul Mi (vec-vec-sub (<ray> origin ray) center))]
        [direction (mat-vec-mul Mi (<ray> direction ray))])
    (let ([t (- (/ (vec-k origin) (vec-k direction)))])
      (if (and 
           (<= (abs (+ (vec-i origin) (* t (vec-i direction)))) 1)
           (<= (abs (+ (vec-j origin) (* t (vec-j direction)))) 1))
          (list t)
          '()))))

(define (plane-normal M intersect-point)
  (mat-vec-mul M (make-vec 0 0 1)))
