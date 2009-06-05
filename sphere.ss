(define-record <sphere> color shader center M Mi)

(define-defaults sphere ([color (make-color 0 0 0)] [shader #f]
                         [center (make-vec 0 0 0)] [radius 1]
                         [M (matrix-identity 3)])
  (let ([M (matrix-mul (scale radius radius radius) M)])
    (<sphere> make [color color] [shader shader] [center center]
      [M M] [Mi (matrix-inverse M)])))

(define (sphere-intersections ray center Mi)
  (let ([origin (mat-vec-mul Mi (vec-vec-sub (<ray> origin ray) center))]
        [direction (mat-vec-mul Mi (<ray> direction ray))])
    (let* ([a (vec-dot direction direction)]
           [b (vec-dot direction origin)]
           [c (- (vec-dot origin origin) 1)]
           [dis (- (* b b) (* a c))])
      (if (>= dis 0)
          (let ([t1 (/ (- (- b) (sqrt dis)) a)]
                [t2 (/ (+ (- b) (sqrt dis)) a)])
            (list t1 t2))
          '()))))

(define (sphere-normal center M intersect-point)
  (mat-vec-mul M (vec-vec-sub intersect-point center)))
