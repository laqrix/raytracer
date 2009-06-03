(define-record <sphere> center radius shader color)

(define-defaults sphere ([center (make-vec 0 0 0)] [radius 1]
                         [shader #f] [color (make-color 0 0 0)])
  (<sphere> make [center center] [radius radius] [shader shader] [color color]))

(define (sphere-intersections ray center radius)
  (let* ([radius-squared (* radius radius)]
         [origin->center (vec-vec-sub center (<ray> origin ray))]
         [o2c-squared (vec-dot origin->center origin->center)]
         [closest-approach (vec-dot origin->center (<ray> direction ray))])
    (if (and (>= o2c-squared radius-squared)
             (< closest-approach EPSILON))
        '()
        (let ([half-chord-squared
               (+ radius-squared
                  (* closest-approach closest-approach)
                  (- o2c-squared))])
          (if (> half-chord-squared EPSILON)
              (let ([half-chord (sqrt half-chord-squared)])
                (list (- closest-approach half-chord)
                      (+ closest-approach half-chord)))
              '())))))

(define (sphere-normal center intersect-point)
  (vec-normalize (vec-vec-sub intersect-point center)))
