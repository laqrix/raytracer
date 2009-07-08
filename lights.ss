(define-record <light> position shader properties)

(define-syntax define-light
  (syntax-rules ()
    [(_ name ([field default] ...) ([prop prop-val] ...) b1 b2 ...)
     (define-defaults name ([position (make-vec 0 0 0)] [field default] ...)
       (<light> make
         [position position]
         [shader (lambda () b1 b2 ...)]
         [properties '((prop . prop-val) ...)]))]))

(define (light-position light)
  (<light> position light))

(define (light-shader light)
  (<light> shader light))

(define (light-property light property)
  (match (assq property (<light> properties light))
    [(,_ . ,val) val]
    [,_ 0]))

(define (in-shadow? point light-pos)
  (match (find-intersections
          (<ray> make
            [origin point]
            [direction (vec-sub light-pos point)])
          scene)
    [() #f]
    [(`(<intersect> [time ,t]) . ,_)
     (not (or (< t 0) (> t 1)))]))

(define-light ambient-light ([color (make-color 1 1 1)] [intensity 1])
  ([__ambient 1] [__nondiffuse 1] [__nonspecular 1])
  (color-num-mul color intensity))

(define-light distant-light ([color (make-color 1 1 1)] [intensity 1]) ()
  (if (in-shadow? intersect-point (light-position light))
      (make-color 0 0 0)
      (color-num-mul color intensity)))

(define-light point-light ([color (make-color 1 1 1)] [intensity 1]) ()
  ;; Light shaders, L is from the light to the surface
  (let ([position (light-position light)])
    (if (in-shadow? intersect-point position)
        (make-color 0 0 0)
        (let ([L (vec-sub intersect-point position)])
          (color-num-mul color (/ intensity (vec-dot L L)))))))

(define-light spot-light
  ([color (make-color 1 1 1)] [intensity 1] [target (make-vec 0 0 1)]
   [coneangle 30] [coneangle-delta 5] [beamdistribution 2]) ()
  (let ([position (light-position light)])
    (if (in-shadow? intersect-point position)
        (make-color 0 0 0)
        ;; Light shaders, L is from the light to the surface
        (let* ([L (vec-sub intersect-point position)]
               [A (vec-normalize (vec-sub target position))]
               [cosangle (/ (vec-dot L A) (vec-length L))]
               [coneangle (degrees->radians coneangle)])
          (if (< (acos cosangle) coneangle)
              (let* ([cosoutside (cos coneangle)]
                     [cosinside
                      (cos (- coneangle (degrees->radians coneangle-delta)))]
                     [atten
                      (* (/ (expt cosangle beamdistribution) (vec-dot L L))
                         (smoothstep cosoutside cosinside cosangle))])
                (color-num-mul color (* intensity atten)))
              (make-color 0 0 0))))))
