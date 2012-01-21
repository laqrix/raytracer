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

(define (visibility point light-pos)
  (let ([ray (<ray> make [origin point] [direction (vec-sub light-pos point)])])
    (let lp ([ls (find-intersections ray ($scene))])
      (match ls
        [() white]
        [(`(<intersect> [time ,t] [object ,obj] [extra ,extra]) . ,rest)
         (let ([incoming (<ray> direction ray)]
               [intersect-point (traverse-ray ray t)])
           (let*-values
            ([(geometric-normal) (object-normal obj extra intersect-point)]
             [(intersect-point normal)
              (object-displace obj intersect-point geometric-normal)]
             [(color opacity)
              (object-shade obj extra intersect-point geometric-normal normal
                incoming ($depth))])
            (if (opaque? opacity)
                black
                (cond
                 [(object-volume obj) =>
                  (lambda (shader)
                    (let-values ([(vol-color vol-opacity)
                                  (volume-shade obj
                                    intersect-point incoming
                                    color opacity)])
                      (let ([color (color-add color vol-color)])
                        (if (opaque? vol-opacity)
                            color
                            (color-add color (lp rest))))))]
                 [else
                  (color-add color (lp rest))]))))]))))

;; Light shaders, L is from the light to the surface
(define-syntax illuminate
  (syntax-rules ()
    [(_ ($from) body ...)
     (let ([from $from])
       (parameterize ([$L (vec-sub P from)])
         body ...))]))

(define (shadow-mul shadow? intersect-point from Cl)
  (if shadow?
      (color-mul Cl (visibility intersect-point from))
      Cl))

(define-light ambient-light ([color white] [intensity 1])
  ([__ambient 1] [__nondiffuse 1] [__nonspecular 1])
  (color-num-mul color intensity))

(define-light distant-light ([color white] [intensity 1] [shadow? #t]) ()
  (let ([from (light-position ($light))])
    (illuminate (from)
      (shadow-mul shadow? P from
        (color-num-mul color intensity)))))

(define-light point-light ([color white] [intensity 1] [shadow? #t]) ()
  (let ([from (light-position ($light))])
    (illuminate (from)
      (shadow-mul shadow? P from
        (color-num-mul color (/ intensity (vec-dot L L)))))))

(define-light spot-light
  ([color white] [intensity 1] [shadow? #t] [target (make-vec 0 0 1)]
   [coneangle 30] [coneangle-delta 5] [beamdistribution 2]) ()
  (let ([from (light-position ($light))])
    (illuminate (from)
      (shadow-mul shadow? P from
        (let* ([A (vec-normalize (vec-sub target from))]
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
              black))))))
