(define-record <light> position shader)

(define light-default
  (<light> make
    [position (make-vec 0 0 0)]
    [shader #f]))

#;;
(define-syntax light
  (syntax-rules ()
    [(_ clause ...)
     (<light> copy light-default clause ...)]))

(define (light-position light)
  (match light
    [`(<light> [position ,x]) x]))

(define (light-shader light)
  (match light
    [`(<light> [shader ,x]) x]))

(define (in-shadow? point light-pos)
    ;; TODO: need to limit intersections to between intersect point
    ;; and light position
    (pair? (find-intersections
            (<ray> make
                   [origin point]
                   [direction (vec-normalize (vec-vec-sub light-pos point))])
            scene)))

(define-shader ambient-light ([color (make-color 1 1 1)] [intensity 1])
  (color-num-mul color intensity))

(define-shader distant-light ([color (make-color 1 1 1)] [intensity 1])
  (if (in-shadow? intersect-point (light-position light))
      (make-color 0 0 0)
      (color-num-mul color intensity)))

(define-shader point-light ([color (make-color 1 1 1)] [intensity 1])
  ;; Light shaders, L is from the light to the surface
  (let ([position (light-position light)])
    (if (in-shadow? intersect-point position)
        (make-color 0 0 0)
        (let ([L (vec-vec-sub intersect-point position)])
          (color-num-mul color (/ intensity (vec-dot L L)))))))
