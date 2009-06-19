(define-syntax define-shader
  (syntax-rules ()
    [(_ name ([field default] ...) b1 b2 ...)
     (define-defaults name ([field default] ...)
       (lambda () b1 b2 ...))]))

(define (faceforward V R)
  (if (< (vec-dot V R) (/ pi 4))
      V
      (vec-reverse V)))

(define (reflect I N)
  (vec-vec-sub I (vec-num-mul N (* 2 (vec-dot I N)))))

(define (sample-environment scene P R Kr depth)
  (pixel-color-from-ray scene
    (<ray> make [origin P] [direction R])
    Kr
    (- depth 1)))

(define-syntax fold-lights
  (syntax-rules ()
    [(_ [light scene] [acc init] b1 b2 ...)
     (and (identifier? #'item) (identifier? #'acc))
     (fold-left (lambda (acc light) b1 b2 ...)
       init
       (<scene> lights scene))]))

(define-shader ambient ()
  (fold-lights [light scene] [color (make-color 0 0 0)]
    (let ([amb (light-property light '__ambient)])
      (if (> amb 0)
          (color-add color (color-num-mul (light-shade light) amb))
          color))))

(define-shader diffuse ([N #f])
  ;; Surface shaders, L is from the surface to the light
  (fold-lights [light scene] [color (make-color 0 0 0)]
    (let ([nondiff (light-property light '__nondiffuse)])
      (if (< nondiff 1)
          (let ([L (vec-vec-sub (light-position light) intersect-point)])
            (color-add color
              (color-num-mul (light-shade light)
                (* (- 1 nondiff) (vec-dot (vec-normalize L) N)))))
          color))))

(define-shader specular ([N #f] [eye #f] [roughness #f])
  ;; Surface shaders, L is from the surface to the light
  (fold-lights [light scene] [color (make-color 0 0 0)]
    (let ([nonspec (light-property light '__nonspecular)])
      (if (< nonspec 1)
          (let ([L (vec-vec-sub (light-position light) intersect-point)])
            (let ([H (vec-normalize (vec-vec-plus L eye))])
              (color-add color
                (color-num-mul (light-shade light)
                  (* (- 1 nonspec)
                     (expt (max 0 (vec-dot N H)) (/ 1 roughness)))))))
          color))))
