(define-syntax define-shader
  (syntax-rules ()
    [(_ name ([field default] ...) b1 b2 ...)
     (define-defaults name ([field default] ...)
       (lambda () b1 b2 ...))]))

(define (faceforward N I)
  (if (> (vec-dot I N) 0)
      (vec-reverse N)
      N))

(define (reflect I N)
  (vec-sub I (vec-num-mul N (* 2 (vec-dot I N)))))

(define (refract I N eta)
  ;; eta = external index of refraction / internal index of refraction
  (let* ([IdotN (vec-dot I N)]
         [k (- 1 (* (sqr eta) (- 1 (sqr IdotN))))])
    (if (< k 0)
        (make-vec 0 0 0)
        (vec-sub (vec-num-mul I eta)
          (vec-num-mul N (+ (* eta IdotN) (sqrt k)))))))

(define (fresnel I N eta)
  (define internal-index 1)
  (let* ([external-index eta]
         [N (vec-reverse N)] ;; Reverse N, but it's not entirely clear why.
         [cos-theta1 (vec-dot I N)]
         [k (- 1 (* (sqr eta) (- 1 (sqr cos-theta1))))]
         [reflection (vec-sub I (vec-num-mul N (* 2 cos-theta1)))])
    (if (< k EPSILON)
        (values 1 0 reflection (make-vec 0 0 0))
        (let* ([cos-theta2 (sqrt k)]
               [refraction (vec-add (vec-num-mul I eta)
                             (vec-num-mul N (- cos-theta2 (* eta cos-theta1))))]
               [rs (sqr (/ (- (* external-index cos-theta1)
                              (* internal-index cos-theta2))
                           (+ (* external-index cos-theta1)
                              (* internal-index cos-theta2))))]
               [rp (sqr (/ (- (* internal-index cos-theta1)
                              (* external-index cos-theta2))
                           (+ (* internal-index cos-theta1)
                              (* external-index cos-theta2))))]
               [r (/ (+ rs rp) 2)]
               [t (/ (/ (+ (- 1 rs) (- 1 rp)) 2)
                     (/ (* internal-index cos-theta2)
                        (* external-index cos-theta1)))])
          (values r t reflection refraction)))))

(define (bump-normal pnt normal f)
  (let* ([x (vec-i pnt)]
         [y (vec-j pnt)]
         [z (vec-k pnt)]
         [f0 (f x y z)]
         [fx (f (+ x EPSILON) y z)]
         [fy (f x (+ y EPSILON) z)]
         [fz (f x y (+ z EPSILON))])
    (make-vec
     (- (vec-i normal) (/ (- fx f0) EPSILON))
     (- (vec-j normal) (/ (- fy f0) EPSILON))
     (- (vec-k normal) (/ (- fz f0) EPSILON)))))

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
  (fold-lights [light scene] [color black]
    (let ([amb (light-property light '__ambient)])
      (if (> amb 0)
          (color-add color (color-num-mul (light-shade light) amb))
          color))))

(define-shader diffuse ([N #f])
  ;; Surface shaders, L is from the surface to the light
  (fold-lights [light scene] [color black]
    (let ([nondiff (light-property light '__nondiffuse)])
      (if (< nondiff 1)
          (let ([L (vec-sub (light-position light) intersect-point)])
            (color-add color
              (color-num-mul (light-shade light)
                (* (- 1 nondiff) (vec-dot (vec-normalize L) N)))))
          color))))

(define-shader specular ([N #f] [eye #f] [roughness #f])
  ;; Surface shaders, L is from the surface to the light
  (fold-lights [light scene] [color black]
    (let ([nonspec (light-property light '__nonspecular)])
      (if (< nonspec 1)
          (let ([L (vec-sub (light-position light) intersect-point)])
            (let ([H (vec-normalize (vec-add L eye))])
              (color-add color
                (color-num-mul (light-shade light)
                  (* (- 1 nonspec)
                     (expt (max 0 (vec-dot N H)) (/ 1 roughness)))))))
          color))))
