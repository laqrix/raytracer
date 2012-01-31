(module (define-scheme-record)
  (import scheme)
  (define-syntax define-scheme-record (identifier-syntax define-record)))
(include "records.ss")
(include "defaults.ss")
(include "math.ss")
(include "color.ss")

(define EPSILON 0.0005)
(define MAXDEPTH 5)

(define-record <scene> background-color objects lights)

(define-record <view> left right bottom top)
(define-record <camera>
  output-width output-height type position target view)
(define-record <display> x-samples y-samples filter x-width y-width gain gamma)

(define-record <intersect> time object extra)
(define-record <ray> origin direction)

(define-syntax fold-list
  (syntax-rules ()
    [(_ [item ls] [acc init] b1 b2 ...)
     (and (identifier? #'item) (identifier? #'acc))
     (fold-left (lambda (acc item) b1 b2 ...)
       init
       ls)]))

(define-syntax list-of-helper
  (syntax-rules ()
    [(_ $acc $exp () $guard)
     (if $guard
         (cons $exp $acc)
         $acc)]
    [(_ $acc $exp ([$var $gen] . $rest) $guard)
     (fold-left
      (lambda (acc $var)
        (list-of-helper acc $exp $rest $guard))
      $acc
      $gen)]))

(define-syntax list-of
  (syntax-rules ()
    [(_ $exp ([$var $gen] . $rest))
     (list-of $exp ([$var $gen] . $rest) #t)]
    [(_ $exp ([$var $gen] . $rest) $guard)
     (reverse
      (list-of-helper '() $exp ([$var $gen] . $rest) $guard))]))

(load "noise.ss")
(load "env.ss")
(load "shaders.ss")
(load "lights.ss")
(load "objects.ss")

(load "image.ss")

(define (filterwidth x) 1)
(define (filterwidthp x) 1)

(define (traverse-ray ray t)
  (vec-add (<ray> origin ray) (vec-num-mul (<ray> direction ray) t)))

(define (find-intersections ray scene)
  (sort-intersections
   (fold-list [obj (<scene> objects scene)] [acc '()]
     (fold-list [inter (object-intersections obj ray)] [acc acc]
       ;; Make sure we don't accidently hit the object at the intersect-point
       (if (< (<intersect> time inter) EPSILON)
           acc
           (cons inter acc))))))

(define (light-shade l)
  (let ([shader (light-shader l)])
    (if shader
        (parameterize ([$light l])
          (shader))
        (errorf 'light-shade "no light shader defined for ~a" l))))

(define (opaque? opacity)
  (and (>= (<color> r opacity) 1)
       (>= (<color> g opacity) 1)
       (>= (<color> b opacity) 1)))

(define (pixel-color-from-ray scene ray Kr depth)
  (if (or (= depth 0) (<= Kr 0))
      black
      (color-num-mul
       (let lp ([ls (find-intersections ray scene)])
         (match ls
           [() (<scene> background-color scene)]
           [(,(intersect <= `(<intersect> ,time ,object ,extra)) . ,rest)
            (let ([incoming (<ray> direction ray)]
                  [intersect-point (traverse-ray ray time)])
              (let*-values
               ([(geometric-normal) (object-normal object extra intersect-point)]
                [(intersect-point normal)
                 (object-displace object intersect-point geometric-normal)]
                [(color opacity)
                 (object-shade object intersect extra intersect-point geometric-normal normal
                   incoming depth)])
               (if (opaque? opacity)
                   color
                   (cond
                    [(object-volume object) =>
                     (lambda (shader)
                       (let-values ([(vol-color vol-opacity)
                                     (volume-shade object
                                       intersect-point incoming
                                       color opacity)])
                         (let ([color (color-add color vol-color)])
                           (if (opaque? vol-opacity)
                               color
                               (color-add color (lp rest))))))]
                    [else
                     (color-add color (lp rest))]))))]))
       Kr)))

(define (ray-gun camera)
  (define view (<camera> view camera))
  (define xt
    (make-linear-transform 0 (- (<camera> output-width camera) 1)
      (<view> left view) (<view> right view)))
  (define yt
    (make-linear-transform 0 (- (<camera> output-height camera) 1)
      (<view> top view) (<view> bottom view)))
  (define (vlincomb3 k1 v1 k2 v2 k3 v3)
    (make-vec
     (+ (* k1 (vec-i v1)) (* k2 (vec-i v2)) (* k3 (vec-i v3)))
     (+ (* k1 (vec-j v1)) (* k2 (vec-j v2)) (* k3 (vec-j v3)))
     (+ (* k1 (vec-k v1)) (* k2 (vec-k v2)) (* k3 (vec-k v3)))))
  (let* ([pos (<camera> position camera)]
         [up (make-vec 0 1 0)]
         [dir (vec-sub (<camera> target camera) pos)]
         [u (vec-normalize (vec-cross dir up))]
         [v (vec-normalize (vec-cross u dir))])
    (match (<camera> type camera)
      [perspective
       (lambda (x y)
         (<ray> make
           [origin pos]
           [direction (vlincomb3 1 dir (xt x) u (yt y) v)]))]
      [orthographic
       (lambda (x y)
         (<ray> make
           [origin (vlincomb3 1 pos (xt x) u (yt y) v)]
           [direction dir]))])))

(define (make-exposure display)
  (define gain (<display> gain display))
  (define gamma (<display> gamma display))
  (if (and (= gain 1) (= gamma 1))
      (lambda (c) c)
      (let ([g (/ 1 gamma)])
        (lambda (c)
          (make-color
           (expt (* (color-r c) gain) g)
           (expt (* (color-g c) gain) g)
           (expt (* (color-b c) gain) g))))))

(define (image-simple camera display scene)
  (define shoot-ray (ray-gun camera))
  (define (f x y)
    (let ([ray (shoot-ray x y)])
      (parameterize ([$E (<ray> origin ray)])
        (pixel-color-from-ray scene ray 1 MAXDEPTH))))
  (define antialias (make-antialias
                     (<display> x-samples display)
                     (<display> y-samples display)
                     (<display> filter display)
                     (<display> x-width display)
                     (<display> y-width display)
                     f))
  (define exposure (make-exposure display))
  (let ([width (<camera> output-width camera)]
        [height (<camera> output-height camera)])
    (parameterize ([$camera camera]
                   [$display display]
                   [$scene scene])
      (make-image width height 0 0
        (lambda (set-pixel)
          (do ([y 0 (+ y 1)]) ((= y height))
            (do ([x 0 (+ x 1)]) ((= x width))
              (set-pixel x y (exposure (antialias x y))))))))))

(define (render f filename camera display scene)
  (let ([image (time (f camera display scene))])
    (write-tga image filename)))

;; Scene Syntax

(define-defaults sphere ([color white]
                         [opacity opaque]
                         [surface #f] [volume #f] [displacement #f]
                         [center (make-vec 0 0 0)]
                         [radius 1]
                         [M (matrix-identity 3)])
  (let ([M (matrix-mul (scale radius radius radius) M)])
    (make-sphere color opacity surface volume displacement
      center M (matrix-inverse M))))

(define-defaults plane ([color white]
                        [opacity opaque]
                        [surface #f] [volume #f] [displacement #f]
                        [center (make-vec 0 0 0)]
                        [M (matrix-identity 3)])
  (make-plane color opacity surface volume displacement
    center M (matrix-inverse M)))

(define-defaults tetrahedron ([color white]
                              [opacity opaque]
                              [surface #f] [volume #f] [displacement #f]
                              [center (make-vec 0 0 0)]
                              [M (matrix-identity 3)])
  (make-polyhedron color opacity surface volume displacement
    center M (matrix-inverse M)
    (list
     (make-vec -1 -1 -1)
     (make-vec -1 1 1)
     (make-vec 1 1 -1)
     (make-vec 1 -1 1))))

(define cube-planes
  (list
   (make-vec 0 0 1) (make-vec 0 0 -1)
   (make-vec 0 1 0) (make-vec 0 -1 0)
   (make-vec 1 0 0) (make-vec -1 0 0)))

(define (cube? object)
  (and (polyhedron? object)
       (eq? (polyhedron-planes object) cube-planes)))

(define-defaults cube ([color white]
                       [opacity opaque]
                       [surface #f] [volume #f] [displacement #f]
                       [center (make-vec 0 0 0)]
                       [M (matrix-identity 3)])
  (make-polyhedron color opacity surface volume displacement
    center M (matrix-inverse M)
    cube-planes))

(define-defaults octahedron ([color white]
                             [opacity opaque]
                             [surface #f] [volume #f] [displacement #f]
                             [center (make-vec 0 0 0)]
                             [M (matrix-identity 3)])
  (make-polyhedron color opacity surface volume displacement
    center M (matrix-inverse M)
    (list
     (make-vec 1 1 1)
     (make-vec -1 1 1)
     (make-vec -1 -1 1)
     (make-vec 1 -1 1)
     (make-vec 1 1 -1)
     (make-vec -1 1 -1)
     (make-vec -1 -1 -1)
     (make-vec 1 -1 -1))))

(define-defaults icosahedron ([color white]
                              [opacity opaque]
                              [surface #f] [volume #f] [displacement #f]
                              [center (make-vec 0 0 0)]
                              [M (matrix-identity 3)])
  (make-polyhedron color opacity surface volume displacement
    center M (matrix-inverse M)
    (let* ([t (/ (- (sqrt 5) 1) 2)]
           [nt (- t)]
           [st (* t t)]
           [nst (- st)])
      (list 
       (make-vec t t t)
       (make-vec 1 0 st)
       (make-vec t nt t)
       (make-vec 0 nst 1)
       (make-vec 0 st 1)
       (make-vec 1 0 nst)
       (make-vec t t nt)
       (make-vec st 1 0)
       (make-vec nst 1 0)
       (make-vec nt t nt)
       (make-vec 0 st -1)
       (make-vec t nt nt)
       (make-vec st -1 0)
       (make-vec nst -1 0)
       (make-vec -1 0 st)
       (make-vec nt t t)
       (make-vec nt nt nt)
       (make-vec 0 nst -1)
       (make-vec nt nt t)
       (make-vec -1 0 nst)))))
  
(define-defaults quadric ([color white]
                          [opacity opaque]
                          [surface #f] [volume #f] [displacement #f]
                          [center (make-vec 0 0 0)]
                          [M (matrix-identity 3)]
                          [coefficients #f])
  (unless (vector? coefficients)
    (errorf 'quadric "coefficients are not a vector: ~s" coefficients))
  (unless (= (vector-length coefficients) 10)
    (errorf 'quadric "incorrect number of coefficients: ~s" coefficients))
  (make-quadric color opacity surface volume displacement
    center M (matrix-inverse M) coefficients))

(define-defaults torus ([color white]
                        [opacity opaque]
                        [surface #f] [volume #f] [displacement #f]
                        [center (make-vec 0 0 0)]
                        [radius 1]
                        [radius2 1/3]
                        [M (matrix-identity 3)])
  (let ([M (matrix-mul (scale radius radius radius) M)])
    (make-torus color opacity surface volume displacement
      center M (matrix-inverse M) radius2)))

(define-defaults union ([color white]
                        [opacity opaque]
                        [surface #f] [volume #f] [displacement #f]
                        [center (make-vec 0 0 0)]
                        [M (matrix-identity 3)]
                        [A #f]
                        [B #f])
  (make-csg-union color opacity surface volume displacement
    center M (matrix-inverse M) A B))

(define-defaults intersect ([color white]
                            [opacity opaque]
                            [surface #f] [volume #f] [displacement #f]
                            [center (make-vec 0 0 0)]
                            [M (matrix-identity 3)]
                            [A #f]
                            [B #f])
  (make-csg-intersect color opacity surface volume displacement
    center M (matrix-inverse M) A B))

(define-defaults difference ([color white]
                             [opacity opaque]
                             [surface #f] [volume #f] [displacement #f]
                             [center (make-vec 0 0 0)]
                             [M (matrix-identity 3)]
                             [A #f]
                             [B #f])
  (make-csg-difference color opacity surface volume displacement
    center M (matrix-inverse M) A B))
