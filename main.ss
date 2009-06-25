(case-sensitive #t)
(module (define-scheme-record)
  (import scheme)
  (define-syntax define-scheme-record (identifier-syntax define-record)))
(include "records.ss")
(include "defaults.ss")
(include "math.ss")
(include "color.ss")
(include "noise.ss")

(define EPSILON 0.0005)
(define MAXDEPTH 5)

(define-record <scene> background-color objects lights)

(define-record <view> left right bottom top)
(define-record <camera> translation target distance view)

(define-record <intersect> time object extra)
(define-record <ray> origin direction)

(define-syntax fold-list
  (syntax-rules ()
    [(_ [item ls] [acc init] b1 b2 ...)
     (and (identifier? #'item) (identifier? #'acc))
     (fold-left (lambda (acc item) b1 b2 ...)
       init
       ls)]))

(load "shaders.ss")
(load "lights.ss")
(load "objects.ss")

(load "image.ss")

(define (filterwidth x) 1)
(define (filterwidthp x) 1)

(define (traverse-ray ray t)
  (vec-vec-plus (<ray> origin ray) (vec-num-mul (<ray> direction ray) t)))

(define (sort-intersections ls)
  (sort (lambda (x y) (< (<intersect> time x) (<intersect> time y)))
        ls))

(define (find-intersections ray scene)
  (sort-intersections
   (fold-list [obj (<scene> objects scene)] [acc '()]
     (fold-list [inter (object-intersections obj ray)] [acc acc]
       ;; Make sure we don't accidently hit the object at the intersect-point
       (if (< (<intersect> time inter) EPSILON)
           acc
           (cons inter acc))))))

(define light)                          ; should really be in user env
(define (light-shade l)
  (let ([shader (light-shader l)])
    (if shader
        (fluid-let ([light l])
          (shader))
        (error 'light-shade "no light shader defined for ~a" l))))

(define (opaque? opacity)
  (and (>= (<color> r opacity) 1)
       (>= (<color> g opacity) 1)
       (>= (<color> b opacity) 1)))

(define (pixel-color-from-ray scene ray Kr depth)
  (if (or (= depth 0) (<= Kr 0))
      (make-color 0 0 0)
      (let lp ([ls (find-intersections ray scene)])
        (if (null? ls)
            (<scene> background-color scene)
            (let ([first (car ls)])
              (match first
                [`(<intersect> [time ,t] [object ,obj] [extra ,extra])
                 (let ([incoming (<ray> direction ray)]
                       [intersect-point (traverse-ray ray t)])
                   (let-values ([(intersect-point normal)
                                 (object-displace obj intersect-point
                                   (object-normal obj extra intersect-point))])
                     (let-values ([(color opacity)
                                   (object-shade scene obj extra intersect-point
                                     normal
                                     incoming
                                     depth)])
                       (let ([rest (cdr ls)]
                             [color (color-num-mul color Kr)])
                         (if (opaque? opacity)
                             color
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
                               (color-add color (lp rest))]))))))]))))))

(define (ray-gun width height camera)
  (define view (<camera> view camera))
  (define xt
    (make-linear-transform 0 (- width 1)
      (<view> left view) (<view> right view)))
  (define yt
    (make-linear-transform 0 (- height 1)
      (<view> bottom view) (<view> top view)))
  
  (let ([up (make-vec 0 1 0)]
        [dir (vec-vec-sub (<camera> target camera)
                          (<camera> translation camera))])
    (let* ([u (vec-normalize (vec-cross up dir))]
           [v (vec-normalize (vec-cross dir u))]
           [n (vec-normalize dir)]
           [R (make-matrix 3 3
                (lambda (dm dn a)
                  (a 1 1 (vec-i u))
                  (a 1 2 (vec-j u))
                  (a 1 3 (vec-k u))
                  (a 2 1 (vec-i v))
                  (a 2 2 (vec-j v))
                  (a 2 3 (vec-k v))
                  (a 3 1 (vec-i n))
                  (a 3 2 (vec-j n))
                  (a 3 3 (vec-k n))))]
           [eye (vec-vec-plus
                 (mat-vec-mul R (vec-num-mul n (<camera> distance camera)))
                 (<camera> translation camera))])
      (lambda (x y)
        (<ray> make
          [origin eye]
          [direction (vec-normalize
                      (vec-vec-sub (make-vec (xt x) (yt y) 0) eye))])))))

(define (image-simple width height camera scene depth)
  (let ([shoot-ray (ray-gun width height camera)])
    (make-image width height 0 0
      (lambda (set-pixel)
        (do ([y 0 (+ y 1)]) ((= y height))
          (do ([x 0 (+ x 1)]) ((= x width))
            (set-pixel x y (pixel-color-from-ray scene (shoot-ray x y) 1 depth))))))))

(define (render f filename width height camera scene)
  (let ([image (time (f width height camera scene MAXDEPTH))])
    (write-tga image filename)))

;; Scene Syntax

(define-defaults sphere ([color (make-color 1 1 1)]
                         [opacity (make-color 1 1 1)]
                         [surface #f] [volume #f] [displacement #f]
                         [center (make-vec 0 0 0)]
                         [radius 1]
                         [M (matrix-identity 3)])
  (let ([M (matrix-mul (scale radius radius radius) M)])
    (make-sphere color opacity surface volume displacement
      center M (matrix-inverse M))))

(define-defaults plane ([color (make-color 1 1 1)]
                        [opacity (make-color 1 1 1)]
                        [surface #f] [volume #f] [displacement #f]
                        [center (make-vec 0 0 0)]
                        [M (matrix-identity 3)])
  (make-plane color opacity surface volume displacement
    center M (matrix-inverse M)))

(define-defaults tetrahedron ([color (make-color 1 1 1)]
                              [opacity (make-color 1 1 1)]
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

(define-defaults cube ([color (make-color 1 1 1)]
                       [opacity (make-color 1 1 1)]
                       [surface #f] [volume #f] [displacement #f]
                       [center (make-vec 0 0 0)]
                       [M (matrix-identity 3)])
  (make-polyhedron color opacity surface volume displacement
    center M (matrix-inverse M)
    (list
     (make-vec 0 0 1) (make-vec 0 0 -1)
     (make-vec 0 1 0) (make-vec 0 -1 0)
     (make-vec 1 0 0) (make-vec -1 0 0))))

(define-defaults octahedron ([color (make-color 1 1 1)]
                             [opacity (make-color 1 1 1)]
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

(define-defaults icosahedron ([color (make-color 1 1 1)]
                              [opacity (make-color 1 1 1)]
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
  
(define-defaults quadric ([color (make-color 1 1 1)]
                          [opacity (make-color 1 1 1)]
                          [surface #f] [volume #f] [displacement #f]
                          [center (make-vec 0 0 0)]
                          [M (matrix-identity 3)]
                          [coefficients #f])
  (unless (vector? coefficients)
    (error 'quadric "coefficients are not a vector: ~s" coefficients))
  (unless (= (vector-length coefficients) 10)
    (error 'quadric "incorrect number of coefficients: ~s" coefficients))
  (make-quadric color opacity surface volume displacement
    center M (matrix-inverse M) coefficients))

(define-defaults union ([color (make-color 1 1 1)]
                        [opacity (make-color 1 1 1)]
                        [surface #f] [volume #f] [displacement #f]
                        [center (make-vec 0 0 0)]
                        [M (matrix-identity 3)]
                        [A #f]
                        [B #f])
  (make-csg-union color opacity surface volume displacement
    center M (matrix-inverse M) A B))

(define-defaults intersect ([color (make-color 1 1 1)]
                            [opacity (make-color 1 1 1)]
                            [surface #f] [volume #f] [displacement #f]
                            [center (make-vec 0 0 0)]
                            [M (matrix-identity 3)]
                            [A #f]
                            [B #f])
  (make-csg-intersect color opacity surface volume displacement
    center M (matrix-inverse M) A B))

(define-defaults difference ([color (make-color 1 1 1)]
                             [opacity (make-color 1 1 1)]
                             [surface #f] [volume #f] [displacement #f]
                             [center (make-vec 0 0 0)]
                             [M (matrix-identity 3)]
                             [A #f]
                             [B #f])
  (make-csg-difference color opacity surface volume displacement
    center M (matrix-inverse M) A B))

(define-defaults texture ([filename #f])
  (let ([img (read-texture-file filename)])
    (let ([xt (make-linear-transform 0 1 0 (- (<image> width img) 1))]
          [yt (make-linear-transform 0 1 0 (- (<image> height img) 1))])
      (lambda (s t)
        (image-ref img (exact (truncate (xt s))) (exact (truncate (yt t))))))))

(define-defaults normals ([filename #f])
  (let ([img (read-normals-file filename)])
    (let ([xt (make-linear-transform 0 1 0 (- (<image> width img) 1))]
          [yt (make-linear-transform 0 1 0 (- (<image> height img) 1))])
      (lambda (s t)
        (image-ref img (exact (truncate (xt s))) (exact (truncate (yt t))))))))
