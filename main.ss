(case-sensitive #t)
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
(define-record <camera> translation target distance view)

(define-record <intersect> time object extra)
(define-record <ray> origin direction)

(load "shaders.ss")
(load "lights.ss")
(load "objects.ss")

(load "image.ss")

(define-syntax fold-list
  (syntax-rules ()
    [(_ [item ls] [acc init] b1 b2 ...)
     (and (identifier? #'item) (identifier? #'acc))
     (fold-left (lambda (acc item) b1 b2 ...)
       init
       ls)]))

(define (traverse-ray ray t)
  (vec-vec-plus (<ray> origin ray) (vec-num-mul (<ray> direction ray) t)))

(define (sort-intersections ls)
  (sort (lambda (x y) (< (<intersect> time x) (<intersect> time y)))
        ls))

(define (find-intersections ray scene)
  (sort-intersections
   (fold-list [obj (<scene> objects scene)] [acc '()]
     (fold-list [t (object-intersections obj ray)] [acc acc]
       ;; Make sure we don't accidently hit the object at the intersect-point
       (if (< (<intersect> time t) EPSILON)
           acc
           (cons t acc))))))

(define scene)                          ; should really be in user env
(define object)                         ; should really be in user env
(define intersect-point)                ; should really be in user env
(define normal)                         ; should really be in user env
(define incoming)                       ; should really be in user env
(define depth)                          ; should really be in user env
(define (object-shade s obj extra ip i d)
  (let ([shader (object-shader obj)])
    (if shader
        (fluid-let ([scene s]
                    [object obj]
                    [intersect-point ip]
                    [normal (object-normal obj extra ip)]
                    [incoming i]
                    [depth d])
          (shader))
        (error 'object-shade "no object shader defined for ~a" obj))))

(define light)                          ; should really be in user env
(define (light-shade l)
  (let ([shader (light-shader l)])
    (if shader
        (fluid-let ([light l])
          (shader))
        (error 'light-shade "no light shader defined for ~a" l))))

(define (pixel-color-from-ray scene ray Kr depth)
  (if (or (= depth 0) (<= Kr 0))
      (make-color 0 0 0)
      (let ([ls (find-intersections ray scene)])
        (if (null? ls)
            (<scene> background-color scene)
            (let ([first (car ls)])
              (match first
                [`(<intersect> [time ,t] [object ,obj] [extra ,extra])
                 (let ([incoming (<ray> direction ray)]
                       [intersect-point (traverse-ray ray t)])
                   (color-num-mul
                    (object-shade scene obj extra intersect-point incoming depth)
                    Kr))]))))))

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

(define-defaults sphere ([color (make-color 1 1 1)] [shader #f]
                         [center (make-vec 0 0 0)] [radius 1]
                         [M (matrix-identity 3)])
  (let ([M (matrix-mul (scale radius radius radius) M)])
    (make-sphere color shader center M (matrix-inverse M))))

(define-defaults plane ([color (make-color 1 1 1)] [shader #f]
                        [center (make-vec 0 0 0)] [M (matrix-identity 3)])
  (make-plane color shader center M (matrix-inverse M)))

(define-defaults cube ([color (make-color 1 1 1)] [shader #f]
                       [center (make-vec 0 0 0)] [M (matrix-identity 3)])
  (make-polyhedron color shader center M (matrix-inverse M)
    (list
     (make-vec 0 0 1) (make-vec 0 0 -1)
     (make-vec 0 1 0) (make-vec 0 -1 0)
     (make-vec 1 0 0) (make-vec -1 0 0))))

(define-defaults tetrahedron ([color (make-color 1 1 1)] [shader #f]
                              [center (make-vec 0 0 0)] [M (matrix-identity 3)])
  (make-polyhedron color shader center M (matrix-inverse M)
    (list
     (make-vec -1 -1 -1)
     (make-vec -1 1 1)
     (make-vec 1 1 -1)
     (make-vec 1 -1 1))))

(define-defaults quadric ([color (make-color 1 1 1)] [shader #f]
                          [center (make-vec 0 0 0)] [M (matrix-identity 3)]
                          [coefficients #f])
  (unless (vector? coefficients)
    (error 'quadric "coefficients are not a vector: ~s" coefficients))
  (unless (= (vector-length coefficients) 10)
    (error 'quadric "incorrect number of coefficients: ~s" coefficients))
  (make-quadric color shader center M (matrix-inverse M) coefficients))

(define-defaults texture ([filename #f])
  (let ([img (read-tga filename)])
    (let ([xt (make-linear-transform 0 1 0 (- (<image> width img) 1))]
          [yt (make-linear-transform 0 1 0 (- (<image> height img) 1))])
      (lambda (s t)
        (image-ref img (exact (truncate (xt s))) (exact (truncate (yt t))))))))
