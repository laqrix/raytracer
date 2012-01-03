(include "matrix.ss")

(define pi (acos -1))

(define (radians->degrees x)
  (/ (* x 180) pi))

(define (degrees->radians x)
  (/ (* x pi) 180))

(define (sqr x) (* x x))

(define (clamp x xmin xmax)
  (max xmin (min x xmax)))

(define (mix-num c0 c1 x)
  (+ (* c0 (- 1 x)) (* c1 x)))

(define (make-linear-transform in-min in-max out-min out-max)
  (let ([delta (/ (- out-max out-min) (- in-max in-min))])
    (lambda (x)
      (+ (* (- x in-min) delta) out-min))))

;; Vector
(define-record <vec> i j k)

(define (make-vec i j k) (<vec> make [i i] [j j] [k k]))

(define (vec? v) (<vec> is? v))
(define (vec-i v) (<vec> i v))
(define (vec-j v) (<vec> j v))
(define (vec-k v) (<vec> k v))

(define vec-add
  (case-lambda
   [(v1) v1]
   [(v1 v2)
    (make-vec
     (+ (<vec> i v1) (<vec> i v2))
     (+ (<vec> j v1) (<vec> j v2))
     (+ (<vec> k v1) (<vec> k v2)))]
   [(v1 v2 . rest) (apply vec-add (vec-add v1 v2) rest)]))

(define vec-sub
  (case-lambda
   [(v1) v1]
   [(v1 v2)
    (make-vec
     (- (<vec> i v1) (<vec> i v2))
     (- (<vec> j v1) (<vec> j v2))
     (- (<vec> k v1) (<vec> k v2)))]
   [(v1 v2 . rest) (apply vec-sub (vec-sub v1 v2) rest)]))

(define vec-mul
  (case-lambda
   [(v1) v1]
   [(v1 v2)
    (make-vec
     (* (<vec> i v1) (<vec> i v2))
     (* (<vec> j v1) (<vec> j v2))
     (* (<vec> k v1) (<vec> k v2)))]
   [(v1 v2 . rest) (apply vec-mul (vec-mul v1 v2) rest)]))

(define (vec-num-add v n)
  (make-vec (+ (<vec> i v) n) (+ (<vec> j v) n) (+ (<vec> k v) n)))

(define (vec-num-mul v n)
  (make-vec (* (<vec> i v) n) (* (<vec> j v) n) (* (<vec> k v) n)))

(define (vec-dot v1 v2)
  (+ (* (vec-i v1) (vec-i v2))
     (* (vec-j v1) (vec-j v2))
     (* (vec-k v1) (vec-k v2))))

(define (vec-cross v1 v2)
  (make-vec
   (- (* (vec-j v1) (vec-k v2))
      (* (vec-k v1) (vec-j v2)))
   (- (* (vec-k v1) (vec-i v2))
      (* (vec-i v1) (vec-k v2)))
   (- (* (vec-i v1) (vec-j v2))
      (* (vec-j v1) (vec-i v2)))))

(define (vec-length v)
  (sqrt (vec-dot v v)))

(define (vec-normalize v)
  (let ([mag (vec-length v)])
    (if (= mag 0)
        (make-vec 0 0 0)
        (vec-num-mul v (/ 1 mag)))))

(define (vec-reverse v)
  (vec-num-mul v -1))

;; Matrix
(define (mat-vec-mul m v)
  (let ([r (matrix-mul m
             (make-matrix 3 1
               (lambda (m n a)
                 (a 1 1 (vec-i v))
                 (a 2 1 (vec-j v))
                 (a 3 1 (vec-k v)))))])
    (make-vec (r 1 1) (r 2 1) (r 3 1))))

(define (scale x y z)
  (make-matrix 3 3
    (lambda (m n set)
      (set 1 1 x)
      (set 2 2 y)
      (set 3 3 z))))

(define (rotate-x degrees)
  (let ([ang (degrees->radians degrees)])
    (let ([ca (cos ang)]
          [sa (sin ang)])
      (make-matrix 3 3
        (lambda (m n set)
          (set 1 1 1)
          (set 2 2 ca)
          (set 2 3 (- sa))
          (set 3 2 sa)
          (set 3 3 ca))))))

(define (rotate-y degrees)
  (let ([ang (degrees->radians degrees)])
    (let ([ca (cos ang)]
          [sa (sin ang)])
      (make-matrix 3 3
        (lambda (m n set)
          (set 1 1 ca)
          (set 1 3 sa)
          (set 2 2 1)
          (set 3 1 (- sa))
          (set 3 3 ca))))))

(define (rotate-z degrees)
  (let ([ang (degrees->radians degrees)])
    (let ([ca (cos ang)]
          [sa (sin ang)])
      (make-matrix 3 3
        (lambda (m n set)
          (set 1 1 ca)
          (set 1 2 (- sa))
          (set 2 1 sa)
          (set 2 2 ca)
          (set 3 3 1))))))

;; Thresholding

(define (step edge x)
  (if (< x edge)
      0
      1))

(define (smoothstep edge0 edge1 x)
  (cond
   [(< x edge0) 0]
   [(>= x edge1) 1]
   [else
    ;; smooth Hermite interpolation
    (let ([x (/ (- x edge0) (- edge1 edge0))])
      (+ (* -2 x x x) (* 3 x x)))]))

(define (pulse edge0 edge1 x)
  (- (step edge0 x) (step edge1 x)))

(define (pulsetrain edge period x)
  (pulse edge period (mod x period)))

(define (smoothpulse e0 e1 e2 e3 x)
  (- (smoothstep e0 e1 x) (smoothstep e2 e3 x)))

(define (smoothpulsetrain e0 e1 e2 e3 period x)
  (smoothpulse e0 e1 e2 e3 (mod x period)))

(define (fadeout g g-avg featuresize fwidth)
  (mix-num g g-avg (smoothstep .2 .6 (/ fwidth featuresize))))

;; Splines

(define (catmull-rom x p1 p2 p3 p4)
  (/ (+ (* (+ (- p1) (* 3 p2) (* -3 p3) p4) x x x)
        (* (+ (* 2 p1) (* -5 p2) (* 4 p3) (- p4)) x x)
        (* (+ (- p1) p3) x)
        (* 2 p2))
     2))

(define (spline-helper x ls)
  (if (<= x 1.0)
      (match ls [(,p1 ,p2 ,p3 ,p4 . ,_) (catmull-rom x p1 p2 p3 p4)])
      (spline-helper (- x 1) (cdr ls))))

(define (spline x ls)
  (let ([edges (- (length ls) 3)])
    (when (< edges 1)
      (errorf 'spline "not enough edges to spline points"))
    (spline-helper (* x edges) ls)))

(define (make-spline ls)
  (let ([edges (- (length ls) 3)])
    (when (< edges 1)
      (errorf 'spline "not enough edges to spline points"))
    (lambda (x)
      (when (or (< x 0.0) (> x 1.0))
        (errorf 'spline "param is out of range: ~a" x))
      (spline-helper (* x edges) ls))))

;; Filters

(define (box-filter x y xwidth ywidth)
  1)

(define (triangle-filter x y xwidth ywidth)
  (* (/ (- 1 (abs x)) (/ xwidth 2))
     (/ (- 1 (abs y)) (/ ywidth 2))))

(define (catmull-rom-filter x y xwidth ywidth)
  (let* ([r2 (+ (sqr x) (sqr y))]
         [r (sqrt r2)])
    (cond
     [(>= r 2) 0]
     [(< r 1) (+ (* 3 r r2) (* -5 r2) 2)]
     [else (+ (- (* r r2)) (* 5 r2) (* -8 r) 4)])))

(define (gaussian-filter x y xwidth ywidth)
  (exp (* -2 (+ (sqr (/ (* 2 x) xwidth)) (sqr (/ (* 2 y) ywidth))))))

(define (sinc-filter x y xwidth ywidth)
  (* (if (and (> x -0.001) (< x 0.001))
         1
         (/ (sin x) x))
     (if (and (> y -0.001) (< y 0.001))
         1
         (/ (sin y) y))))
