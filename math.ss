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

;; Polynomial Solving
;; c0 + c1 x + c2 x^2 + c3 x^3 + c4 x^4 = 0

(define (is-zero? x)
  ;; TODO: Might change to more global EPSILON, or may reduce to much
  ;; smaller number.
  (define EPSILON 1e-9)
  (< (- EPSILON) x EPSILON))

(define (cbrt x)
  (cond
   [(> x 0) (expt x 1/3)]
   [(< x 0) (- (expt (- x) 1/3))]
   [else 0]))

(define (solve-quadratic c0 c1 c2)
  ;; x^2 + p x + q = 0
  (let ([p (/ c1 (* 2 c2))]
        [q (/ c0 c2)])
    (let ([D (- (* p p) q)])
      (cond
       [(is-zero? D) (list (- p))]
       [(< D 0) '()]
       [else
        (let ([d (sqrt D)])
          (list (- d p) (- (- d) p)))]))))

(define (solve-cubic c0 c1 c2 c3)
  ;; x^3 + A x^2 + B x + C = 0
  (let ([A (/ c2 c3)]
        [B (/ c1 c3)]
        [C (/ c0 c3)])
    (let ([sub (/ A 3)])
      (map (lambda (r) (- r sub))
        (let* ([A2 (sqr A)]
               [p (/ (+ (* -1/3 A2) B) 3)]
               [q (/ (+ (* 2/27 A A2) (* -1/3 A B) C) 2)]
               [p3 (* p p p)]
               [D (+ (sqr q) p3)])
          (cond
           [(is-zero? D)
            (if (is-zero? q)
                (list 0)
                (let ([u (cbrt (- q))])
                  (list (* 2 u) (- u))))]
           [(< D 0)
            (let ([phi (* 1/3 (acos (/ (- q) (sqrt (- p3)))))]
                  [t (* 2 (sqrt (- p)))])
              (list
               (* t (cos phi))
               (* (- t) (cos (+ phi (/ pi 3))))
               (* (- t) (cos (- phi (/ pi 3))))))]
           [else
            (let* ([d (sqrt D)]
                   [u (cbrt (- d q))]
                   [v (- (cbrt (+ d q)))])
              (list (+ u v)))]))))))

(define (solve-quartic c0 c1 c2 c3 c4)
  ;; x^4 + A x^3 + B x^2 + C x + D = 0
  (call/cc
   (lambda (return)
     (let ([A (/ c3 c4)]
           [B (/ c2 c4)]
           [C (/ c1 c4)]
           [D (/ c0 c4)])
       (let ([sub (/ A 4)])
         (map (lambda (r) (- r sub))
           (let* ([A2 (sqr A)]
                  [p (+ (* -3/8 A2) B)]
                  [q (+ (* 1/8 A A2) (* -1/2 A B) C)]
                  [r (+ (* -3/256 A2 A2) (* 1/16 A2 B) (* -1/4 A C) D)])
             (cond
              [(is-zero? r)
               (cons 0 (solve-cubic q p 0 1))]
              [else
               (let* ([z (car (solve-cubic
                               (+ (* 1/2 r p) (* -1/8 q q))
                               (- r)
                               (* -1/2 p)
                               1))]
                      [u (- (sqr z) r)]
                      [v (- (* 2 z) p)]
                      [u (cond
                          [(is-zero? u) 0]
                          [(> u 0) (sqrt u)]
                          [else (return '())])]
                      [v (cond
                          [(is-zero? v) 0]
                          [(> v 0) (sqrt v)]
                          [else (return '())])])
                 (append
                  (solve-quadratic
                   (- z u)
                   (if (< q 0) (- v) v)
                   1)
                  (solve-quadratic
                   (+ z u)
                   (if (< q 0) v (- v))
                   1)))]))))))))
