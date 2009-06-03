(include "matrix.ss")

(define pi (acos -1))

(define (radians->degrees x)
  (/ (* x 180) pi))

(define (degrees->radians x)
  (/ (* x pi) 180))

(define (sqr x) (* x x))

(define (make-linear-transform in-min in-max out-min out-max)
  (let ([delta (/ (- out-max out-min) (- in-max in-min))])
    (lambda (x)
      (+ (* (- x in-min) delta) out-min))))

;; Triple
(define-record <triple> x y z)

(define (triple-scalar-plus t n)
  (<triple> make
    [x (+ (<triple> x t) n)]
    [y (+ (<triple> y t) n)]
    [z (+ (<triple> z t) n)]))

(define (triple-scalar-mul t n)
  (<triple> make
    [x (* (<triple> x t) n)]
    [y (* (<triple> y t) n)]
    [z (* (<triple> z t) n)]))

(define (triple-triple-plus t1 t2)
  (<triple> make
    [x (+ (<triple> x t1) (<triple> x t2))]
    [y (+ (<triple> y t1) (<triple> y t2))]
    [z (+ (<triple> z t1) (<triple> z t2))]))

(define (triple-triple-sub t1 t2)
  (<triple> make
    [x (- (<triple> x t1) (<triple> x t2))]
    [y (- (<triple> y t1) (<triple> y t2))]
    [z (- (<triple> z t1) (<triple> z t2))]))

;; Vector
(define (vec? t) (<triple> is? t))
(define (vec-i t) (<triple> x t))
(define (vec-j t) (<triple> y t))
(define (vec-k t) (<triple> z t))
(define (make-vec i j k) (<triple> make [x i] [y j] [z k]))
(define vec-num-plus triple-scalar-plus)
(define vec-num-mul triple-scalar-mul)
(define vec-vec-plus triple-triple-plus)
(define vec-vec-sub triple-triple-sub)

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
(define (build-identity-matrix) (matrix-identity 3))

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
