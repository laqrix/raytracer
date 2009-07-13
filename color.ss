(define-record <color> r g b)

(define (make-color r g b) (<color> make [r r] [g g] [b b]))

(define black (make-color 0 0 0))
(define white (make-color 1 1 1))

(define transparent black)
(define opaque white)

(define (color=? c1 c2)
  (or (eq? c1 c2)
      (and (= (<color> r c1) (<color> r c2))
           (= (<color> g c1) (<color> g c2))
           (= (<color> b c1) (<color> b c2)))))

(define color-add
  (case-lambda
   [(c1) c1]
   [(c1 c2)
    (make-color
     (+ (<color> r c1) (<color> r c2))
     (+ (<color> g c1) (<color> g c2))
     (+ (<color> b c1) (<color> b c2)))]
   [(c1 c2 . rest) (apply color-add (color-add c1 c2) rest)]))

(define color-mul
  (case-lambda
   [(c1) c1]
   [(c1 c2)
    (make-color
     (* (<color> r c1) (<color> r c2))
     (* (<color> g c1) (<color> g c2))
     (* (<color> b c1) (<color> b c2)))]
   [(c1 c2 . rest) (apply color-mul (color-mul c1 c2) rest)]))

(define (color-num-add c n)
  (make-color
   (+ (<color> r c) n)
   (+ (<color> g c) n)
   (+ (<color> b c) n)))

(define (color-num-mul c n)
  (make-color
   (* (<color> r c) n)
   (* (<color> g c) n)
   (* (<color> b c) n)))

(define (color-mix c1 c2 x)
  (color-add
   (color-num-mul c1 (- 1 x))
   (color-num-mul c2 x)))

(define (color-catmull-rom x p1 p2 p3 p4)
  (make-color
   (catmull-rom x (<color> r p1) (<color> r p2) (<color> r p3) (<color> r p4))
   (catmull-rom x (<color> g p1) (<color> g p2) (<color> g p3) (<color> g p4))
   (catmull-rom x (<color> b p1) (<color> b p2) (<color> b p3) (<color> b p4))))

(define (color-spline-helper x ls)
  (if (<= x 1.0)
      (match ls [(,p1 ,p2 ,p3 ,p4 . ,_) (color-catmull-rom x p1 p2 p3 p4)])
      (color-spline-helper (- x 1) (cdr ls))))

(define (color-spline x ls)
  (let ([edges (- (length ls) 3)])
    (when (< edges 1)
      (error 'color-spline "not enough edges to spline colors"))
    (color-spline-helper (* x edges) ls)))
