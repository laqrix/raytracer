(define-record <color> r g b)

(define (make-color r g b) (<color> make [r r] [g g] [b b]))

(define (color-r c) (<color> r c))
(define (color-g c) (<color> g c))
(define (color-b c) (<color> b c))

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

(define color-sub
  (case-lambda
   [(c1) c1]
   [(c1 c2)
    (make-color
     (- (<color> r c1) (<color> r c2))
     (- (<color> g c1) (<color> g c2))
     (- (<color> b c1) (<color> b c2)))]
   [(c1 c2 . rest) (apply color-sub (color-sub c1 c2) rest)]))

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
      (errorf 'color-spline "not enough edges to spline colors"))
    (color-spline-helper (* x edges) ls)))

;; Color space conversions

(define (hcx->rgb hp c x m)
  (define (rgb r1 g1 b1 m)
    (values (+ r1 m) (+ g1 m) (+ b1 m)))
  (cond
   [(<= 0 hp 1) (rgb c x 0 m)]
   [(<= 1 hp 2) (rgb x c 0 m)]
   [(<= 2 hp 3) (rgb 0 c x m)]
   [(<= 3 hp 4) (rgb 0 x c m)]
   [(<= 4 hp 5) (rgb x 0 c m)]
   [(<= 5 hp 6) (rgb c 0 x m)]))

(define (hsl->rgb h s l)
  ;; h = [0, 360], s,l = [0, 1]
  (define (rgb r1 g1 b1 m)
    (values (+ r1 m) (+ g1 m) (+ b1 m)))
  (let* ([hp (/ (mod h 360) 60)]
         [c (* (- 1 (abs (- (* 2 l) 1))) s)]
         [x (* c (- 1 (abs (- (mod hp 2) 1))))]
         [m (- l (/ c 2))])
    (hcx->rgb hp c x m)))

(define (hsv->rgb h s v)
  ;; h = [0, 360], s,v = [0, 1]
  (define (rgb r1 g1 b1 m)
    (values (+ r1 m) (+ g1 m) (+ b1 m)))
  (let* ([hp (/ (mod h 360) 60)]
         [c (* v s)]
         [x (* c (- 1 (abs (- (mod hp 2) 1))))]
         [m (- v c)])
    (hcx->rgb hp c x m)))

(define (hue&chroma r g b)
  ;; r,g,b = [0, 1]
  (let* ([m (min r g b)]
         [M (max r g b)]
         [c (- M m)]
         [hp (if (= c 0)
                 0
                 (cond
                  [(= M r) (mod (/ (- g b) c) 6)]
                  [(= M g) (+ (/ (- b r) c) 2)]
                  [(= M b) (+ (/ (- r g) c) 4)]
                  [else (errorf 'hue&chroma "undefined hue")]))]
         [h (* hp 60)])
    (values h c m M)))

(define (rgb->hsl r g b)
  (let-values ([(h c m M) (hue&chroma r g b)])
    (let* ([l (/ (+ M m) 2)]
           [s (if (= c 0)
                  0
                  (/ c (- 1 (abs (- (* 2 l) 1)))))])
      (values h s l))))

(define (rgb->hsv r g b)
  (let-values ([(h c m M) (hue&chroma r g b)])
    (let* ([v M]
           [s (if (= c 0)
                  0
                  (/ c v))])
      (values h s v))))

(define color
  (case-lambda
   [(x) (make-color x x x)]
   [(r g b) (make-color r g b)]
   [(space u v w)
    (match space
      ["rgb" (make-color u v w)]
      ["hsl" (let-values ([(r g b) (hsl->rgb (* u 360) v w)])
               (make-color r g b))]
      ["hsv" (let-values ([(r g b) (hsv->rgb (* u 360) v w)])
               (make-color r g b))])]))
