(define-record <color> r g b)

(define (make-color r g b) (<color> make [r r] [g g] [b b]))

(define (color-color-plus c1 c2)
  (make-color
   (+ (<color> r c1) (<color> r c2))
   (+ (<color> g c1) (<color> g c2))
   (+ (<color> b c1) (<color> b c2))))

(define (color-color-mul c1 c2)
  (make-color
   (* (<color> r c1) (<color> r c2))
   (* (<color> g c1) (<color> g c2))
   (* (<color> b c1) (<color> b c2))))

(define (color-num-plus c n)
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
  (color-color-plus
   (color-num-mul c1 (- 1 x))
   (color-num-mul c2 x)))
