(define-record <quadric> color shader center M Mi coefficients)

(define-defaults quadric ([color (make-color 0 0 0)] [shader #f]
                          [center (make-vec 0 0 0)] [M (matrix-identity 3)]
                          [coefficients #f])
  (unless (vector? coefficients)
    (error 'quadric "coefficients are not a vector: ~s" coefficients))
  (unless (= (vector-length coefficients) 10)
    (error 'quadric "incorrect number of coefficients: ~s" coefficients))
  (<quadric> make [color color] [shader shader] [center center]
    [M M] [Mi (matrix-inverse M)] [coefficients coefficients]))

(define (quadric-intersections ray center Mi coefficients)
  (let ([origin (mat-vec-mul Mi (vec-vec-sub (<ray> origin ray) center))]
        [direction (mat-vec-mul Mi (<ray> direction ray))])
    (let ([Xo (vec-i origin)] [Yo (vec-j origin)] [Zo (vec-k origin)]
          [Xd (vec-i direction)] [Yd (vec-j direction)] [Zd (vec-k direction)])
      (match coefficients
        [#(,A ,E ,H ,B ,C ,F ,D ,G ,I ,J)
         (let ([a (+ (* Xd (+ (* A Xd) (* B Yd) (* C Zd)))
                     (* Yd (+ (* E Yd) (* F Zd)))
                     (* H Zd Zd))]
               [b (+ (* Xd (+ (* A Xo) (* 0.5 (+ (* B Yo) (* C Zo) D))))
                     (* Yd (+ (* E Yo) (* 0.5 (+ (* B Xo) (* F Zo) G))))
                     (* Zd (+ (* H Zo) (* 0.5 (+ (* C Xo) (* F Yo) I)))))]
               [c (+ (* Xo (+ (* A Xo) (* B Yo) (* C Zo) D))
                     (* Yo (+ (* E Yo) (* F Zo) G))
                     (* Zo (+ (* H Zo) I))
                     J)])
           (if (not (= a 0))            ; quadratic
               (let ([dis (- (* b b) (* a c))])
                 (if (>= dis 0)
                     (let ([d (sqrt dis)])
                       (list (/ (- (- b) d) a) (/ (+ (- b) d) a)))
                     '()))
               (if (= b 0)
                   '()
                   (list (- 0.5 (/ c b))))))]))))

(define (quadric-normal center M coefficients intersect-point)
  ;; This solution doesn't make sense. I don't trust it.
  (let ([ip (mat-vec-mul M (vec-vec-sub intersect-point center))])
    (let ([ipx (vec-i ip)] [ipy (vec-j ip)] [ipz (vec-k ip)])
      (match coefficients
        [#(,A ,E ,H ,B ,C ,F ,D ,G ,I ,J)
         (let ([v (make-vec
                   (+ (* 2 A ipx) (* B ipy) (* C ipz) D)
                   (+ (* B ipx) (* 2 E ipy) (* F ipz) G)
                   (+ (* C ipx) (* F ipy) (* 2 H ipz) I))])
           (mat-vec-mul (matrix-inverse M) v))]))))
