(define-scheme-record object
  ((immutable color)
   (immutable shader)
   (immutable center)
   (immutable M)                        ; Transform matrix
   (immutable Mi)                       ; Inverse transform
   ))

(define-scheme-record sphere object ())
(define-scheme-record plane object ())
(define-scheme-record quadric object ((immutable coefficients)))
(define-scheme-record polyhedron object ((immutable planes)))

(define (object-intersections object ray)
  ((cond
    [(sphere? object) sphere-intersections]
    [(plane? object) plane-intersections]
    [(polyhedron? object) polyhedron-intersections]
    [(quadric? object) quadric-intersections]
    [else (error 'object-intersections "unknown object type: ~s" object)])
   object ray))

(define (object-normal object extra intersect-point)
  ((cond
    [(sphere? object) sphere-normal]
    [(plane? object) plane-normal]
    [(polyhedron? object) polyhedron-normal]
    [(quadric? object) quadric-normal]
    [else (error 'object-normal "unknown object type: ~s" object)])
   object extra intersect-point))

(define (point->surface object point)
  ;; Maps intersect point to surface shader coordinate space
  (cond
   [(plane? object) (plane-point->surface object point)]
   [else
    (mat-vec-mul (object-Mi object)
      (vec-vec-sub intersect-point (object-center object)))]))

(define (sphere-intersections object ray)
  (let ([origin (mat-vec-mul (object-Mi object)
                  (vec-vec-sub (<ray> origin ray) (object-center object)))]
        [direction (mat-vec-mul (object-Mi object) (<ray> direction ray))])
    (let* ([a (vec-dot direction direction)]
           [b (vec-dot direction origin)]
           [c (- (vec-dot origin origin) 1)]
           [dis (- (* b b) (* a c))])
      (if (>= dis 0)
          (let ([d (sqrt dis)])
            (list
             (<intersect> make [time (/ (- (- b) d) a)]
               [object object] [extra #f])
             (<intersect> make [time (/ (+ (- b) d) a)]
               [object object] [extra #f])))
          '()))))

(define (sphere-normal object extra intersect-point)
  (mat-vec-mul (object-M object)
    (vec-vec-sub intersect-point (object-center object))))

(define (plane-intersections object ray)
  (let ([origin (mat-vec-mul (object-Mi object)
                  (vec-vec-sub (<ray> origin ray) (object-center object)))]
        [direction (mat-vec-mul (object-Mi object) (<ray> direction ray))])
    (let ([t (- (/ (vec-k origin) (vec-k direction)))])
      (if (and 
           (<= (abs (+ (vec-i origin) (* t (vec-i direction)))) 1)
           (<= (abs (+ (vec-j origin) (* t (vec-j direction)))) 1))
          (list (<intersect> make [time t] [object object] [extra #f]))
          '()))))

(define (plane-normal object extra intersect-point)
  (mat-vec-mul (object-M object) (make-vec 0 0 1)))

(define (plane-point->surface object point)
  (vec-num-mul
   (vec-num-plus 
    (mat-vec-mul (object-Mi object)
      (vec-vec-sub point (object-center object)))
    1)
   0.5))

(define (polyhedron-intersections obj ray)
  (let ([origin (mat-vec-mul (object-Mi obj)
                  (vec-vec-sub (<ray> origin ray) (object-center obj)))]
        [direction (mat-vec-mul (object-Mi obj) (<ray> direction ray))])
    (let lp ([planes (polyhedron-planes obj)] [t-in -inf.0] [t-out +inf.0]
             [plane-in #f] [plane-out #f])
      (if (null? planes)
          (if (< t-in t-out)
              (list                     ; found intersections
               (<intersect> make [time t-in] [object obj] [extra plane-in])
               (<intersect> make [time t-out] [object obj] [extra plane-out]))
              '())
          (let ([plane (car planes)])
            (if (>= t-in t-out)
                '()                     ; No hit
                (let ([numer (- 1 (vec-dot plane origin))]
                      [denom (vec-dot plane direction)])
                  (cond
                   [(= denom 0)         ; ray is parallel
                    (if (< numer EPSILON)
                        '()             ; done
                        (lp (cdr planes) t-in t-out plane-in plane-out))]
                   [else                ; ray is not parallel
                    (let ([t-hit (/ numer denom)])
                      (cond
                       [(> denom 0)
                        (if (< t-out t-hit)
                            (lp (cdr planes) t-in t-out plane-in plane-out)
                            (lp (cdr planes) t-in t-hit plane-in plane))]
                       [else
                        (if (> t-in t-hit)
                            (lp (cdr planes) t-in t-out plane-in plane-out)
                            (lp (cdr planes) t-hit t-out plane plane-out))]))]))))))))

(define (polyhedron-normal obj extra intersect-point)
  (mat-vec-mul (object-M obj) extra))

(define (quadric-intersections object ray)
  (let ([origin (mat-vec-mul (object-Mi object)
                  (vec-vec-sub (<ray> origin ray) (object-center object)))]
        [direction (mat-vec-mul (object-Mi object) (<ray> direction ray))])
    (let ([Xo (vec-i origin)] [Yo (vec-j origin)] [Zo (vec-k origin)]
          [Xd (vec-i direction)] [Yd (vec-j direction)] [Zd (vec-k direction)])
      (match (quadric-coefficients object)
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
                       (list
                        (<intersect> make [time (/ (- (- b) d) a)]
                          [object object] [extra #f])
                        (<intersect> make [time (/ (+ (- b) d) a)]
                          [object object] [extra #f])))
                     '()))
               (if (= b 0)
                   '()
                   (list (<intersect> make [time (- 0.5 (/ c b))]
                           [object object] [extra #f])))))]))))

(define (quadric-normal object extra intersect-point)
  ;; This solution doesn't make sense. I don't trust it.
  (let ([ip (mat-vec-mul (object-M object)
              (vec-vec-sub intersect-point (object-center object)))])
    (let ([ipx (vec-i ip)] [ipy (vec-j ip)] [ipz (vec-k ip)])
      (match (quadric-coefficients object)
        [#(,A ,E ,H ,B ,C ,F ,D ,G ,I ,J)
         (let ([v (make-vec
                   (+ (* 2 A ipx) (* B ipy) (* C ipz) D)
                   (+ (* B ipx) (* 2 E ipy) (* F ipz) G)
                   (+ (* C ipx) (* F ipy) (* 2 H ipz) I))])
           (mat-vec-mul (object-Mi object) v))]))))
