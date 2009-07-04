(define-scheme-record object
  ((immutable color)
   (immutable opacity)
   (immutable surface)                  ; Surface shader, cannot be #f
   (immutable volume)                   ; Volume shader | #f
   (immutable displacement)             ; Displacement shader | #f
   (immutable center)
   (immutable M)                        ; Transform matrix
   (immutable Mi)                       ; Inverse transform
   ))

(define-scheme-record sphere object ())
(define-scheme-record plane object ())
(define-scheme-record quadric object ((immutable coefficients)))
(define-scheme-record polyhedron object ((immutable planes)))

(define-scheme-record csg object ((immutable object1) (immutable object2)))
(define-scheme-record csg-union csg ())
(define-scheme-record csg-intersect csg ())
(define-scheme-record csg-difference csg ())

(define (object-intersections object ray)
  ((cond
    [(sphere? object) sphere-intersections]
    [(plane? object) plane-intersections]
    [(polyhedron? object) polyhedron-intersections]
    [(quadric? object) quadric-intersections]
    [(csg-union? object) csg-union-intersections]
    [(csg-intersect? object) csg-intersect-intersections]
    [(csg-difference? object) csg-difference-intersections]
    [else (error 'object-intersections "unknown object type: ~s" object)])
   object
   (<ray> make
     [origin (mat-vec-mul (object-Mi object)
               (vec-sub (<ray> origin ray) (object-center object)))]
     [direction (mat-vec-mul (object-Mi object) (<ray> direction ray))])))

(define (object-normal object extra intersect-point)
  ((cond
    [(sphere? object) sphere-normal]
    [(plane? object) plane-normal]
    [(polyhedron? object) polyhedron-normal]
    [(quadric? object) quadric-normal]
    [(csg? object) csg-normal]
    [else (error 'object-normal "unknown object type: ~s" object)])
   object extra intersect-point))

(define object)                         ; should really be in user env
(define normal)                         ; should really be in user env
(define incoming)                       ; should really be in user env
(define (object-displace obj ip n)
  (cond
   [(object-displacement obj) =>
    (lambda (shader)
      (fluid-let ([object obj]
                  [intersect-point ip]
                  [normal n])
        (call-with-values shader
          (case-lambda
           [(normal) (values intersect-point normal)]
           [(intersect-point normal) (values intersect-point normal)]))))]
   [else
    (values ip n)]))

(define scene)                          ; should really be in user env
(define Cs)                             ; should really be in user env
(define Os)                             ; should really be in user env
(define intersect-point)                ; should really be in user env
(define depth)                          ; should really be in user env
(define (object-shade s obj extra ip norm i d)
  (cond
   [(object-surface obj) =>
    (lambda (shader)
      (fluid-let ([scene s]
                  [object obj]
                  [Cs (object-color obj)]
                  [Os (object-opacity obj)]
                  [intersect-point ip]
                  [normal norm]
                  [incoming i]
                  [depth d])
        (call-with-values shader
          (case-lambda
           [(color) (values color Os)]
           [(color opacity) (values color opacity)]))))]
   [(and (csg? obj) (<intersect> object extra))
    (object-shade s (<intersect> object extra) (<intersect> extra extra)
      ip norm i d)]
   [else
    (error 'object-shade "no object shader defined for ~a" obj)]))

(define Ci)                             ; should really be in user env
(define Oi)                             ; should really be in user env
(define (volume-shade obj ip i ci oi)
  (cond
   [(object-volume obj) =>
    (lambda (shader)
      (fluid-let ([object obj]
                  [intersect-point ip]
                  [incoming i]
                  [Ci ci]
                  [Oi oi])
        (call-with-values shader
          (case-lambda
           [(color) (values color (object-opacity obj))]
           [(color opacity) (values color opacity)]))))]
   [else #f]))

(define (point->surface object point)
  ;; Maps intersect point to surface shader coordinate space
  (cond
   [(plane? object) (plane-point->surface object point)]
   [else
    (mat-vec-mul (object-Mi object)
      (vec-sub intersect-point (object-center object)))]))

(define (point->texture object point)
  ;; Maps point from surface shader coordinates to texture coordinates
  (cond
   [(sphere? object) (sphere-point->texture object point)]
   [(plane? object) (plane-point->texture object point)]
   [else (error 'point->texture "unknown object type: ~s" object)]))

(define (sphere-intersections object ray)
  (let ([origin (<ray> origin ray)]
        [direction (<ray> direction ray)])
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
    (vec-sub intersect-point (object-center object))))

(define (sphere-point->texture object point)
  (let* ([pnt (vec-normalize point)]
         [x (vec-i pnt)]
         [y (vec-j pnt)]
         [z (vec-k pnt)]
         [phi (+ 0.5 (/ (asin y) pi))]  ; [0-1]
         [xz-len (sqrt (+ (sqr x) (sqr z)))]
         [theta (if (= 0 xz-len)        ; [0-1]
                    0
                    (/ (if (= 0 z)
                           (if (> x 0)
                               0
                               pi)
                           (let ([q (acos (/ x xz-len))])
                             (if (< z 0)
                                 (- (* 2 pi) q)
                                 q)))
                       (* 2 pi)))])
    (make-vec theta phi 0)))

(define (plane-intersections object ray)
  (let ([origin (<ray> origin ray)]
        [direction (<ray> direction ray)])
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
   (vec-num-add
    (mat-vec-mul (object-Mi object)
      (vec-sub point (object-center object)))
    1)
   0.5))

(define (plane-point->texture object point)
  (make-vec (fmod (vec-i point) 1) (fmod (vec-j point) 1) 0))

(define (polyhedron-intersections obj ray)
  (let ([origin (<ray> origin ray)]
        [direction (<ray> direction ray)])
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
  (let ([origin (<ray> origin ray)]
        [direction (<ray> direction ray)])
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
              (vec-sub intersect-point (object-center object)))])
    (let ([ipx (vec-i ip)] [ipy (vec-j ip)] [ipz (vec-k ip)])
      (match (quadric-coefficients object)
        [#(,A ,E ,H ,B ,C ,F ,D ,G ,I ,J)
         (let ([v (make-vec
                   (+ (* 2 A ipx) (* B ipy) (* C ipz) D)
                   (+ (* B ipx) (* 2 E ipy) (* F ipz) G)
                   (+ (* C ipx) (* F ipy) (* 2 H ipz) I))])
           (mat-vec-mul (object-Mi object) v))]))))

(define (csg-union-intersections object ray)
  (map
   (lambda (inter)
     (<intersect> copy inter [object object] [extra inter]))
   (let union ([a (object-intersections (csg-object1 object) ray)]
               [b (object-intersections (csg-object2 object) ray)])
     (cond
      [(null? a) b]
      [(null? b) a]
      [else
       (let ([a1 (car a)]
             [a2 (cadr a)]
             [b1 (car b)]
             [b2 (cadr b)])
         (let ([ca1 (<intersect> time a1)]
               [ca2 (<intersect> time a2)]
               [cb1 (<intersect> time b1)]
               [cb2 (<intersect> time b2)])
           (cond
            [(<= ca2 cb1)               ; A is completely less than B
             (append (list a1 a2) (union (cddr a) b))]
            [(<= cb2 ca1)               ; B is completely less than A
             (append (list b1 b2) (union a (cddr b)))]
            [(and (<= ca1 cb1) (>= ca2 cb2)) ; A is completely around B
             (union a (cddr b))]
            [(and (<= cb1 ca1) (>= cb2 ca2)) ; B is completely around A
             (union (cddr a) b)]
            [(and (< ca1 cb1) (> cb2 ca2)) ; A and B cross, A first
             (union (append (list a1 b2) (cddr a)) b)]
            [(and (< cb1 ca1) (> ca2 cb2)) ; A and B cross, B first
             (union a (append (list b1 a2) (cddr b)))]
            [else (error 'union "Unhandled condition A=~s, B=~s" a b)])))]))))

(define (csg-intersect-intersections object ray)
  (map
   (lambda (inter)
     (<intersect> copy inter [object object] [extra inter]))
   (let intersection ([a (object-intersections (csg-object1 object) ray)]
                      [b (object-intersections (csg-object2 object) ray)])
     (cond
      [(null? a) '()]
      [(null? b) '()]
      [else
       (let ([a1 (car a)]
             [a2 (cadr a)]
             [b1 (car b)]
             [b2 (cadr b)])
         (let ([ca1 (<intersect> time a1)]
               [ca2 (<intersect> time a2)]
               [cb1 (<intersect> time b1)]
               [cb2 (<intersect> time b2)])
           (cond
            [(and (= ca1 cb1) (= ca2 cb2)) ; A and B are the same segment
             (append (list a1 a2) (intersection (cddr a) (cddr b)))]
            [(<= ca2 cb1)               ; A is completely less than B
             (intersection (cddr a) b)]
            [(<= cb2 ca1)               ; B is completely less than A
             (intersection a (cddr b))]
            [(and (< ca1 cb1) (> ca2 cb2)) ; A is completely around B
             (intersection (append (list a1 b1 b1 b2 b2 a2) (cddr a)) b)]
            [(and (< cb1 ca1) (> cb2 ca2)) ; B is completely around A
             (intersection a (append (list b1 a1 a1 a2 a2 b2) (cddr b)))]
            [(and (< ca1 cb1) (> cb2 ca2)) ; A and B cross, A first
             (intersection (append (list a1 b1 b1 a2) (cddr a)) 
               (append (list b1 a2 a2 b2) (cddr b)))]
            [(and (< cb1 ca1) (> ca2 cb2)) ; A and B cross, B first
             (intersection (append (list a1 b2 b2 a2) (cddr a))
               (append (list b1 a1 a1 b2) (cddr b)))]
            [else (error 'intersection "Unhandled condition A=~s, B=~s" a b)])))]))))

(define (csg-difference-intersections object ray)
  (map
   (lambda (inter)
     (<intersect> copy inter [object object] [extra inter]))
   (let difference ([a (object-intersections (csg-object1 object) ray)]
                    [b (object-intersections (csg-object2 object) ray)])
     (cond
      [(null? a) '()]
      [(null? b) a]
      [else
       (let ([a1 (car a)]
             [a2 (cadr a)]
             [b1 (car b)]
             [b2 (cadr b)])
         (let ([ca1 (<intersect> time a1)]
               [ca2 (<intersect> time a2)]
               [cb1 (<intersect> time b1)]
               [cb2 (<intersect> time b2)])
           (cond
            [(<= ca2 cb1)               ; A is completely less than B
             (append (list a1 a2) (difference (cddr a) b))] 
            [(<= cb2 ca1)               ; B is completely less than A
             (difference a (cddr b))]
            [(and (<= ca1 cb1) (> ca2 cb2)) ; A is completely around B
             (difference (append (list a1 b1 b2 a2) (cddr a)) b)]
            [(and (<= cb1 ca1) (> cb2 ca2)) ; B is completely around A
             (difference (cddr a) b)]
            [(and (< ca1 cb1) (>= cb2 ca2)) ; A and B cross, A first
             (difference (append (list a1 b1) (cddr a)) b)]
            [(and (< cb1 ca1) (>= ca2 cb2)) ; A and B cross, B first
             (difference (append (list b2 a2) (cddr a)) b)]
            [(and (= ca1 cb1) (= ca2 cb2)) ; A and B are exactly the same
             (difference (cddr a) (cddr b))]
            [else (error 'difference "Unhandled condition ca1=~s, ca2=~s, cb1=~s, cb2=~s" ca1 ca2 cb1 cb2)])))]))))

(define (csg-normal object extra intersect-point)
  ;;(mat-vec-mul (object-M object)
    (object-normal (<intersect> object extra)
      (<intersect> extra extra)
      intersect-point))
  ;;)
