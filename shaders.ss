(define-syntax (define-shader x)
  (syntax-case x ()
    [(_ name ([field default] ...) b1 b2 ...)
     (and (identifier? #'name)
          (let valid-fields? ([fields #'(field ...)] [seen '()])
            (syntax-case fields ()
              [(fn . rest)
               (and (identifier? #'fn)
                    (let ([f (syntax->datum #'fn)])
                      (when (memq f seen)
                        (syntax-error x (format "duplicate field ~a in" f)))
                      (valid-fields? #'rest (cons f seen))))]
              [() #t]
              [_ #f])))
     #'(begin
         (define-syntax (name x)
         (define (valid-bindings? bindings seen)
           (syntax-case bindings ()
             [((fn fv) . rest)
              (and (identifier? #'fn)
                   (let ([f (syntax->datum #'fn)])
                     (when (memq f seen)
                       (syntax-error x (format "duplicate field ~a in" f)))
                     (unless (memq f '(field ...))
                       (syntax-error x (format "unknown field ~a in" f)))
                     (valid-bindings? #'rest (cons f seen))))]
             [() #t]
             [_ #f]))
         (define (build-args fields defaults bindings)
           (if (snull? fields)
               '()
               (let* ([f (scar fields)]
                      [v (find-binding f bindings)])
                 (if v
                     (cons v (build-args (scdr fields) (scdr defaults)
                               (remove-binding f bindings)))
                     (cons (scar defaults)
                           (build-args (scdr fields) (scdr defaults)
                             bindings))))))
         
         (define (find-binding f bindings)
           (syntax-case bindings ()
             [((fn fv) . rest)
              (if (identifier-symbol-eq? #'fn f)
                  #'fv
                  (find-binding f #'rest))]
             [() #f]))
         (define (remove-binding f bindings)
           (syntax-case bindings ()
             [((fn fv) . rest)
              (if (identifier-symbol-eq? #'fn f)
                  #'rest
                  #`((fn fv) #,@(remove-binding f #'rest)))]))
         (define (snull? x) (syntax-case x () [() #t] [_ #f]))
         (define (scar x) (syntax-case x () [(x . _) #'x]))
         (define (scdr x) (syntax-case x () [(_ . y) #'y]))
         (syntax-case x ()
           [(name . bindings)
            (valid-bindings? #'bindings '())
            #`(shader-proc #,@(build-args #'(field ...) #'(default ...) #'bindings))]))
         (define (shader-proc field ...)
           (lambda ()
             b1 b2 ...)))]))

(define (faceforward V R)
  (if (< (vec-dot V R) (/ pi 4))
      V
      (vec-reverse V)))

(define (reflect I N)
  (vec-vec-sub I (vec-num-mul N (* 2 (vec-dot I N)))))

(define (sample-environment scene P R Kr depth)
  (pixel-color-from-ray scene
    (<ray> make [origin P] [direction R])
    Kr
    (- depth 1)))

(define-syntax fold-lights
  (syntax-rules ()
    [(_ [light scene] [acc init] b1 b2 ...)
     (and (identifier? #'item) (identifier? #'acc))
     (fold-left (lambda (acc light) b1 b2 ...)
       init
       (<scene> lights scene))]))

(define-shader ambient ()
  (fold-lights [light scene] [color (make-color 0 0 0)]
    (color-color-plus color (light-shade light))))

(define-shader diffuse ([N #f])
  ;; Surface shaders, L is from the surface to the light
  (fold-lights [light scene] [color (make-color 0 0 0)]
    (let  ([L (vec-vec-sub (light-position light) intersect-point)])
      (color-color-plus color
        (color-num-mul (light-shade light) (vec-dot (vec-normalize L) N))))))

(define-shader specular ([N #f] [eye #f] [roughness #f])
  ;; Surface shaders, L is from the surface to the light
  (fold-lights [light scene] [color (make-color 0 0 0)]
    (let  ([L (vec-vec-sub (light-position light) intersect-point)])
      (let ([H (vec-normalize (vec-vec-plus L eye))])
        (color-color-plus color
          (color-num-mul (light-shade light)
            (expt (max 0 (vec-dot N H)) (/ 1 roughness))))))))
