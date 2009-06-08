(case-sensitive #t)

(define targets (open-output-file ".src" 'replace))
(fprintf targets "src :=")

(define ($build filename exprs)
  (let* ([filename (string-append filename ".scene")]
         [op (open-output-file filename 'replace)])
    (fprintf op ";; Automatically Generated -- Do not edit\n\n")
    (for-each
     (lambda (expr)
       (pretty-print expr op)
       (fprintf op "\n"))
     exprs)
    (close-port op)
    (fprintf targets " ~a" filename)))

(define-syntax build
  (syntax-rules ()
    [(_ filename expr ...)
     ($build filename '(expr ...))]))

(build "ambient-light"
  (define-shader plain ([Ka 1])
    (color-color-mul
     (object-color object)
     (color-num-mul ((ambient)) Ka)))
  (render image-simple "ambient-light" 128 128 
    (<camera> make
      [translation (make-vec 0 0 10)]
      [target (make-vec 0 0 0)]
      [distance 1]
      [view (<view> make [left -2] [right 2] [bottom -2] [top 2])])
    (<scene> make
      [background-color (make-color 0 .3 .3)]
      [objects
       (list
        (sphere [center (make-vec 0 0 0)]
                [radius 1]
                [shader (plain)]
                [color (make-color 1 1 1)]))]
      [lights
       (list
        (ambient-light [color (make-color 1 1 1)]
                       [intensity 1]))])))

(build "distant-light"
  (define-shader matte ([Ka 1] [Kd 1])
    (let ([Nf (faceforward (vec-normalize normal) incoming)])
      (color-color-mul
       (object-color object)
       (color-color-plus
        (color-num-mul ((ambient)) Ka)
        (color-num-mul ((diffuse [N Nf])) Kd)))))
  (render image-simple "distant-light" 128 128
    (<camera> make
      [translation (make-vec 0 0 10)]
      [target (make-vec 0 0 0)]
      [distance 1]
      [view (<view> make [left -2] [right 2] [bottom -2] [top 2])])
    (<scene> make
      [background-color (make-color 0 .3 .3)]
      [objects
       (list
        (sphere [center (make-vec 0 0 0)]
                [radius 1]
                [shader (matte)]
                [color (make-color 1 1 1)]))]
      [lights
       (list
        (distant-light [position (make-vec -10 10 10)]
                     [color (make-color 1 1 1)]
                     [intensity 1]))])))

(build "point-light"
  (define-shader matte ([Ka 1] [Kd 1])
    (let ([Nf (faceforward (vec-normalize normal) incoming)])
      (color-color-mul
       (object-color object)
       (color-color-plus
        (color-num-mul ((ambient)) Ka)
        (color-num-mul ((diffuse [N Nf])) Kd)))))
  (render image-simple "point-light" 128 128
    (<camera> make
      [translation (make-vec 0 0 10)]
      [target (make-vec 0 0 0)]
      [distance 1]
      [view (<view> make [left -2] [right 2] [bottom -2] [top 2])])
    (<scene> make
      [background-color (make-color 0 .3 .3)]
      [objects
       (list
        (sphere [center (make-vec 0 0 0)]
                [radius 1]
                [shader (matte)]
                [color (make-color 1 1 1)]))]
      [lights
       (list
        (point-light [position (make-vec -10 10 10)]
                     [color (make-color 1 1 1)]
                     [intensity 100]))])))

(build "plane"
  (define-shader matte ([Ka 1] [Kd 1])
    (let ([Nf (faceforward (vec-normalize normal) incoming)])
      (color-color-mul
       (object-color object)
       (color-color-plus
        (color-num-mul ((ambient)) Ka)
        (color-num-mul ((diffuse [N Nf])) Kd)))))
  (render image-simple "plane" 128 128
    (<camera> make
      [translation (make-vec 0 0 10)]
      [target (make-vec 0 0 0)]
      [distance 1]
      [view (<view> make [left -2] [right 2] [bottom -2] [top 2])])
    (<scene> make
      [background-color (make-color 0 .3 .3)]
      [objects
       (list
        (plane [color (make-color 0 1 0)] [shader (matte)]))]
      [lights
       (list
        (distant-light [position (make-vec -10 10 10)]
          [color (make-color 1 1 1)]
          [intensity 1]))])))

(for-each
 (lambda (p)
   (let ([name (string-append "quadric-" (car p))]
         [coef (cdr p)])
     ($build name
       `((define-shader matte ([Ka 1] [Kd 1])
           (let ([Nf (faceforward (vec-normalize normal) incoming)])
             (color-color-mul
              (object-color object)
              (color-color-plus
               (color-num-mul ((ambient)) Ka)
               (color-num-mul ((diffuse [N Nf])) Kd)))))
         (render image-simple ,name 128 128
           (<camera> make
             [translation (make-vec 0 0 10)]
             [target (make-vec 0 0 0)]
             [distance 1]
             [view (<view> make [left -2] [right 2] [bottom -2] [top 2])])
           (<scene> make
             [background-color (make-color 0 .3 .3)]
             [objects
              (list
               (quadric [color (make-color 0 1 0)] [shader (matte)]
                 [coefficients ,coef]
                 [M (matrix-mul (rotate-x -90) (rotate-y 10))]
                 ))]
             [lights
              (list
               (distant-light [position (make-vec -1 1 10)]
                 [color (make-color 1 1 1)]
                 [intensity 1]))]))))))
 '(("sphere" . #(1 1 1 0 0 0 0 0 0 -1))
   ("cylinder" . #(1 1 0 0 0 0 0 0 0 -1))
   ("cone" . #(1 1 -1 0 0 0 0 0 0 0))
   ("hyperboloid" . #(1 1 -1 0 0 0 0 0 0 -1))))

(build "spheres"
  (define-shader shiny ([Ka 1] [Kd .1] [Ks 1] [roughness .2] [Kr .8])
    (let* ([Nf (faceforward (vec-normalize normal) incoming)]
           [IN (vec-normalize incoming)]
           [V (vec-reverse IN)]
           [R (reflect IN Nf)])
      (color-color-mul
       (object-color object)
       (color-color-plus
        (color-color-plus
         (color-color-plus
          (color-num-mul ((ambient)) Ka)
          (color-num-mul ((diffuse [N Nf])) Kd))
         (color-num-mul ((specular [N Nf] [eye V] [roughness roughness])) Ks))
        (sample-environment scene intersect-point R Kr depth)))))
  (render image-simple "spheres" 128 128
    (<camera> make
      [translation (make-vec 0 0 10)]
      [target (make-vec 0 0 0)]
      [distance 1]
      [view (<view> make [left -2] [right 2] [bottom -2] [top 2])])
    (<scene> make
      [background-color (make-color 0 0 0)]
      [objects
       (list
        (sphere [center (make-vec -1 .8 0)]
                [radius 1]
                [shader (shiny [Ka 0] [Kd 1] [Kr .5] [roughness 1])]
                [color (make-color 1 1 0)])
        (sphere [center (make-vec 1 .8 0)]
                [radius 1]
                [shader (shiny [Ka 0] [Kd 1] [Kr .5] [roughness 1])]
                [color (make-color 0 1 1)])
        (sphere [center (make-vec 0 -1 0)]
                [radius 1]
                [shader (shiny [Ka 0] [Kd 1] [Kr .5] [roughness 1])]
                [color (make-color 1 0 1)]))]
      [lights
       (list
        (point-light [position (make-vec -5 0 5)]
                     [color (make-color 1 1 1)]
                     [intensity 15])
        (point-light [position (make-vec 0 0 10)]
                     [color (make-color .6 .7 1)]
                     [intensity 10]))])))

(build "metal-ball"
  (define-shader shiny ([Ka 1] [Kd .1] [Ks 1] [roughness .2] [Kr .8])
    (let* ([Nf (faceforward (vec-normalize normal) incoming)]
           [IN (vec-normalize incoming)]
           [V (vec-reverse IN)]
           [R (reflect IN Nf)])
      (color-color-mul
       (object-color object)
       (color-color-plus
        (color-color-plus
         (color-color-plus
          (color-num-mul ((ambient)) Ka)
          (color-num-mul ((diffuse [N Nf])) Kd))
         (color-num-mul ((specular [N Nf] [eye V] [roughness roughness])) Ks))
        (sample-environment scene intersect-point R Kr depth)))))
  (render image-simple "metal-ball" 128 128
    (<camera> make (translation (make-vec 0 0 10))
      (target (make-vec 0 0 0)) (distance 1)
      (view
       (<view> make (left -2) (right 2) (bottom -2) (top 2))))
    (<scene>
     make
     (background-color (make-color 0 .3 .3))
     (objects
      (list (plane
             [center (make-vec 0 -1 0)]
             [M (matrix-mul (rotate-x -75) (scale 3 3 1))]
             (color (make-color 0 .6 0))
             (shader (shiny [Kr 0])))
        (sphere
         (color (make-color .5 .5 .5))
         (shader (shiny [Kr 1])))))
     (lights
      (list
       (distant-light
        (position (make-vec -10 10 10))
        (color (make-color 1 1 1))
        (intensity .9))
       (point-light
        (position (make-vec 1 1 1))
        (color (make-color 1 1 1))
        (intensity 1)))))))

(exit)
