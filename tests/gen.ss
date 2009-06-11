(case-sensitive #t)

(define sources '())
(define groups (make-eq-hashtable))

(define current-group (make-parameter #f))

(define (write-sources)
  (let ([op (open-output-file ".src" 'replace)])
    (fprintf op "src :=")
    (for-each
     (lambda (s) (fprintf op " ~a" s))
     sources)
    (fprintf op "\n\n")
    (let-values ([(keys vals) (hashtable-entries groups)])
      (vector-for-each
       (lambda (group files)
         (fprintf op "~a:" group)
         (for-each
          (lambda (file) (fprintf op " ~a" file))
          files)
         (fprintf op "\n\n"))
       keys vals))))

(define ($build basename exprs)
  (let* ([filename (string-append basename ".scene")]
         [op (open-output-file filename 'replace)])
    (fprintf op ";; Automatically Generated -- Do not edit\n\n")
    (pretty-print `(load "../user-shaders.ss") op)
    (fprintf op "\n")
    (for-each
     (lambda (expr)
       (pretty-print expr op)
       (fprintf op "\n"))
     exprs)
    (close-port op)
    ;; capture filename and group
    (set! sources (cons filename sources))
    (let ([group (current-group)])
      (when group
        (hashtable-set! groups group
          (cons (string-append basename ".tga")
            (hashtable-ref groups group '())))))))

(define-syntax build
  (syntax-rules ()
    [(_ filename expr ...)
     ($build filename '(expr ...))]))

(define-syntax group
  (syntax-rules ()
    [(_ name expr ...)
     (parameterize ([current-group 'name])
       expr ...)]))

(group lights
  (build "light-ambient"
    (render image-simple "light-ambient" 128 128 
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
          (ambient-light [color (make-color 1 1 1)]
            [intensity 1]))])))
  
  (build "light-distant"
    (render image-simple "light-distant" 128 128
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

  (build "light-point"
    (render image-simple "light-point" 128 128
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
  )

(group objects
  (build "plane"
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

  (build "cube"
    (render image-simple "cube" 128 128
      (<camera> make
        [translation (make-vec 0 0 10)]
        [target (make-vec 0 0 0)]
        [distance 1]
        [view (<view> make [left -2] [right 2] [bottom -2] [top 2])])
      (<scene> make
        [background-color (make-color 0 .3 .3)]
        [objects
         (list
          (cube [color (make-color 0 1 0)] [shader (matte)]
            [M (matrix-mul (rotate-x 15) (rotate-y 80))]))]
        [lights
         (list
          (distant-light [position (make-vec -10 10 10)]
            [color (make-color 1 1 1)]
            [intensity 1]))])))

  (build "tetrahedron"
    (render image-simple "tetrahedron" 128 128
      (<camera> make
        [translation (make-vec 0 0 10)]
        [target (make-vec 0 0 0)]
        [distance 1]
        [view (<view> make [left -2] [right 2] [bottom -2] [top 2])])
      (<scene> make
        [background-color (make-color 0 .3 .3)]
        [objects
         (list
          (tetrahedron [color (make-color 0 1 0)] [shader (matte)]
            [M (matrix-mul (rotate-x -45) (rotate-z 45))]
            ))]
        [lights
         (list
          (distant-light [position (make-vec -10 10 10)]
            [color (make-color 1 1 1)]
            [intensity 1])
          (distant-light [position (make-vec 10 -10 10)]
            [color (make-color 1 1 1)]
            [intensity 1/2]))])))

  (for-each
   (lambda (p)
     (let ([name (string-append "quadric-" (car p))]
           [coef (cdr p)])
       ($build name
         `((render image-simple ,name 128 128
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
  )

(build "spheres"
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
                [shader (shiny-metal [Ka 0] [Kd 1] [Kr .5] [roughness 1])]
                [color (make-color 1 1 0)])
        (sphere [center (make-vec 1 .8 0)]
                [radius 1]
                [shader (shiny-metal [Ka 0] [Kd 1] [Kr .5] [roughness 1])]
                [color (make-color 0 1 1)])
        (sphere [center (make-vec 0 -1 0)]
                [radius 1]
                [shader (shiny-metal [Ka 0] [Kd 1] [Kr .5] [roughness 1])]
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
             (shader (shiny-metal [Kr 0])))
        (sphere
         (color (make-color .5 .5 .5))
         (shader (shiny-metal [Kr 1])))))
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

(group shaders
  (for-each
   (lambda (p)
     (let ([name (string-append "shader-" (car p))]
           [expr (cdr p)])
       ($build name
         `((render image-simple ,name 128 128
             (<camera> make (translation (make-vec 0 0 10))
               (target (make-vec 0 0 0))
               (distance 1)
               (view (<view> make (left -3.5) (right 3.5) (bottom -3.5) (top 3.5))))
             (<scene>
              make
              [background-color (make-color 0 .3 .3)]
              [objects
               (list
                (sphere [color (make-color 0 .8 0)]
                  [shader ,expr]
                  [M (scale 2 2 2)])
                (plane [center (make-vec 0 -3 0)]
                  [M (matrix-mul (scale 9 9 9) (rotate-x -90))]
                  [shader (matte)])
                #;(plane [center (make-vec -3 0 0)]
                [M (matrix-mul (scale 9 9 9) (rotate-y 90))]
                [shader (matte)])
                #;(plane [center (make-vec 0 0 -3)]
                [M (matrix-mul (scale 9 9 9))]
                [shader (matte)]))]
              [lights
               (list
                (ambient-light [intensity 0.1])
                (distant-light [position (make-vec 5 5 10)]))]))))))
   `(("matte" . (matte))
     ("metal" . (metal))
     ("shiny-metal" . (shiny-metal))
     ("plastic" . (plastic))
     ("stripes" . (stripes))
     ("checker" . (checker))))
  )


(write-sources)
(exit)
