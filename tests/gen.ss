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
  (for-each
   (lambda (p)
     (let ([name (string-append "light-" (car p))]
           [expr (cdr p)])
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
                 (sphere [center (make-vec 0 0 0)]
                   [radius 1]
                   [surface (matte)]
                   [color white])
                 (plane
                  [center (make-vec 0 -1 0)]
                  [M (matrix-mul (rotate-x -90))]
                  (color (make-color 0 .6 0))
                  (surface (shiny-metal [Kr 0]))))]
               [lights (list ,expr)]))))))
   `(("ambient" . (ambient-light))
     ("distant" . (distant-light [position (make-vec -10 10 10)]))
     ("point" . (point-light [position (make-vec -10 10 10)] [intensity 100]))
     ("spot" . (spot-light [intensity 15]
                 [position (make-vec -3 3 3)]
                 [target (make-vec 0 0 0)]))))

  (do ([i 0 (+ i 1)]) ((= i 8))
    (let ([name (format "light-spots-~a" i)])
      (define (make-lights x)
        (cond
         [(fxlogbit? 0 x)
          (cons `(spot-light
                  [intensity 25]
                  [position (make-vec 5 -1 5)]
                  [target (make-vec 0 0 0)]
                  [coneangle 15]
                  [color (make-color 1 .3 .3)])
            (make-lights (fxlogbit0 0 x)))]
         [(fxlogbit? 1 x)
          (cons `(spot-light
                  [intensity 25]
                  [position (make-vec -5 -1 5)]
                  [target (make-vec 0 0 0)]
                  [coneangle 15]
                  [color (make-color .3 1 .3)])
            (make-lights (fxlogbit0 1 x)))]
         [(fxlogbit? 2 x)
          (cons `(spot-light
                  [intensity 25]
                  [position (make-vec 0 5 5)]
                  [target (make-vec 0 0 0)]
                  [coneangle 15]
                  [color (make-color .3 .3 1)])
            (make-lights (fxlogbit0 2 x)))]
         [else '()]))
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
                (sphere [surface (matte)])
                (plane
                 (surface (checker [blackcolor (make-color .5 .5 .5)]))
                 [center (make-vec 0 -1.2 0)]
                 [M (matrix-mul (scale 5 5 5) (rotate-x -90))])
                (plane
                 (surface (checker [blackcolor (make-color .5 .5 .5)]))
                 [center (make-vec 0 0 -1.2)]
                 [M (scale 5 5 5)])
                )]
              [lights
               (list
                (ambient-light [intensity .3])
                ,@(make-lights i))]))))))
  )

(group objects
  (for-each
   (lambda (p)
     (let ([name (string-append "object-" (car p))]
           [expr (cdr p)])
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
                (list ,expr)]
               [lights
                (list
                 (distant-light [position (make-vec -10 10 10)]
                   [color white]
                   [intensity 1])
                 (distant-light [position (make-vec 10 -10 10)]
                   [color white]
                   [intensity 1/4]))]))))))
   `(("sphere" . (sphere [color (make-color 0 1 0)] [surface (matte)]))
     ("plane" . (plane [color (make-color 0 1 0)] [surface (matte)]))
     ("cube" . (cube [color (make-color 0 1 0)] [surface (matte)]
            [M (matrix-mul (rotate-x 15) (rotate-y 80))]))
     ("tetrahedron" . (tetrahedron [color (make-color 0 1 0)] [surface (matte)]
            [M (matrix-mul (rotate-x -45) (rotate-z 45))]))
     ("octahedron" . (octahedron [color (make-color 0 1 0)] [surface (matte)]))
     ("icosahedron" . (icosahedron [color (make-color 0 1 0)] [surface (matte)]))
     ))


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
                 (quadric [color (make-color 0 1 0)] [surface (matte)]
                   [coefficients ,coef]
                   [M (matrix-mul (rotate-x -90) (rotate-y 10))]
                   ))]
               [lights
                (list
                 (distant-light [position (make-vec -1 1 10)]
                   [color white]
                   [intensity 1]))]))))))
   '(("sphere" . '#(1 1 1 0 0 0 0 0 0 -1))
     ("cylinder" . '#(1 1 0 0 0 0 0 0 0 -1))
     ("cone" . '#(1 1 -1 0 0 0 0 0 0 0))
     ("hyperboloid" . '#(1 1 -1 0 0 0 0 0 0 -1))))
  )

(build "spheres"
  (render image-simple "spheres" 128 128
    (<camera> make
      [translation (make-vec 0 0 10)]
      [target (make-vec 0 0 0)]
      [distance 1]
      [view (<view> make [left -2] [right 2] [bottom -2] [top 2])])
    (<scene> make
      [background-color black]
      [objects
       (list
        (sphere [center (make-vec -1 .8 0)]
                [radius 1]
                [surface (shiny-metal [Ka 0] [Kd 1] [Kr .5] [roughness 1])]
                [color (make-color 1 1 0)])
        (sphere [center (make-vec 1 .8 0)]
                [radius 1]
                [surface (shiny-metal [Ka 0] [Kd 1] [Kr .5] [roughness 1])]
                [color (make-color 0 1 1)])
        (sphere [center (make-vec 0 -1 0)]
                [radius 1]
                [surface (shiny-metal [Ka 0] [Kd 1] [Kr .5] [roughness 1])]
                [color (make-color 1 0 1)]))]
      [lights
       (list
        (point-light [position (make-vec -5 0 5)]
                     [color white]
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
             [M (matrix-mul (rotate-x -90))]
             (color (make-color 0 .6 0))
             (surface (shiny-metal [Kr 0])))
        (sphere
         (color (make-color .5 .5 .5))
         (surface (shiny-metal [Kr 1])))))
     (lights
      (list
       (distant-light
        (position (make-vec -10 10 10))
        (color white)
        (intensity .9))
       (point-light
        (position (make-vec 1 1 1))
        (color white)
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
                  [surface ,expr]
                  [M (scale 2 2 2)])
                (plane [center (make-vec 0 -3 0)]
                  [M (matrix-mul (scale 9 9 9) (rotate-x -90))]
                  [surface (matte)])
                #;(plane [center (make-vec -3 0 0)]
                [M (matrix-mul (scale 9 9 9) (rotate-y 90))]
                [surface (matte)])
                #;(plane [center (make-vec 0 0 -3)]
                [M (matrix-mul (scale 9 9 9))]
                [surface (matte)]))]
              [lights
               (list
                (ambient-light [intensity 0.1])
                (distant-light [position (make-vec 5 5 10)]))]))))))
   `(("constant" . (constant))
     ("matte" . (matte))
     ("metal" . (metal))
     ("shiny-metal" . (shiny-metal))
     ("plastic" . (plastic))
     ("stripes" . (stripes))
     ("checker" . (checker))
     ("mirror" . (mirror))
     ("marble" . (marble))
     ("granite" . (granite))
     ("wood" . (wood))
     ("glow" . (glow))
     ("screen" . (screen))))
  )

(group transparent
  (for-each
   (lambda (p)
     (let ([name (string-append "transparent-" (car p))]
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
                  [opacity (make-color .8 .8 .8)]
                  [surface ,expr]
                  [M (scale 2 2 2)])
                (plane [center (make-vec 0 -3 0)]
                  [M (matrix-mul (scale 9 9 9) (rotate-x -90))]
                  [surface (matte)])
                #;(plane [center (make-vec -3 0 0)]
                [M (matrix-mul (scale 9 9 9) (rotate-y 90))]
                [surface (matte)])
                (plane
                 [color (make-color .8 .8 .8)]
                 [center (make-vec 0 0 -3)]
                 [M (matrix-mul (scale 9 9 9))]
                 [surface (checker)]))]
              [lights
               (list
                (ambient-light [intensity 0.1])
                (distant-light [position (make-vec 5 5 10)]))]))))))
   `(("constant" . (constant))
     ("matte" . (matte))
     ("metal" . (metal))
     ("shiny-metal" . (shiny-metal))
     ("plastic" . (plastic))
     ("stripes" . (stripes))
     ("checker" . (checker))
     ("mirror" . (mirror))
     ("marble" . (marble))
     ("granite" . (granite))
     ("wood" . (wood))
     ("glow" . (glow))
     ("screen" . (screen))))
  )

(group textures
  (for-each
   (lambda (p)
     (let ([name (string-append "texture-" (car p))]
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
                ,expr
                (plane [center (make-vec 0 -3 0)]
                  [M (matrix-mul (scale 9 9 9) (rotate-x -90))]
                  [surface (matte)])
                #;(plane [center (make-vec -3 0 0)]
                [M (matrix-mul (scale 9 9 9) (rotate-y 90))]
                [surface (matte)])
                #;(plane [center (make-vec 0 0 -3)]
                [M (matrix-mul (scale 9 9 9))]
                [surface (matte)]))]
              [lights
               (list
                (ambient-light [intensity 0.1])
                (distant-light [position (make-vec 5 5 10)]))]))))))
   `(("plane" .
      (plane [M (scale 3 3 3)]
        [surface (simple-texmap
                 [texture (texture [filename "test-texture"])])]))
     ("sphere" .
      (sphere [M (scale 2 2 2)]
        [surface (simple-texmap
                 [texture (texture [filename "test-texture"])])]))))
  )

(group csg
  (build "csg-difference"
    (render image-simple "csg-difference" 128 128
      (<camera> make (translation (make-vec 0 0 10))
        (target (make-vec 0 0 0)) (distance 1)
        (view
         (<view> make (left -2) (right 2) (bottom -2) (top 2))))
      (<scene>
       make
       (background-color (make-color 0 .3 .3))
       (objects
        (list
         (plane
          [center (make-vec 0 -1 0)]
          [M (matrix-mul (rotate-x -90))]
          (color (make-color 0 .6 0))
          (surface (matte)))
         (difference
          [M (matrix-mul (rotate-x 15) (rotate-y 80))]
          [A (cube
              [color (make-color 1 0 0)]
              [surface (matte)]
              [M (scale .75 .75 .75)])]
          [B (sphere
              (color (make-color 0 0 1))
              (surface (matte)))])))
       (lights
        (list
         (distant-light
          (position (make-vec -10 10 10))
          (color white)
          (intensity .9))
         (point-light
          (position (make-vec 1 1 1))
          (color white)
          (intensity 1)))))))

  (build "csg-difference2"
    (render image-simple "csg-difference2" 128 128
      (<camera> make (translation (make-vec 0 0 10))
        (target (make-vec 0 0 0)) (distance 1)
        (view
         (<view> make (left -2) (right 2) (bottom -2) (top 2))))
      (<scene>
       make
       (background-color (make-color 0 .3 .3))
       (objects
        (list
         (plane
          [center (make-vec 0 -1 0)]
          [M (matrix-mul (rotate-x -90))]
          (color (make-color 0 .6 0))
          (surface (matte)))
         (difference
          [surface (marble)]
          [M (matrix-mul (rotate-x 15) (rotate-y 80))]
          [A (cube
              [color (make-color 1 0 0)]
              [surface (matte)]
              [M (scale .75 .75 .75)])]
          [B (sphere
              (color (make-color 0 0 1))
              (surface (matte)))])))
       (lights
        (list
         (distant-light
          (position (make-vec -10 10 10))
          (color white)
          (intensity .9))
         (point-light
          (position (make-vec 1 1 1))
          (color white)
          (intensity 1)))))))

  (build "csg-intersect"
    (render image-simple "csg-intersect" 128 128
      (<camera> make (translation (make-vec 0 0 10))
        (target (make-vec 0 0 0)) (distance 1)
        (view
         (<view> make (left -2) (right 2) (bottom -2) (top 2))))
      (<scene>
       make
       (background-color (make-color 0 .3 .3))
       (objects
        (list
         (plane
          [center (make-vec 0 -1 0)]
          [M (matrix-mul (rotate-x -90))]
          (color (make-color 0 .6 0))
          (surface (matte)))
         (intersect
          [M (matrix-mul (rotate-x 15) (rotate-y 80))]
          [A (cube
              [color (make-color 1 0 0)]
              [surface (matte)]
              [M (scale .75 .75 .75)])]
          [B (sphere
              (color (make-color 0 0 1))
              (surface (matte)))])))
       (lights
        (list
         (distant-light
          (position (make-vec -10 10 10))
          (color white)
          (intensity .9))
         (point-light
          (position (make-vec 1 1 1))
          (color white)
          (intensity 1)))))))

  (build "csg-intersect2"
    (render image-simple "csg-intersect2" 128 128
      (<camera> make (translation (make-vec 0 0 10))
        (target (make-vec 0 0 0)) (distance 1)
        (view
         (<view> make (left -2) (right 2) (bottom -2) (top 2))))
      (<scene>
       make
       (background-color (make-color 0 .3 .3))
       (objects
        (list
         (plane
          [center (make-vec 0 -1 0)]
          [M (matrix-mul (rotate-x -90))]
          (color (make-color 0 .6 0))
          (surface (matte)))
         (intersect
          [surface (marble)]
          [M (matrix-mul (rotate-x 15) (rotate-y 80))]
          [A (cube
              [color (make-color 1 0 0)]
              [surface (matte)]
              [M (scale .75 .75 .75)])]
          [B (sphere
              (color (make-color 0 0 1))
              (surface (matte)))])))
       (lights
        (list
         (distant-light
          (position (make-vec -10 10 10))
          (color white)
          (intensity .9))
         (point-light
          (position (make-vec 1 1 1))
          (color white)
          (intensity 1)))))))

  (build "csg-union"
    (render image-simple "csg-union" 128 128
      (<camera> make (translation (make-vec 0 0 10))
        (target (make-vec 0 0 0)) (distance 1)
        (view
         (<view> make (left -2) (right 2) (bottom -2) (top 2))))
      (<scene>
       make
       (background-color (make-color 0 .3 .3))
       (objects
        (list
         (plane
          [center (make-vec 0 -1 0)]
          [M (matrix-mul (rotate-x -90))]
          (color (make-color 0 .6 0))
          (surface (matte)))
         (union
          [M (matrix-mul (rotate-x 15) (rotate-y 80))]
          [A (cube
              [color (make-color 1 0 0)]
              [surface (matte)]
              [M (scale .75 .75 .75)])]
          [B (sphere
              (color (make-color 0 0 1))
              (surface (matte)))])))
        (lights
         (list
          (distant-light
           (position (make-vec -10 10 10))
           (color white)
           (intensity .9))
          (point-light
           (position (make-vec 1 1 1))
           (color white)
           (intensity 1)))))))

  (build "csg-union2"
    (render image-simple "csg-union2" 128 128
      (<camera> make (translation (make-vec 0 0 10))
        (target (make-vec 0 0 0)) (distance 1)
        (view
         (<view> make (left -2) (right 2) (bottom -2) (top 2))))
      (<scene>
       make
       (background-color (make-color 0 .3 .3))
       (objects
        (list
         (plane
          [center (make-vec 0 -1 0)]
          [M (matrix-mul (rotate-x -90))]
          (color (make-color 0 .6 0))
          (surface (matte)))
         (union
          [surface (marble)]
          [M (matrix-mul (rotate-x 15) (rotate-y 80))]
          [A (cube
              [color (make-color 1 0 0)]
              [surface (matte)]
              [M (scale .75 .75 .75)])]
          [B (sphere
              (color (make-color 0 0 1))
              (surface (matte)))])))
       (lights
        (list
         (distant-light
          (position (make-vec -10 10 10))
          (color white)
          (intensity .9))
         (point-light
          (position (make-vec 1 1 1))
          (color white)
          (intensity 1)))))))

  (build "csg-all"
    (render image-simple "csg-all" 256 256
      (<camera> make (translation (make-vec 0 0 10))
        (target (make-vec 0 0 0)) (distance 1)
        (view
         (<view> make (left -2) (right 2) (bottom -2) (top 2))))
      (<scene>
       make
       (background-color white)
       (objects
        (list
         (difference
          [M (matrix-mul (rotate-x 30) (rotate-y 50))]
          [A (intersect
              [A (cube
                  [color (make-color 1 0 0)]
                  [surface (plastic)]
                  [M (scale .75 .75 .75)])]
              [B (sphere
                  (color (make-color 0 0 1))
                  (surface (plastic)))])]
          [B (union
              [M (scale .5 .5 .5)]
              [surface (plastic)]
              [color (make-color 0 1 0)]
              [A (quadric [coefficients '#(1 1 0 0 0 0 0 0 0 -1)])]
              [B (union
                  [A (quadric
                      [M (rotate-y 90)]
                      [coefficients '#(1 1 0 0 0 0 0 0 0 -1)])]
                  [B (quadric
                      [M (rotate-x 90)]
                      [coefficients '#(1 1 0 0 0 0 0 0 0 -1)])])])])))
       (lights
        (list
         (ambient-light [intensity 0.3])
         (distant-light
          (position (make-vec -10 10 10))
          (color white)
          (intensity .9))
         (point-light
          (position (make-vec 1 1 1))
          (color white)
          (intensity 1)))))))
  )

(write-sources)
(exit)
