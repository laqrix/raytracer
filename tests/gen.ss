(define width 128)
(define height (* width 10/16))

(define default-camera
  `(<camera> make
     [output-width ,width]
     [output-height ,height]
     [type 'perspective]
     [position (make-vec 0 0 10)]
     [target (make-vec 0 0 0)]
     [view
      (<view> make
        (left (* -2 16/10)) (right (* 2 16/10))
        (bottom -2) (top 2))]))

(define default-display
  '(<display> make
     [x-samples 1]
     [y-samples 1]
     [filter gaussian-filter]
     [x-width 2/3]
     [y-width 2/3]
     [gain 1]
     [gamma 1]))

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
     ($build filename `(expr ...))]))

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
         `((render image-simple ,name
             ,default-camera
             ,default-display
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
        `((render image-simple ,name
            ,default-camera
            ,default-display
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
         `((render image-simple ,name
             ,default-camera
             ,default-display
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
     ("torus" . (torus [color (make-color 0 1 0)] [surface (matte)]
                  [M (rotate-x 15)]))
     ))

  (for-each
   (lambda (p)
     (let ([name (string-append "quadric-" (car p))]
           [coef (cdr p)])
       ($build name
         `((render image-simple ,name
             ,default-camera
             ,default-display
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
                 (distant-light [position (make-vec -10 10 10)]
                   [color white]
                   [intensity 1])
                 (distant-light [position (make-vec 10 -10 10)]
                   [color white]
                   [intensity 1/4]))]))))))
   '(("sphere" . '#(1 1 1 0 0 0 0 0 0 -1))
     ("cylinder" . '#(1 1 0 0 0 0 0 0 0 -1))
     ("cone" . '#(1 1 -1 0 0 0 0 0 0 0))
     ("hyperboloid" . '#(1 1 -1 0 0 0 0 0 0 -1))))
  )

(build "spheres"
  (render image-simple "spheres"
    ,default-camera
    ,default-display
    (<scene> make
      [background-color black]
      [objects
       (map
        (lambda (i c)
          (define ang (+ (* i 2/3 pi) (* 1/6 pi)))
          (sphere
           [center (make-vec (cos ang) (sin ang) 0)]
           [radius 1]
           [surface (shiny-metal [Ka 0] [Kd 1] [Kr .5] [roughness 1])]
           [color c]))
        (list 0 1 2)
        (list (make-color 0 1 1) (make-color 1 1 0) (make-color 1 0 1)))]
      [lights
       (list
        (distant-light
         (position (make-vec -4 0 1))
         (color white)
         (intensity .5))
        (distant-light
         (position (make-vec 4 0 100))
         (color (make-color 0.6 0.7 1))
         (intensity .5)))])))

(build "metal-ball"
  (render image-simple "metal-ball"
    ,default-camera
    ,default-display
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
         `((render image-simple ,name
             ,default-camera
             ,default-display
             (<scene> make
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
   `(("checker" . (checker))
     ("constant" . (constant))
     ("glass" . (glass))
     ("glow" . (glow))
     ("granite" . (granite))
     ("marble" . (marble))
     ("marble-green-and-tan" .
      (marble
       [colorlist
        (list
         (make-color 0.824 0.725 0.584)
         (make-color 0.514 0.584 0.533)
         (make-color 0.514 0.584 0.533)
         (make-color 0.298 0.376 0.318)
         (make-color 0.298 0.376 0.318)
         (make-color 0.263 0.337 0.282)
         (make-color 0.263 0.337 0.282)
         (make-color 0.431 0.506 0.451)
         (make-color 0.431 0.506 0.451)
         (make-color 0.529 0.631 0.471)
         (make-color 0.529 0.631 0.471)
         (make-color 0.333 0.376 0.318)
         (make-color 0.333 0.376 0.318)
         (make-color 0.298 0.376 0.318)
         (make-color 0.298 0.376 0.318)
         (make-color 0.416 0.376 0.318)
         (make-color 0.416 0.376 0.318)
         (make-color 0.416 0.376 0.318)
         (make-color 0.416 0.376 0.318)
         (make-color 0.824 0.725 0.584))]))
     ("marble-rainbow" .
      (marble
       [octaves 2] [lacunarity 1] [gain 1]
       [colorlist
        (list
         (make-color 1 0 0)
         (make-color 1 0 0)
         (make-color 1 1 0)
         (make-color 1 1 0)
         (make-color 0 1 0)
         (make-color 0 1 0)
         (make-color 0 1 1)
         (make-color 0 1 1)
         (make-color 0 0 1)
         (make-color 0 0 1))]))
     ("marble-rgb" .
      (marble
       [colorlist
        (list
         (make-color 0 0 0)
         (make-color 1 0 0)
         (make-color 0 1 0)
         (make-color 0 0 1)
         (make-color 1 1 1))]))
     ("matte" . (matte))
     ("metal" . (metal))
     ("mirror" . (mirror))
     ("plastic" . (plastic))
     ("screen" . (screen))
     ("shiny-metal" . (shiny-metal))
     ("show-xyz" . (show-xyz))
     ("stripes" . (stripes))
     ("wood" . (wood))))
  )

(group bumps
  (for-each
   (lambda (p)
     (let ([name (string-append "bump-" (car p))]
           [disp (cadr p)]
           [color (caddr p)])
       ($build name
         `((render image-simple ,name
             ,default-camera
             ,default-display
             (<scene> make
               [background-color (make-color 0 .3 .3)]
               [objects
                (list
                 (sphere [color ,color]
                   [surface (plastic)]
                   [displacement ,disp]
                   [M (scale 2 2 2)])
                 (plane [center (make-vec 0 -3 0)]
                   [M (matrix-mul (scale 9 9 9) (rotate-x -90))]
                   [surface (matte)]))]
               [lights
                (list
                 (ambient-light [intensity 0.1])
                 (distant-light [position (make-vec 5 5 10)]))]))))))
   `(("crinkly" (crinkly) (color .6 .4 .3))
     ("lumpy" (lumpy) (color .6 .2 .1))
     ("marbled" (marbled) (color .2 .4 .8))))
  )

(group transparent
  (for-each
   (lambda (p)
     (let ([name (string-append "transparent-" (car p))]
           [expr (cdr p)])
       ($build name
         `((render image-simple ,name
             ,default-camera
             ,default-display
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
     ("glass" . (glass))
     ("glow" . (glow))
     ("matte" . (matte))
     ("metal" . (metal))
     ("mirror" . (mirror))
     ("plastic" . (plastic))
     ("screen" . (screen))
     ("shiny-metal" . (shiny-metal))))
  )

(group textures
  (for-each
   (lambda (p)
     (let ([name (string-append "texture-" (car p))]
           [expr (cdr p)])
       ($build name
         `((render image-simple ,name
             ,default-camera
             ,default-display
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
      (let ([tex (make-texture "test-texture" 'black 'black box 0 0)])
        (plane [M (scale 3 3 3)]
          [surface (simple-tex [tex tex])])))
     ("cube" .
      (let ([tex (make-texture "test-texture" 'black 'black box 0 0)])
        (cube [M (matrix-mul (rotate-x 45) (rotate-y 45))]
          [surface (simple-tex [tex tex])])))
     ("sphere" .
      (let ([tex (make-texture "test-texture" 'black 'black box 0 0)])
        (sphere [M (scale 2 2 2)]
          [surface (simple-tex [tex tex])])))
     ("torus" .
      (let ([tex (make-texture "test-texture" 'black 'black box 0 0)])
        (torus [M (scale 2 2 2)]
          [surface (simple-tex [tex tex])])))
     ("plane-st" .
      (plane [M (scale 3 3 3)]
        [surface (show-st)]))
     ("cube-st" .
      (cube [M (matrix-mul (rotate-x 45) (rotate-y 45))]
        [surface (show-st)]))
     ("sphere-st" .
      (sphere [M (scale 2 2 2)]
        [surface (show-st)]))
     ("torus-st" .
      (torus [M (scale 2 2 2)]
        [surface (show-st)]))))
  )

(group csg
  (for-each
   (lambda (p)
     (let ([name (string-append "csg-" (car p))]
           [expr (cdr p)])
       ($build name
         `((render image-simple ,name
             ,default-camera
             ,default-display
             (<scene>
              make
              (background-color (make-color 0 .3 .3))
              (objects
               (list
                ,@expr
                (plane
                 [center (make-vec 0 -1 0)]
                 [M (matrix-mul (rotate-x -90))]
                 (color (make-color 0 .6 0))
                 (surface (matte)))))
              (lights
               (list
                (distant-light
                 (position (make-vec -10 10 10))
                 (color white)
                 (intensity .9))
                (point-light
                 (position (make-vec 1 1 1))
                 (color white)
                 (intensity 1))))))))))
   `(("ops-color"
      (difference
       [center (make-vec -2 0 0)]
       [M (matrix-mul (rotate-x 15) (rotate-y 80))]
       [A (cube
           [color (make-color 1 0 0)]
           [surface (matte)]
           [M (scale .75 .75 .75)])]
       [B (sphere
           (color (make-color 0 0 1))
           (surface (matte)))])
      (union
       [center (make-vec 0 0 0)]
       [M (matrix-mul (rotate-x 15) (rotate-y 80))]
       [A (cube
           [color (make-color 1 0 0)]
           [surface (matte)]
           [M (scale .75 .75 .75)])]
       [B (sphere
           (color (make-color 0 0 1))
           (surface (matte)))])
      (intersect
       [center (make-vec 2 0 0)]
       [M (matrix-mul (rotate-x 15) (rotate-y 80))]
       [A (cube
           [color (make-color 1 0 0)]
           [surface (matte)]
           [M (scale .75 .75 .75)])]
       [B (sphere
           (color (make-color 0 0 1))
           (surface (matte)))]))
     ("ops-marble"
      (difference
       [center (make-vec -2 0 0)]
       [M (matrix-mul (rotate-x 15) (rotate-y 80))]
       [surface (marble)]
       [A (cube
           [color (make-color 1 0 0)]
           [surface (matte)]
           [M (scale .75 .75 .75)])]
       [B (sphere
           (color (make-color 0 0 1))
           (surface (matte)))])
      (union
       [center (make-vec 0 0 0)]
       [M (matrix-mul (rotate-x 15) (rotate-y 80))]
       [surface (marble)]
       [A (cube
           [color (make-color 1 0 0)]
           [surface (matte)]
           [M (scale .75 .75 .75)])]
       [B (sphere
           (color (make-color 0 0 1))
           (surface (matte)))])
      (intersect
       [center (make-vec 2 0 0)]
       [M (matrix-mul (rotate-x 15) (rotate-y 80))]
       [surface (marble)]
       [A (cube
           [color (make-color 1 0 0)]
           [surface (matte)]
           [M (scale .75 .75 .75)])]
       [B (sphere
           (color (make-color 0 0 1))
           (surface (matte)))]))
     ("ops-color2"
      (difference
       [center (make-vec -2 0 0)]
       [M (matrix-mul (scale .75 .75 .75) (rotate-x 15) (rotate-y 80))]
       [A (sphere
           (color (make-color 0 0 1))
           (surface (matte)))]
       [B (torus
           [color (make-color 1 0 0)]
           [surface (matte)])])
      (union
       [center (make-vec 0 0 0)]
       [M (matrix-mul (scale .75 .75 .75) (rotate-x 15) (rotate-y 80))]
       [A (sphere
           (color (make-color 0 0 1))
           (surface (matte)))]
       [B (torus
           [color (make-color 1 0 0)]
           [surface (matte)])])
      (intersect
       [center (make-vec 2 0 0)]
       [M (matrix-mul (scale .75 .75 .75) (rotate-x 15) (rotate-y 80))]
       [A (sphere
           (color (make-color 0 0 1))
           (surface (matte)))]
       [B (torus
           [color (make-color 1 0 0)]
           [surface (matte)])]))
     ("ops-marble2"
      (difference
       [center (make-vec -2 0 0)]
       [M (matrix-mul (scale .75 .75 .75) (rotate-x 15) (rotate-y 80))]
       [surface (marble)]
       [A (sphere
           (color (make-color 0 0 1))
           (surface (matte)))]
       [B (torus
           [color (make-color 1 0 0)]
           [surface (matte)])])
      (union
       [center (make-vec 0 0 0)]
       [M (matrix-mul (scale .75 .75 .75) (rotate-x 15) (rotate-y 80))]
       [surface (marble)]
       [A (sphere
           (color (make-color 0 0 1))
           (surface (matte)))]
       [B (torus
           [color (make-color 1 0 0)]
           [surface (matte)])])
      (intersect
       [center (make-vec 2 0 0)]
       [M (matrix-mul (scale .75 .75 .75) (rotate-x 15) (rotate-y 80))]
       [surface (marble)]
       [A (sphere
           (color (make-color 0 0 1))
           (surface (matte)))]
       [B (torus
           [color (make-color 1 0 0)]
           [surface (matte)])]))
     ))

  (build "csg-all"
    (render image-simple "csg-all"
      ,default-camera
      ,default-display
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

(group filters
  (for-each
   (lambda (p)
     (let ([name (string-append "filter-" (car p))]
           [expr (cdr p)])
       ($build name
         `((render image-simple ,name
             ,default-camera
             ,expr
             (<scene> make
               [background-color (make-color 0 .3 .3)]
               [objects
                (list
                 (plane [center (make-vec 0 -3 0)]
                   [M (matrix-mul (scale 9 9 9) (rotate-x -90))]
                   [surface (checker)]))]
               [lights
                (list
                 (ambient-light [intensity 0.1])
                 (distant-light [position (make-vec 5 5 10)]))]))))))
   `(("none" . (<display> make [x-samples 1] [y-samples 1]
                 [filter #f] [x-width #f] [y-width #f]
                 [gain 1] [gamma 1]))
     ("box" . (<display> make [x-samples 4] [y-samples 4]
                [filter box-filter] [x-width 2/3] [y-width 2/3]
                [gain 1] [gamma 1]))
     ("triangle" . (<display> make [x-samples 4] [y-samples 4]
                     [filter triangle-filter] [x-width 2/3] [y-width 2/3]
                     [gain 1] [gamma 1]))
     ("catmull-rom" . (<display> make [x-samples 4] [y-samples 4]
                        [filter catmull-rom-filter] [x-width 2/3] [y-width 2/3]
                        [gain 1] [gamma 1]))
     ("gaussian" . (<display> make [x-samples 4] [y-samples 4]
                     [filter gaussian-filter] [x-width 2/3] [y-width 2/3]
                     [gain 1] [gamma 1]))
     ("sinc" . (<display> make [x-samples 4] [y-samples 4]
                 [filter sinc-filter] [x-width 2/3] [y-width 2/3]
                 [gain 1] [gamma 1]))))
  )

(group params
  (for-each
   (lambda (p)
     (let ([name (string-append "param-" (car p))]
           [pre (cadr p)]
           [shader (caddr p)])
       ($build name
         `(,@pre
           (render image-simple ,name
             (<camera> make
               (output-width 512)
               (output-height 512)
               (type 'orthographic)
               (position (make-vec 0 0 10))
               (target (make-vec 0 0 0))
               (view
                (<view> make (left -.5) (right 4.5) (bottom -.5) (top 4.5))))
             (<display> make
               (x-samples 1) (y-samples 1)
               (filter gaussian-filter)
               (x-width 2/3) (y-width 2/3)
               (gain 1) (gamma 1))
             (<scene> make
               (background-color (make-color 0 0.3 0.3))
               (objects
                (append
                 (list-of
                  (sphere
                   [center (make-vec x y 0)]
                   [radius 7/16]
                   [color (make-color 0 0.8 0)]
                   [surface ,shader])
                  ([x (iota 5)]
                   [y (iota 5)]))
                 (list
                  (plane
                   [center (make-vec 0 0 -1)]
                   [surface (checker [frequency 2])]))))
               (lights
                (list
                 (distant-light
                  (shadow? #f)
                  (position (make-vec 0 0 10))
                  (color white)
                  (intensity 1))))))))))
   `(("glass"
      ((define xt (make-linear-transform 0 4 0 1))
       (define yt (make-linear-transform 0 4 0 1)))
      (glass [Kr (xt x)] [Kt (yt y)]))
     ("marble"
      ()
      (marble [octaves (+ x 2)] [lacunarity y]))
     ("mirror"
      ((define xt (make-linear-transform 0 4 0 1))
       (define yt (make-linear-transform 0 4 0 1)))
      (mirror [Ks (xt x)] [Kr (yt y)]))
     ("screen"
      ((define xt (make-linear-transform 0 4 0.1 0.5))
       (define yt (make-linear-transform 0 4 3 20)))
      (screen [density (xt x)] [frequency (exact (truncate (yt y)))]))))
  )

(group images
  (build "image-color"
    (let ([width 200]
          [height 200])
      (write-tga
       (make-image width height 0 0
         (lambda (set-pixel)
           (do ([y 0 (+ y 1)]) ((= y height))
             (do ([x 0 (+ x 1)]) ((= x width))
               (set-pixel x y (make-color (/ x width) (/ y height) 0))))))
       "image-color")))

  (build "image-gray"
    (let ([width 200]
          [height 200])
      (write-tga
       (make-image width height 0 0
         (lambda (set-pixel)
           (do ([y 0 (+ y 1)]) ((= y height))
             (do ([x 0 (+ x 1)]) ((= x width))
               (set-pixel x y (* (/ x width) (/ y height)))))))
       "image-gray")))

  (build "image-noise"
    (let* ([width 200]
           [height 200]
           [xt (make-linear-transform 0 (- width 1) 0.0 10.0)]
           [yt (make-linear-transform 0 (- height 1) 0.0 10.0)])
      (write-tga
       (make-image width height 0 0
         (lambda (set-pixel)
           (do ([y 0 (+ y 1)]) ((= y height))
             (do ([x 0 (+ x 1)]) ((= x width))
               (set-pixel x y (noise (make-vec (xt x) (yt y) 0)))))))
       "image-noise")))

  (build "image-hsl"
    (let* ([width 200]
           [height 200]
           [xt (make-linear-transform 0 (- width 1) 0.0 1.0)]
           [yt (make-linear-transform 0 (- height 1) 0.0 1.0)])
      (write-tga
       (make-image width height 0 0
         (lambda (set-pixel)
           (do ([y 0 (+ y 1)]) ((= y height))
             (do ([x 0 (+ x 1)]) ((= x width))
               (set-pixel x y (color "hsl" (xt x) 1 (yt y)))))))
       "image-hsl")))

  (build "image-hsv"
    (let* ([width 200]
           [height 200]
           [xt (make-linear-transform 0 (- width 1) 0.0 1.0)]
           [yt (make-linear-transform 0 (- height 1) 0.0 1.0)])
      (write-tga
       (make-image width height 0 0
         (lambda (set-pixel)
           (do ([y 0 (+ y 1)]) ((= y height))
             (do ([x 0 (+ x 1)]) ((= x width))
               (set-pixel x y (color "hsv" (xt x) 1 (yt y)))))))
       "image-hsv")))
  )

(group voronoi
  (for-each
   (lambda (p)
     (let ([name (string-append "voronoi-" (car p))]
           [dist (cdr p)])
       ($build name
         `((let* ([width 100]
                  [height 100]
                  [xt (make-linear-transform 0 (- width 1) 0.0 10.0)]
                  [yt (make-linear-transform 0 (- height 1) 0.0 10.0)])
             (write-tga
              (make-image width height 0 0
                (lambda (set-pixel)
                  (do ([y 0 (+ y 1)]) ((= y height))
                    (do ([x 0 (+ x 1)]) ((= x width))
                      (set-pixel x y
                        (color (voronoi (make-vec (xt x) (yt y) 0)
                                 1 0 ,dist)))))))
              ,name))))))
   `(("distance" . distance)
     ("distance2" . distance2)
     ("manhattan" . distance-manhattan)
     ("chebyshev" . distance-chebyshev)
     ("quadratic" . distance-quadratic)
     ("minkowski4" . (make-distance-minkowski 4))
     ("minkowski.5" . (make-distance-minkowski 0.5))))

  (for-each
   (lambda (p)
     (let ([name (string-append "voronoi-diff-" (car p))]
           [dist (cdr p)])
       ($build name
         `((let* ([width 100]
                  [height 100]
                  [xt (make-linear-transform 0 (- width 1) 0.0 10.0)]
                  [yt (make-linear-transform 0 (- height 1) 0.0 10.0)])
             (write-tga
              (make-image width height 0 0
                (lambda (set-pixel)
                  (do ([y 0 (+ y 1)]) ((= y height))
                    (do ([x 0 (+ x 1)]) ((= x width))
                      (set-pixel x y
                        (color (voronoi-diff (make-vec (xt x) (yt y) 0)
                                 1 1 0 ,dist)))))))
              ,name))))))
   `(("distance" . distance)
     ("distance2" . distance2)
     ("manhattan" . distance-manhattan)
     ("chebyshev" . distance-chebyshev)
     ("quadratic" . distance-quadratic)
     ("minkowski4" . (make-distance-minkowski 4))
     ("minkowski.5" . (make-distance-minkowski 0.5))))
  )

(write-sources)
(exit)
