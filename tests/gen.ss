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

(build "basic"
  (define-shader matte ([Ka 1] [Kd 1])
    (let ([Nf (faceforward (vec-normalize normal) incoming)])
      (color-color-mul
       (object-color object)
       (color-color-plus
        (color-num-mul ((ambient)) Ka)
        (color-num-mul ((diffuse [N Nf])) Kd)))))
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
      (<light> copy light-default
               [position (make-vec -10 10 10)]
               [shader (point-light [color (make-color 1 1 1)]
                                    [intensity 50])]))]))

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
      (<light> copy light-default
               [position (make-vec -5 0 5)]
               [shader (point-light [color (make-color 1 1 1)]
                                    [intensity 15])])
      (<light> copy light-default
               [position (make-vec 0 0 10)]
               [shader (point-light [color (make-color .6 .7 1)]
                                    [intensity 10])]))]))

(exit)
