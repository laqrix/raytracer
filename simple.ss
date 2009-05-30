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
    (sphere [center (make-vec 233 290 0)]
            [radius 100]
            [shader (shiny [Ka 0] [Kd 1] [Kr .5] [roughness 1])]
            [color (make-color 1 1 0)])
    (sphere [center (make-vec 407 290 0)]
            [radius 100]
            [shader (shiny [Ka 0] [Kd 1] [Kr .5] [roughness 1])]
            [color (make-color 0 1 1)])
    (sphere [center (make-vec 320 140 0)]
            [radius 100]
            [shader (shiny [Ka 0] [Kd 1] [Kr .5] [roughness 1])]
            [color (make-color 1 0 1)]))]
  [lights
   (list
    (<light> copy light-default
             [position (make-vec 0 240 -100)]
             [shader (point-light [color (make-color 1 1 1)] [intensity 20000])])
    (<light> copy light-default
             [position (make-vec 640 240 -10000)]
             [shader (point-light [color (make-color .6 .7 1)] [intensity 100000000/2])]))])
