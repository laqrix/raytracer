(define-shader matte ([Ka 1] [Kd 1])
  (let ([Nf (faceforward (vec-normalize normal) incoming)])
    (color-color-mul
     (object-color object)
     (color-color-plus
      (color-num-mul ((ambient)) Ka)
      (color-num-mul ((diffuse [N Nf])) Kd)))))

(<scene> make
  [background-color (make-color 0 0 0)]
  [objects
   (list
    (sphere [center (make-vec 233 290 0)]
            [radius 100]
            [shader (matte [Kd 1000])]
            [color (make-color 1 1 0)])
    (sphere [center (make-vec 407 290 0)]
            [radius 100]
            [shader (matte [Kd 1000])]
            [color (make-color 0 1 1)])
    (sphere [center (make-vec 320 140 0)]
            [radius 100]
            [shader (matte [Kd 1000])]
            [color (make-color 1 0 1)]))]
  [lights
   (list
    (<light> copy light-default
             [position (make-vec 0 240 -100)]
             [shader (point-light [color (make-color 1 1 1)] [intensity 100])])
    (<light> copy light-default
             [position (make-vec 640 240 -10000)]
             [shader (point-light [color (make-color .6 .7 1)] [intensity 100000])]))])
