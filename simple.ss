(<scene> make
  [background-color (make-color 0 0 0)]
  [objects
   (list
    (sphere [center (make-vec 233 290 0)]
            [radius 100]
            [material
             (material [diffuse (make-color 1 1 0)]
                       [reflection .5])])
    (sphere [center (make-vec 407 290 0)]
            [radius 100]
            [material
             (material [diffuse (make-color 0 1 1)]
                       [reflection .5])])
    (sphere [center (make-vec 320 140 0)]
            [radius 100]
            [material
             (material [diffuse (make-color 1 0 1)]
                       [reflection .5])]))]
  [lights
   (list
    (point-light [origin (make-vec 0 240 -100)])
    (point-light [origin (make-vec 640 240 -10000)]))])
