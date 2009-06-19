(load "user-shaders.ss")

(render image-simple "simple" 640 480
  (<camera> make
    [translation (make-vec 320 240 -1000)]
    [target (make-vec 320 240 0)]
    [distance 1]
    [view (<view> make [left 0] [right 639] [bottom 0] [top 479])])
  (<scene> make
    [background-color (make-color 0 0 0)]
    [objects
     (list
      (sphere
       [center (make-vec 233 290 0)]
       [radius 100]
       [surface (shiny-metal [Ka 0] [Kd 1] [Kr .5] [roughness 1])]
       [color (make-color 1 1 0)])
      (sphere
       [center (make-vec 407 290 0)]
       [radius 100]
       [surface (shiny-metal [Ka 0] [Kd 1] [Kr .5] [roughness 1])]
       [color (make-color 0 1 1)])
      (sphere
       [center (make-vec 320 140 0)]
       [radius 100]
       [surface (shiny-metal [Ka 0] [Kd 1] [Kr .5] [roughness 1])]
       [color (make-color 1 0 1)]))]
    [lights
     (list
      (distant-light
       [position (make-vec 0 240 -100)]
       [color (make-color 1 1 1)]
       [intensity 1])
      (distant-light
       [position (make-vec 640 240 -10000)]
       [color (make-color .6 .7 1)]
       [intensity .5]))]))
