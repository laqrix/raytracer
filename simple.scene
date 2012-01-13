(load "user-shaders.ss")

(render image-simple "simple"
  (<camera> make
    [output-width 640]
    [output-height 480]
    [type 'perspective]
    [position (make-vec 320 240 1000)]
    [target (make-vec 320 240 0)]
    [view (<view> make [left -320] [right 320] [bottom -240] [top 240])])
  (<display> make
    [x-samples 2]
    [y-samples 2]
    [filter gaussian-filter]
    [x-width 2/3] [y-width 2/3]
    [gain 1] [gamma 1])
  (<scene> make
    [background-color black]
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
       [position (make-vec 0 240 100)]
       [color white]
       [intensity 1])
      (distant-light
       [position (make-vec 640 240 10000)]
       [color (make-color .6 .7 1)]
       [intensity .5]))]))
