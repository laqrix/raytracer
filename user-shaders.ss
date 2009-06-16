(define-shader matte ([Ka 1] [Kd 1])
  (let ([Nf (faceforward (vec-normalize normal) incoming)])
    (color-color-mul
     (object-color object)
     (color-color-plus
      (color-num-mul ((ambient)) Ka)
      (color-num-mul ((diffuse [N Nf])) Kd)))))

(define-shader metal ([Ka 1] [Ks 1] [roughness .05])
  (let* ([Nf (faceforward (vec-normalize normal) incoming)]
         [V (vec-normalize (vec-reverse incoming))])
    (color-color-mul
     (object-color object)
     (color-color-plus
      (color-num-mul ((ambient)) Ka)
      (color-num-mul ((specular [N Nf] [eye V] [roughness roughness])) Ks)))))

(define-shader shiny-metal ([Ka 1] [Kd .1] [Ks 1] [roughness .2] [Kr .8])
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

(define-shader plastic ([Ks .5] [Kd .5] [Ka 1] [roughness .1]
                        [specularcolor (make-color 1 1 1)])
  (let ([Nf (faceforward (vec-normalize normal) incoming)]
        [V  (vec-normalize (vec-reverse incoming))])
    (color-color-plus
     (color-color-mul
      (object-color object)
      (color-color-plus
       (color-num-mul ((ambient)) Ka)
       (color-num-mul ((diffuse [N Nf])) Kd)))
     (color-color-mul 
      (color-num-mul ((specular [N Nf] [eye V] [roughness roughness])) Ks)
      specularcolor))))

(define-shader stripes ([Kd 1] [Ka .5] [frequency 10]
                        [blackcolor (make-color 0 0 0)])
  (let ([pnt (point->surface object intersect-point)]
        [Nf (faceforward (vec-normalize normal) incoming)])
    (let* ([t (vec-j pnt)]
           [tmod (fmod (* t frequency) 1)])
      (color-color-mul
       (if (< tmod .5)
           (object-color object)
           blackcolor)
       (color-color-plus
        (color-num-mul ((ambient)) Ka)
        (color-num-mul ((diffuse [N Nf])) Kd))))))

(define-shader checker ([Kd 1] [Ka .5] [frequency 4]
                        [blackcolor (make-color 0 0 0)])
  (let ([pnt (point->surface object intersect-point)]
        [Nf (faceforward (vec-normalize normal) incoming)])
    (let ([xmod (fmod (* (vec-i pnt) frequency) (+ 1 EPSILON))]
          [ymod (fmod (* (vec-j pnt) frequency) (+ 1 EPSILON))]
          [zmod (fmod (* (vec-k pnt) frequency) (+ 1 EPSILON))])
      (color-color-mul
       (if (< zmod .5)
           (if (< xmod .5)
               (if (< ymod .5)
                   (object-color object)
                   blackcolor)
               (if (< ymod .5)
                   blackcolor
                   (object-color object)))
           (if (< xmod .5)
               (if (< ymod .5)
                   blackcolor
                   (object-color object))             
               (if (< ymod .5)
                   (object-color object)
                   blackcolor)))
       (color-color-plus
        (color-num-mul ((ambient)) Ka)
        (color-num-mul ((diffuse [N Nf])) Kd))))))

(define-shader mirror ([Ks 1] [Kr 1] [roughness 0.05])
  (let* ([Nf (faceforward (vec-normalize normal) incoming)]
         [IN (vec-normalize incoming)]
         [V (vec-reverse IN)]
         [R (reflect IN Nf)])
    (color-color-plus
     (color-num-mul ((specular [N Nf] [eye V] [roughness roughness])) Ks)
     (sample-environment scene intersect-point R Kr depth))))

(define-shader simple-texmap
  ([Ka 1] [Kd 1] [Ks 0.5] [roughness 0.1] [specularcolor (make-color 1 1 1)]
   [texture #f] [normals #f]
   [sstart 0] [sscale 1] [tstart 0] [tscale 1])
  (let* ([st (point->texture object
               (point->surface object intersect-point))]
         [ss (/ (- (vec-i st) sstart) sscale)]
         [tt (/ (- (vec-j st) tstart) tscale)]
         [N (if normals
                (vec-vec-plus normal ; this is a guess for normal mapping
                  (vec-vec-sub 
                   (make-vec 0 0 1)
                   (normals ss tt)))
                normal)]
         [Nf (faceforward (vec-normalize N) incoming)]
         [V  (vec-normalize (vec-reverse incoming))])
    (color-color-plus
     (color-color-mul
      (if texture
          ;; May also want to use opacity from the texture file here
          (texture ss tt)
          (object-color object))
      (color-color-plus
       (color-num-mul ((ambient)) Ka)
       (color-num-mul ((diffuse [N Nf])) Kd)))
     (color-color-mul 
      (color-num-mul ((specular [N Nf] [eye V] [roughness roughness])) Ks)
      specularcolor))))

(define-shader marble
  ([Ks .4] [Kd .6] [Ka .1] [roughness .1] [specularcolor (make-color 1 1 1)]
   [octaves 6] [lacunarity 2] [gain .5]
   [colorlist 
    (list 
     (make-color .71 .71 1.0)          ; pale blue
     (make-color .71 .71 1.0)          ; pale blue
     (make-color .57 .57 .86)          ; medium blue
     (make-color .57 .57 .86)          ; medium blue
     (make-color .57 .57 .86)          ; medium blue
     (make-color .71 .71 1.0)          ; pale blue
     (make-color .71 .71 1.0)          ; pale blue
     (make-color .43 .43 .74)          ; medium dark blue
     (make-color .43 .43 .74)          ; medium dark blue
     (make-color .29 .29 .57)          ; dark blue
     (make-color .29 .29 .57)          ; dark blue
     (make-color .71 .71 1.0)          ; pale blue
     (make-color .29 .29 .57)          ; dark blue
     )])
  (let* ([pnt (point->surface object intersect-point)]
         [Nf (faceforward (vec-normalize normal) incoming)]
         [IN (vec-normalize incoming)]
         [V (vec-reverse IN)]
         [color (color-spline
                 (abs (sin (vec-i (vec-num-plus pnt
                                    (turbulence pnt octaves lacunarity gain)))))
                  colorlist)])
    (color-color-plus
     (color-color-mul
      color
      (color-color-plus
       (color-num-mul ((ambient)) Ka)
       (color-num-mul ((diffuse [N Nf])) Kd)))
     (color-num-mul ((specular [N Nf] [eye V] [roughness roughness])) Ks))))

(define-shader granite ([Ka .2] [Kd .8])
  (let ([Nf (faceforward (vec-normalize normal) incoming)]
        [sum
         (let lp ([i 0] [sum 0] [freq 1])
           (if (= i 6)
               sum
               (lp (+ i 1)
                 (+ sum (/ (abs (- .5 (noise (vec-num-mul incoming
                                               (* 4 freq)))))
                           freq))
                 (* 2 freq))))])
    (color-color-mul
     (object-color object)
     (color-num-mul
      (color-color-plus
       (color-num-mul ((ambient)) Ka)
       (color-num-mul ((diffuse [N Nf])) Kd))
      sum))))

(define (oaktexture Pshad 
          ringfreq ringnoise ringnoisefreq 
          grainfreq
          trunkwobble trunkwobblefreq
          angularwobble angularwobblefreq
          ringy grainy)
  (let* ([offset (vfBm (vec-num-mul Pshad ringnoisefreq) 2 4 .5)]
         [Pring (vec-vec-plus Pshad (vec-num-mul offset ringnoise))]
         [Pring (vec-vec-plus Pring 
                  (vec-num-mul 
                   (vec-vec-mul 
                    (vsnoise (* (vec-k Pshad) trunkwobblefreq))
                    (make-vec 1 1 0)) 
                   trunkwobble))]
         [r2 (+ (sqr (vec-i Pring)) (sqr (vec-j Pring)))]
         [r (* (sqrt r2) ringfreq)]
         [r (+ r (* angularwobble (smoothstep 0 5 r) 
                    (snoise (vec-num-mul  
                             (vec-vec-mul Pring (make-vec 1 1 .1))
                             angularwobblefreq))))]
         [dr (filterwidth r)]
         [r (+ r (* .5 (filteredsnoise r dr)))]
         [inring (smoothpulsetrain .1 .55 .7 .95 1 r)])
    (* inring ringy)))

(define-shader wood
  ([Ka 1] [Kd 1] [Ks .25]
   [roughness .2]
   [ringfreq 8] [ringnoise .02] [ringnoisefreq 1]
   [grainfreq 25]
   [trunkwobble .15] [trunkwobblefreq .025]
   [angularwobble 1] [angularwobblefreq 1.5]
   [lightwood (make-color .5 .2 .067)]
   [darkwood (make-color .15 .077 .028)]
   [ringy 1] [grainy 1])
  (let* ([pnt (point->surface object intersect-point)]
         [Nf (faceforward (vec-normalize normal) incoming)]
         [IN (vec-normalize incoming)]
         [V (vec-reverse IN)]
         [wood (oaktexture pnt
                 ringfreq ringnoise ringnoisefreq 
                 grainfreq
                 trunkwobble trunkwobblefreq
                 angularwobble angularwobblefreq
                 ringy grainy)])
    (color-color-plus
     (color-color-mul
      (color-mix lightwood darkwood wood)
      (color-color-plus
       (color-num-mul ((ambient)) Ka)
       (color-num-mul ((diffuse [N Nf])) Kd)))
     (color-num-mul ((specular [N Nf] [eye V] [roughness roughness])) Ks))))
