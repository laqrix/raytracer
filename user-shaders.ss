(define-shader constant ()
  (color-mul Os Cs))

(define-shader matte ([Ka 1] [Kd 1])
  (let ([Nf (faceforward (vec-normalize N) I)])
    (color-mul Os Cs
      (color-add
       (color-num-mul ((ambient)) Ka)
       (color-num-mul ((diffuse [N Nf])) Kd)))))

(define-shader metal ([Ka 1] [Ks 1] [roughness .05])
  (let* ([Nf (faceforward (vec-normalize N) I)]
         [V (vec-normalize (vec-reverse I))])
    (color-mul Os Cs
      (color-add
       (color-num-mul ((ambient)) Ka)
       (color-num-mul ((specular [N Nf] [eye V] [roughness roughness])) Ks)))))

(define-shader shiny-metal ([Ka 1] [Kd .1] [Ks 1] [roughness .2] [Kr .8])
  (let* ([Nf (faceforward (vec-normalize N) I)]
         [IN (vec-normalize I)]
         [V (vec-reverse IN)]
         [R (reflect IN Nf)])
    (color-mul Os Cs
      (color-add
       (color-num-mul ((ambient)) Ka)
       (color-num-mul ((diffuse [N Nf])) Kd)
       (color-num-mul ((specular [N Nf] [eye V] [roughness roughness])) Ks)
       (sample-environment P R Kr)))))

(define-shader plastic ([Ks .5] [Kd .5] [Ka 1] [roughness .1]
                        [specularcolor white])
  (let ([Nf (faceforward (vec-normalize N) I)]
        [V  (vec-normalize (vec-reverse I))])
    (color-add
     (color-mul Os Cs
       (color-add
        (color-num-mul ((ambient)) Ka)
        (color-num-mul ((diffuse [N Nf])) Kd)))
     (color-mul
      (color-num-mul ((specular [N Nf] [eye V] [roughness roughness])) Ks)
      specularcolor))))

(define-shader stripes ([Kd 1] [Ka .5] [frequency 10] [blackcolor black])
  (let ([pnt (point->surface ($object) P)]
        [Nf (faceforward (vec-normalize N) I)])
    (let* ([t (vec-j pnt)]
           [tmod (mod (* t frequency) 1)])
      (color-mul Os
        (if (< tmod .5)
            Cs
            blackcolor)
        (color-add
         (color-num-mul ((ambient)) Ka)
         (color-num-mul ((diffuse [N Nf])) Kd))))))

(define-shader checker ([Kd 1] [Ka .5] [frequency 4] [blackcolor black])
  (let ([pnt (point->surface ($object) P)]
        [Nf (faceforward (vec-normalize N) I)])
    (let ([xmod (mod (* (vec-i pnt) frequency) (+ 1 EPSILON))]
          [ymod (mod (* (vec-j pnt) frequency) (+ 1 EPSILON))]
          [zmod (mod (* (vec-k pnt) frequency) (+ 1 EPSILON))])
      (color-mul Os
        (if (< zmod .5)
            (if (< xmod .5)
                (if (< ymod .5)
                    Cs
                    blackcolor)
                (if (< ymod .5)
                    blackcolor
                    Cs))
            (if (< xmod .5)
                (if (< ymod .5)
                    blackcolor
                    Cs)             
                (if (< ymod .5)
                    Cs
                    blackcolor)))
        (color-add
         (color-num-mul ((ambient)) Ka)
         (color-num-mul ((diffuse [N Nf])) Kd))))))

(define-shader mirror ([Ks 1] [Kr 1] [roughness 0.05])
  (let* ([Nf (faceforward (vec-normalize N) I)]
         [IN (vec-normalize I)]
         [V (vec-reverse IN)]
         [R (reflect IN Nf)])
    ;; TODO: Revisit for opacity and color?
    (color-add
     (color-num-mul ((specular [N Nf] [eye V] [roughness roughness])) Ks)
     (sample-environment P R Kr))))

(define-shader simple-tex
  ([Ka 1] [Kd 1] [Ks 0.5] [roughness 0.1] [specularcolor white]
   [tex #f]
   [sflip? #f] [tflip? #f]
   [sstart 0] [sscale 1] [tstart 0] [tscale 1])
  (let ([s (if sflip? (- 1 s) s)]
        [t (if tflip? (- 1 t) t)])
    (let ([ss (/ (- s sstart) sscale)]
          [tt (/ (- t tstart) tscale)]
          [Nf (faceforward (vec-normalize N) I)]
          [V  (vec-normalize (vec-reverse I))])
      (color-add
       (color-mul Os
         (if tex
             ;; May also want to use opacity from the texture file here
             (texture tex ss tt)
             Cs)
         (color-add
          (color-num-mul ((ambient)) Ka)
          (color-num-mul ((diffuse [N Nf])) Kd)))
       (color-mul
        (color-num-mul ((specular [N Nf] [eye V] [roughness roughness])) Ks)
        specularcolor)))))

(define-shader marble
  ([Ks .4] [Kd .6] [Ka .1] [roughness .1] [specularcolor white]
   [octaves 6] [lacunarity 2] [gain .5]
   [colorlist 
    (list 
     (make-color .71 .71 1.0)           ; pale blue
     (make-color .71 .71 1.0)           ; pale blue
     (make-color .57 .57 .86)           ; medium blue
     (make-color .57 .57 .86)           ; medium blue
     (make-color .57 .57 .86)           ; medium blue
     (make-color .71 .71 1.0)           ; pale blue
     (make-color .71 .71 1.0)           ; pale blue
     (make-color .43 .43 .74)           ; medium dark blue
     (make-color .43 .43 .74)           ; medium dark blue
     (make-color .29 .29 .57)           ; dark blue
     (make-color .29 .29 .57)           ; dark blue
     (make-color .71 .71 1.0)           ; pale blue
     (make-color .29 .29 .57)           ; dark blue
     )])
  (let* ([pnt (point->surface ($object) P)]
         [Nf (faceforward (vec-normalize N) I)]
         [IN (vec-normalize I)]
         [V (vec-reverse IN)]
         [color (color-spline
                 (abs (sin (vec-i (vec-num-add pnt
                                    (turbulence pnt octaves lacunarity gain)))))
                 colorlist)])
    (color-add
     (color-mul Os color
       (color-add
        (color-num-mul ((ambient)) Ka)
        (color-num-mul ((diffuse [N Nf])) Kd)))
     (color-num-mul ((specular [N Nf] [eye V] [roughness roughness])) Ks))))

(define-shader granite ([Ka .2] [Kd .8])
  (let ([Nf (faceforward (vec-normalize N) I)]
        [sum
         (let lp ([i 0] [sum 0] [freq 1])
           (if (= i 6)
               sum
               (lp (+ i 1)
                 (+ sum (/ (abs (- .5 (noise (vec-num-mul I
                                               (* 4 freq)))))
                           freq))
                 (* 2 freq))))])
    (color-mul Cs
      (color-num-mul
       (color-add
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
         [Pring (vec-add Pshad (vec-num-mul offset ringnoise))]
         [Pring (vec-add Pring 
                  (vec-num-mul 
                   (vec-mul 
                    (vsnoise (* (vec-k Pshad) trunkwobblefreq))
                    (make-vec 1 1 0)) 
                   trunkwobble))]
         [r2 (+ (sqr (vec-i Pring)) (sqr (vec-j Pring)))]
         [r (* (sqrt r2) ringfreq)]
         [r (+ r (* angularwobble (smoothstep 0 5 r) 
                    (snoise (vec-num-mul  
                             (vec-mul Pring (make-vec 1 1 .1))
                             angularwobblefreq))))]
         [dr (filterwidth r)]
         [r (+ r (* .5 (filteredsnoise r dr)))]
         [inring (smoothpulsetrain .1 .55 .7 .95 1 r)])
    (* inring ringy)))

(define (material-plastic Nf base-color Ka Kd Ks roughness)
  (color-add
   (color-mul base-color
     (color-add
      (color-num-mul ((ambient)) Ka)
      (color-num-mul ((diffuse [N Nf])) Kd)))
   (color-num-mul ((specular [N Nf]
                     [eye (vec-reverse (vec-normalize I))]
                     [roughness roughness]))
     Ks)))

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
  (let* ([pnt (point->surface ($object) P)]
         [Nf (faceforward (vec-normalize N) I)]
         ;;[IN (vec-normalize I)]
         ;;[V (vec-reverse IN)]
         [wood (oaktexture pnt
                 ringfreq ringnoise ringnoisefreq 
                 grainfreq
                 trunkwobble trunkwobblefreq
                 angularwobble angularwobblefreq
                 ringy grainy)]
         [Cwood (color-mix lightwood darkwood wood)])
    ;; TODO: Try to displace Nf
    (color-mul Os
      (material-plastic Nf Cwood Ka Kd (* Ks (- 1 (* .5 wood))) roughness))))

(define-shader glow ([attenuation 2])
  (let ([falloff (vec-dot I N)])
    (if (< falloff 0)
        (let ([n (expt (/ (* falloff falloff)
                          (* (vec-dot I I)
                             (vec-dot N N)))
                   attenuation)])
          (values (color-num-mul Cs n) (make-color n n n)))
        (values Cs transparent))))

(define-shader screen
  ([Ka 1] [Kd .75] [Ks .4] [roughness .1] [specularcolor white]
   [density .25] [frequency 20])
  (if (or (< (mod (* s frequency) 1) density)
          (< (mod (* t frequency) 1) density))
      (let ([Nf (faceforward (vec-normalize N) I)]
            [V  (vec-normalize (vec-reverse I))])
        (values
         (color-mul Os
           (color-add
            (color-mul Cs
              (color-add
               (color-num-mul ((ambient)) Ka)
               (color-num-mul ((diffuse [N Nf])) Kd)))
            (color-mul
             (color-num-mul ((specular [N Nf] [eye V] [roughness roughness]))
               Ks)
             specularcolor)))
         opaque))
      (values black transparent)))

(define-shader glass
  ([Ka .2] [Kd 0] [Ks .5] [roughness .05]
   [Kr 1] [Kt 1] [eta 1.5] [transmitcolor white])
  (let* ([Nf (faceforward (vec-normalize N) I)]
         [IN (vec-normalize I)]
         [V (vec-reverse IN)])
    (let-values
     ([(kr kt Rfldir Rfrdir)
       (fresnel IN Nf (if (< (vec-dot I N) 0)
                          (/ 1.0 eta)
                          eta))])
     (let* ([kt (- 1 kr)]
            [kr (* kr Kr)]
            [kt (* kt Kt)]
            [Crefl (sample-environment P (vec-normalize Rfldir) kr)]
            [Crefr (sample-environment P (vec-normalize Rfrdir) kt)])
       (color-add
        (color-mul Cs
          (color-add
           (color-num-mul ((ambient)) Ka)
           (color-num-mul ((diffuse [N Nf])) Kd)))
        Crefl
        (color-num-mul
         ((specular [N Nf] [eye V] [roughness roughness]))
         Ks)                           ; not quite what RenderMan does
        (color-mul transmitcolor Crefr))))))

(define-shader show-st ()
  (make-color s t 0))

(define-shader show-xyz
  ([xmin -1] [ymin -1] [zmin -1] [xmax 1] [ymax 1] [zmax 1])
  (let* ([scale
          (make-vec (/ 1 (- xmax xmin)) (/ 1 (- ymax ymin)) (/ 1 (- zmax zmin)))]
         [zero (make-vec xmin ymin zmin)]
         [pnt (point->surface ($object) P)]
         [cubeP (vec-mul (vec-sub pnt zero) scale)])
    (make-color (vec-i cubeP) (vec-j cubeP) (vec-k cubeP))))

(define-shader simple-bumpmap
  ([normals #f]
   [sflip? #f] [tflip? #f]
   [sstart 0] [sscale 1] [tstart 0] [tscale 1])
  (let ([s (if sflip? (- 1 s) s)]
        [t (if tflip? (- 1 t) t)])
    (let ([ss (/ (- s sstart) sscale)]
          [tt (/ (- t tstart) tscale)])
      (let ([pnt (point->surface ($object) P)]
            [c (texture normals ss tt)])
        (bump-normal pnt N
          (lambda (x y z)
            (- 1 (+ (* x (color-r c)) (* y (color-g c)) (* z (color-b c))))))))))

(module perlin-helpers
  (noise stripes turbulence)
  ;; Based on Ken Perlin's bump textures
  (define (noise x y z freq)
    (let* ([x1 (- (* .707 x) (* .707 z))]
           [z1 (+ (* .707 x) (* .707 z))]
           [y1 (+ (* .707 x1) (* .707 y))]
           [x1 (- (* .707 x1) (* .707 y))])
      (perlin-noise (+ (* freq x1) 100) (* freq y1) (* freq z1))))

  (define (stripes x f)
    (- (sqr (+ .5 (* .5 (sin (* f 2 pi x))))) .5))

  (define (turbulence x y z f)
    (let lp ([f f] [t -0.5])
      (if (> f 25)
          t
          (lp (* f 2) (+ t (abs (/ (noise x y z f) f)))))))
  )

(define-shader lumpy ()
  (import perlin-helpers)
  (let ([pnt (point->surface ($object) P)])
    (bump-normal pnt N
      (lambda (x y z)
        (* 0.03 (noise x y z 8))))))

(define-shader crinkly ()
  (import perlin-helpers)
  (let ([pnt (point->surface ($object) P)])
    (bump-normal pnt N
      (lambda (x y z)
        (* -0.1 (turbulence x y z 1))))))

(define-shader marbled ()
  (import perlin-helpers)
  (let ([pnt (point->surface ($object) P)])
    (bump-normal pnt N
      (lambda (x y z)
        (* 0.01 (stripes (+ x (* 2 (turbulence x y z 1))) 1.6))))))
