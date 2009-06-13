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
                (vec-vec-sub normal (normals ss tt))
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
