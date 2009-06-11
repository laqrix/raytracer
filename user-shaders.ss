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

#;;
(define-shader stripes ([Kd 1] [Ka .5] [frequency 10]
                        [blackcolor (make-color 0 0 0)])
  (let ([pnt (object.PointToShaderSpace intersect-point)]
        [Nf (faceforward (vec-normalize normal) incoming)])
    (let* ([t (pnt-y pnt)]
           [tmod (fmod (* t frequency) 1)])
      (color-color-mul
       (if (< tmod .5)
           (object-color object)
           blackcolor)
       (color-color-plus
        (color-num-mul ((ambient)) Ka)
        (color-num-mul ((diffuse [N Nf]) Kd)))))))
