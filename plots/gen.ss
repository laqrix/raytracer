(define sources '())

(define (write-sources)
  (let ([op (open-output-file ".src" 'replace)])
    (fprintf op "src :=")
    (for-each
     (lambda (s) (fprintf op " ~a" s))
     sources)
    (fprintf op "\n\n")
    #;(let-values ([(keys vals) (hashtable-entries groups)])
      (vector-for-each
       (lambda (group files)
         (fprintf op "~a:" group)
         (for-each
          (lambda (file) (fprintf op " ~a" file))
          files)
         (fprintf op "\n\n"))
       keys vals))))

(define colors
  (list-of
   (string-append tint color)
   ([tint '("heavy" "deep" "dark")]
    [color '("red" "green" "blue" "cyan" "magenta")])))

(define (plot filename num-points xmin xmax . fs)
  (set! sources (cons filename sources))
  (let* ([op (open-file-output-port filename
               (file-options no-fail)
               (buffer-mode block)
               (make-transcoder (utf-8-codec) (eol-style none)))]
         [ls (iota (+ num-points 1))]
         [xmin (inexact xmin)]
         [xmax (inexact xmax)]
         [xt (make-linear-transform 0 num-points xmin xmax)]
         [xs (map (lambda (x) (inexact (xt x))) ls)]
         [vars (map (lambda (f) (gensym)) fs)]
         [ys (map (lambda (f) (map f xs)) fs)]
         [ymin (fold-left
                (lambda (acc x) (if (list? x) (apply min acc x) (min x acc)))
                +inf.0 ys)]
         [ymax (fold-left
                (lambda (acc x) (if (list? x) (apply max acc x) (max x acc)))
                -inf.0 ys)])
    (fprintf op "import graph;\n")
    (fprintf op "size(500,500,IgnoreAspect);\n")
    (fprintf op "xlimits(~a,~a);\n" xmin xmax)
    (fprintf op "ylimits(~a,~a);\n" ymin ymax)
    (fprintf op "xaxis(BottomTop,LeftTicks);\n")
    (fprintf op "yaxis(LeftRight,RightTicks);\n")
    (fprintf op "\n")
    (fprintf op "real[] x={~{~a,~}};\n" xs)
    (for-each
     (lambda (v y) (fprintf op "real[] ~a={~{~a,~}};\n" v y))
     vars ys)
    (fprintf op "\n")
    (for-each
     (lambda (v c)
       (fprintf op "draw(graph(x,~a),~a);\n" v c))
     vars (list-head colors (length vars)))
    (close-port op)))

(plot "step.asy" 100 0 1
  (lambda (x) (step .5 x))
  (lambda (x) (smoothstep .25 .75 x)))

(plot "pulse.asy" 100 0 1
  (lambda (x) (pulse .25 .75 x))
  (lambda (x) (smoothpulse .2 .3 .7 .8 x)))

(plot "filters.asy" 100 -1 1
  (lambda (x) (box-filter x 0 2 2))
  (lambda (x) (triangle-filter x 0 2 2))
  (lambda (x) (catmull-rom-filter x 0 2 2))
  (lambda (x) (gaussian-filter x 0 2 2))
  (lambda (x) (sinc-filter x 0 2 2)))

(plot "noise.asy" 100 0 10
  (lambda (x) (noise x)))

(plot "snoise.asy" 100 0 10
  (lambda (x) (snoise x))
  (lambda (x) (filteredsnoise x .3))
  (lambda (x) (filteredsnoise x .4)))

(plot "fBm-octaves.asy" 100 0 10
  (lambda (x) (fBm (make-vec x 0 0) 1 1 1))
  (lambda (x) (fBm (make-vec x 0 0) 2 1 1))
  (lambda (x) (fBm (make-vec x 0 0) 3 1 1))
  (lambda (x) (fBm (make-vec x 0 0) 4 1 1)))

(plot "fBm-lacunarity.asy" 100 0 10
  (lambda (x) (fBm (make-vec x 0 0) 1 1 1))
  (lambda (x) (fBm (make-vec x 0 0) 2 2 1))
  (lambda (x) (fBm (make-vec x 0 0) 3 3 1))
  (lambda (x) (fBm (make-vec x 0 0) 4 4 1)))

(plot "turbulence-octaves.asy" 100 0 10
  (lambda (x) (turbulence (make-vec x 0 0) 1 1 1))
  (lambda (x) (turbulence (make-vec x 0 0) 2 1 1))
  (lambda (x) (turbulence (make-vec x 0 0) 3 1 1))
  (lambda (x) (turbulence (make-vec x 0 0) 4 1 1)))

(plot "turbulence-lacunarity.asy" 100 0 10
  (lambda (x) (turbulence (make-vec x 0 0) 1 1 1))
  (lambda (x) (turbulence (make-vec x 0 0) 2 2 1))
  (lambda (x) (turbulence (make-vec x 0 0) 3 3 1))
  (lambda (x) (turbulence (make-vec x 0 0) 4 4 1)))

(for-each
 (lambda (x)
   (let ([name (car x)]
         [internal-ior (cdr x)])
     (define N (make-vec 0 1 0))
     (define external-ior 1)
     (define eta (/ external-ior internal-ior))
     (plot (format "fresnel~a.asy" name) 100 0 90
       (lambda (x)
         (let ([I (vec-reverse (mat-vec-mul (rotate-z x) N))])
           (let-values ([(kr kt vr vt) (fresnel I N eta)])
             kr)))
       (lambda (x)
         (let ([I (vec-reverse (mat-vec-mul (rotate-z x) N))])
           (let-values ([(kr kt vr vt) (fresnel I N eta)])
             kt))))))
 `(("1-vacuum" . 1)
   ("2-water" . 1.333)
   ("3-glass" . 1.5)
   ("4-ior2" . 2)
   ("5-diamond" . 2.419)
   ("6-ior3" . 3)
   ("7-silicon" . 3.96)))

(for-each
 (lambda (ang)
   (let* ([N (make-vec 0 1 0)]
          [external-ior 1]
          [internal-ior 1.5]
          [eta (/ external-ior internal-ior)]
          [I (vec-reverse (mat-vec-mul (rotate-z ang) N))])
     (let-values ([(kr kt vr vt) (fresnel I N eta)])
       (let* ([filename (format "fresnel-~4,1,,,'0f.asy" ang)]
              [op (open-file-output-port filename
                    (file-options no-fail)
                    (buffer-mode block)
                    (make-transcoder (utf-8-codec) (eol-style none)))])
         (set! sources (cons filename sources))
         (fprintf op "import graph;\n")
         (fprintf op "size(500,500);\n")
         (fprintf op "xlimits(-1,1);\n")
         (fprintf op "ylimits(-1,1);\n")
         (fprintf op "xaxis(BottomTop, Ticks(\"%\",extend=true, pTick=lightgrey));\n")
         (fprintf op "yaxis(LeftRight, Ticks(\"%\",extend=true, pTick=lightgrey));\n")
         (fprintf op "\n")
         (fprintf op "pair i=(~a,~a);\n" (- (vec-i I)) (- (vec-j I)))
         (fprintf op "pair r=(~a,~a);\n" (vec-i vr) (vec-j vr))
         (fprintf op "pair t=(~a,~a);\n" (vec-i vt) (vec-j vt))
         (fprintf op "\n")
         (fprintf op "draw((-1,0)--(1,0));\n")
         (fprintf op "draw((0,-1)--(0,1),dotted);\n")
         (fprintf op "draw(Label(\"$N$\",EndPoint),(0,0)--(0,1),Arrow);\n")
         (fprintf op "draw(Label(\"$I$\",BeginPoint),i--(0,0),Arrow);\n")
         (fprintf op "draw(Label(\"$r$\",EndPoint),(0,0)--r,red,Arrow);\n")
         (fprintf op "draw(Label(\"$t$\",EndPoint),(0,0)--t,blue,Arrow);\n")
         (fprintf op "draw(\"$\\theta_i$\",arc((0,0),0.5,90,degrees(angle(i))),black,Arrow,PenMargins);\n")
         (fprintf op "draw(\"$\\theta_r$\",arc((0,0),0.5,90,degrees(angle(r))),red,Arrow,PenMargins);\n")
         (fprintf op "draw(\"$\\theta_t$\",arc((0,0),0.5,-90,degrees(angle(t))),blue,Arrow,PenMargins);\n")

         (close-port op)))))
 '(22.5 45.0 67.5))

(write-sources)
(exit)
