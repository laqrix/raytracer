(define (perlin-noise x y z)
  ;; Range [-0.5, 0.5]
  (define (p x)
    (define permutation
      '#vu8(151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225 140
             36 103 30 69 142 8 99 37 240 21 10 23 190 6 148 247 120 234 75 0 26
             197 62 94 252 219 203 117 35 11 32 57 177 33 88 237 149 56 87 174 20
             125 136 171 168 68 175 74 165 71 134 139 48 27 166 77 146 158 231 83
             111 229 122 60 211 133 230 220 105 92 41 55 46 245 40 244 102 143 54
             65 25 63 161 1 216 80 73 209 76 132 187 208 89 18 169 200 196 135 130
             116 188 159 86 164 100 109 198 173 186 3 64 52 217 226 250 124 123 5
             202 38 147 118 126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28
             42 223 183 170 213 119 248 152 2 44 154 163 70 221 153 101 155 167 43
             172 9 129 22 39 253 19 98 108 110 79 113 224 232 178 185 112 104 218
             246 97 228 251 34 242 193 238 210 144 12 191 179 162 241 81 51 145 235
             249 14 239 107 49 192 214 31 181 199 106 157 184 84 204 176 115 121 50
             45 127 4 150 254 138 236 205 93 222 114 67 29 24 72 243 141 128 195 78
             66 215 61 156 180))
    (define size (bytevector-length permutation))
    (bytevector-u8-ref permutation (modulo x size)))

  (define (grad hash x y z)
    (let ([h (fxlogand hash 15)])
      (let ([u (if (< h 8) x y)]
            [v (if (< h 4) y (if (or (= h 12) (= h 14)) x z))])
        (+ (if (fxlogbit? 0 h) (- u) u)
           (if (fxlogbit? 1 h) (- v) v)))))

  (define (lerp t a b)
    (+ a (* t (- b a))))

  (define (fade t)
    (* t t t (+ (* t (- (* t 6) 15)) 10)))

  (define (as-int n) (exact (floor n)))

  (let ([X (fxlogand (as-int x) 255)]
        [Y (fxlogand (as-int y) 255)]
        [Z (fxlogand (as-int z) 255)]
        [x (- x (as-int x))]
        [y (- y (as-int y))]
        [z (- z (as-int z))])
    (let ([u (fade x)]
          [v (fade y)]
          [w (fade z)])
      (let* ([A (+ (p X) Y)]
             [AA (+ (p A) Z)]
             [AB (+ (p (+ A 1)) Z)]
             [B (+ (p (+ X 1)) Y)]
             [BA (+ (p B) Z)]
             [BB (+ (p (+ B 1)) Z)])
        (lerp w
          (lerp v
            (lerp u (grad (p AA) x y z)
              (grad (p BA) (- x 1) y z))
            (lerp u (grad (p AB) x (- y 1) z)
              (grad (p BB) (- x 1) (- y 1) z)))
          (lerp v
            (lerp u (grad (p (+ AA 1)) x y (- z 1))
              (grad (p (+ BA 1)) (- x 1) y (- z 1)))
            (lerp u (grad (p (+ AB 1)) x (- y 1) (- z 1))
              (grad (p (+ BB 1)) (- x 1) (- y 1) (- z 1)))))))))

(define (noise x)
  ;; Range [0, 1]
  (+ 0.5
     (if (vec? x)
         (perlin-noise (vec-i x) (vec-j x) (vec-k x))
         (perlin-noise x 0 0))))

(define (snoise x)
  ;; Range [-1, 1]
  (- (* 2 (noise x)) 1))

(define (filteredsnoise x w)
  (fadeout (snoise x) 0 1 w))

(define (vsnoise x)
  (let ([n (snoise x)])
    (make-vec n n n)))

;; Fractional Brownian Motion and Turbulance

(define (fBm p octaves lacunarity gain)
  (let lp ([i 0] [sum 0] [amp 1] [pp p])
    (if (= i octaves)
        sum
        (lp (+ i 1)
          (+ sum (* amp (snoise pp)))
          (* amp gain)
          (vec-num-mul pp lacunarity)))))

(define (vfBm p octaves lacunarity gain)
  (let lp ([i 0] [sum (make-vec 0 0 0)] [amp 1] [pp p])
    (if (= i octaves)
        sum
        (lp (+ i 1)
          (vec-add sum (vec-num-mul (vsnoise pp) amp))
          (* amp gain)
          (vec-num-mul pp lacunarity)))))

(define (turbulence p octaves lacunarity gain)
  (let lp ([i 0] [sum 0] [amp 1] [pp p])
    (if (= i octaves)
        sum
        (lp (+ i 1)
          (+ sum (* amp (abs (snoise pp))))
          (* amp gain)
          (vec-num-mul pp lacunarity)))))

;; Cellular and Voronoi Noise

(define (cellnoise x)
  (define (center x) (+ (floor x) 0.5))
  (+ 0.5
     (if (vec? x)
         (perlin-noise
          (center (vec-i x)) (center (vec-j x)) (center (vec-k x)))
         (perlin-noise (center x) 0 0))))

(define (vec-cellnoise x)
  (let ([n (cellnoise x)])
    (make-vec n n n)))

(define (distance a b)
  (let ([v (vec-sub a b)])
    (vec-length v)))

(define (distance2 a b)
  (let ([v (vec-sub a b)])
    (vec-dot v v)))

(define (distance-manhattan a b)
  (let ([v (vec-sub a b)])
    (+ (abs (vec-i v)) (abs (vec-j v)) (abs (vec-k v)))))

(define (distance-chebyshev a b)
  (let ([v (vec-sub a b)])
    (max (abs (vec-i v)) (abs (vec-j v)) (abs (vec-k v)))))

(define (distance-quadratic a b)
  (let ([v (vec-sub a b)])
    (let ([x (vec-i v)] [y (vec-j v)] [z (vec-k v)])
      (+ (* x x) (* y y) (* z z) (* x y) (* x z) (* y z)))))

(define (make-distance-minkowski order)
  (lambda (a b)
    (let ([v (vec-sub a b)])
      (expt
       (+ (expt (abs (vec-i v)) order)
          (expt (abs (vec-j v)) order)
          (expt (abs (vec-k v)) order))
       (/ 1 order)))))

(define (voronoi* p jitter distance)
  (let* ([thiscell (make-vec (+ (floor (vec-i p)) 0.5)
                     (+ (floor (vec-j p)) 0.5)
                     (+ (floor (vec-k p)) 0.5))])
    (sort! fl<
      (list-of
       (let* ([testcell (vec-add thiscell (make-vec i j k))]
              [pos (vec-add testcell
                     (vec-num-mul
                      (vec-num-add (vec-cellnoise testcell) -0.5)
                      jitter))])
         (distance pos p))
       ([i '(-1 0 1)]
        [j '(-1 0 1)]
        [k '(-1 0 1)])))))

(define (voronoi p jitter feature distance)
  ;; feature=[0, 26], but only [0, 3] are practical
  (let ([ls (voronoi* p jitter distance)])
    (list-ref ls feature)))

(define (voronoi-diff p jitter f1 f2 distance)
  ;; f1,f2=[0, 26], but only [0, 3] are practical
  (let ([ls (voronoi* p jitter distance)])
    (- (list-ref ls f1) (list-ref ls f2))))
