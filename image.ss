(define-record <image> width height x-origin y-origin pixels)

(define (make-image width height x-origin y-origin initializer)
  (let ([store (make-vector (fx* width height) #f)])
    (define (index i j)
      (unless (and (fx<= 0 i (- width 1)) (fx<= 0 j (- height 1)))
              (errorf #f "image index (~a, ~a) out of bounds (~a, ~a)"
                     i j (- width 1) (- height 1)))
      (fx+ (fx* j width) i))
    (initializer
     (case-lambda
      [(i j) (vector-ref store (index i j))]
      [(i j x) (vector-set! store (index i j) x)]))
    (<image> make
             [width width]
             [height height]
             [x-origin x-origin]
             [y-origin y-origin]
             [pixels store])))

(define (image-index img i j)
  (let ([width (<image> width img)]
        [height (<image> height img)])
    (unless (and (fx<= 0 i (- width 1)) (fx<= 0 j (- height 1)))
      (errorf #f "image index (~a, ~a) out of bounds (~a, ~a)"
        i j (- width 1) (- height 1)))
    (fx+ (fx* j width) i)))

(define (image-ref img i j)
  (vector-ref (<image> pixels img) (image-index img i j)))

(define (image-normalize image)
  (let* ([pixels (<image> pixels image)]
         [mag (do ([i 0 (+ i 1)] [mag 0 (max mag (vector-ref pixels i))])
                  ((= i (vector-length pixels)) mag))])
    (if (= mag 0)
        image
        (<image> copy image
          [pixels (vector-map (lambda (x) (/ x mag)) pixels)]))))

(define (as-byte x)
  (exact (min (truncate (* (max 0 x) 255)) 255)))

(define (write-tga image filename)
  (let* ([filename (string-append filename ".tga")]
         [op (open-file-output-port filename (file-options no-fail))]
         [color? (<color> is? (image-ref image 0 0))]
         [image-type (if color? 2 3)]
         [pixel-depth (if color? 24 8)])
    (define (write8 x)
      (put-u8 op x))
    (define (write16 x)
      (write8 (logand x #xFF))
      (write8 (logand (ash x -8) #xFF)))
    ;; Header
    (write8 0)                          ; ID Length
    (write8 0)                          ; Color Map Type
    (write8 image-type)                 ; Image Type
    (do ([i 0 (+ i 1)]) ((= i 5))       ; Color Map Specification
      (write8 0))
    (write16 (<image> x-origin image))
    (write16 (<image> y-origin image))
    (write16 (<image> width image))
    (write16 (<image> height image))
    (write8 pixel-depth)          ; Pixel Depth; bits per pixel
    (write8 32)                   ; Image Descriptor (top-left origin)
    ;; Data
    (if color?
        (vector-for-each
         (lambda (pixel)
           (write8 (as-byte (<color> b pixel)))
           (write8 (as-byte (<color> g pixel)))
           (write8 (as-byte (<color> r pixel))))
         (<image> pixels image))
        (vector-for-each
         (lambda (pixel) (write8 (as-byte pixel)))
         (<image> pixels image)))
    (close-port op)))

(define (read-texture-file filename)
  ;; Colors are stored in the range [0, 255] but are represented [0, 1]
  (define t (make-linear-transform 0 255 0.0 1.0))
  (read-tga filename
    (lambda (r g b a)
      (make-color (t r) (t g) (t b)))))

(define (read-normals-file filename)
  ;; Normals are stored in the range [0, 255] but represent [-1, 1]
  (define t (make-linear-transform 0 255 -1.0 1.0))
  (read-tga filename
    (lambda (r g b a)
      (make-vec (t r) (t g) (t b)))))

(define (read-depth-file filename)
  ;; Depths are stored in the range [0, 255] but represent [0, 1]
  (read-tga filename (make-linear-transform 0 255 0.0 1.0)))

(define (read-tga filename proc)
  (define (read-header ip)
    (define size 18)
    (let ([x (get-bytevector-n ip size)])
      (unless (= (bytevector-length x) size)
        (errorf 'read-header "file does not contain TGA header"))
      x))
  (define (get8 x n)
    (bytevector-u8-ref x n))
  (define (get16 x n)
    (bytevector-u16-ref x n 'little))
  (let* ([filename (string-append filename ".tga")]
         [ip (open-file-input-port filename)])
    (define (read8)
      (get-u8 ip))
    (define (read-pixel pixel-depth)
      (cond
       [(= pixel-depth 8)
        (proc (read8))]
       [(>= pixel-depth 24)
        (let* ([b (read8)]
               [g (read8)]
               [r (read8)])
          (proc r g b (and (= pixel-depth 32) (read8))))]
       [else
        (errorf 'read-pixel "invalid pixel depth ~a" pixel-depth)]))
    (let* ([hdr (read-header ip)]
           [image-type (get8 hdr 2)]
           [xo (get16 hdr 8)]
           [yo (get16 hdr 10)]
           [width (get16 hdr 12)]
           [height (get16 hdr 14)]
           [pixel-depth (get8 hdr 16)]
           [image-descriptor (get8 hdr 17)]
           [color-type (fxlogand image-type 3)]
           [rle? (fxlogbit? 3 image-type)]
           [xt (if (fxlogbit? 4 image-descriptor)
                   (lambda (x) (- (- width 1) x))
                   (lambda (x) x))]
           [yt (if (fxlogbit? 5 image-descriptor)
                   (lambda (y) y)
                   (lambda (y) (- (- height 1) y)))])
      (unless (<= 0 image-type 11)
        (errorf #f "Image Type: unhandled type ~a" image-type))
      (unless (or (= color-type 2) (= color-type 3))
        (errorf #f "Color Type: unhandled type ~a" color-type))
      (make-image width height xo yo
        (lambda (set-pixel)
          (if (not rle?)
              (do ([y 0 (+ y 1)]) ((= y height))
                (do ([x 0 (+ x 1)]) ((= x width))
                  (set-pixel (xt x) (yt y) (read-pixel pixel-depth))))
              (do ([y 0 (+ y 1)]) ((= y height))
                (let lp ([x 0])
                  (unless (= x width)
                    (lp (let* ([packet (read8)]
                               [run-length? (fxlogbit? 7 packet)]
                               [length (+ (fxlogbit0 7 packet) 1)])
                          (if run-length?
                              (let ([color (read-pixel pixel-depth)])
                                (do ([j 0 (+ j 1)]
                                     [x x (+ x 1)])
                                    ((= j length) x)
                                  (set-pixel (xt x) (yt y) color)))
                              (do ([j 0 (+ j 1)]
                                   [x x (+ x 1)])
                                  ((= j length) x)
                                (set-pixel (xt x) (yt y)
                                  (read-pixel pixel-depth)))))))))))))))
