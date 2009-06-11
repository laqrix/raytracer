(define-record <image> width height x-origin y-origin pixels)

(define (make-image width height x-origin y-origin initializer)
  (let ([store (make-vector (fx* width height) #f)])
    (define (index i j)
      (unless (and (fx<= 0 i (- width 1)) (fx<= 0 j (- height 1)))
              (error #f "image index (~a, ~a) out of bounds (~a, ~a)"
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

(define (as-byte x)
  (exact (min (truncate (* (max 0 x) 255)) 255)))

(define (as-float x)
  (inexact (/ x 255)))

(define (write-tga image filename)
  (let* ([filename (string-append filename ".tga")]
         [op (open-output-file filename 'replace)])
    (define (write8 x)
      (write-char (integer->char x) op))
    (define (write16 x)
      (write8 (logand x #xFF))
      (write8 (logand (ash x -8) #xFF)))
    ;; Header
    (write8 0)                   ; ID Length
    (write8 0)                   ; Color Map Type
    (write8 2)                   ; Image Type: Uncompressed True-color
    (do ([i 0 (+ i 1)]) ((= i 5))       ; Color Map Specification
      (write8 0))
    (write16 (<image> x-origin image))
    (write16 (<image> y-origin image))
    (write16 (<image> width image))
    (write16 (<image> height image))
    (write8 24)                         ; Pixel Depth; bits per pixel
    (write8 0)                          ; Image Descriptor
    ;; Data
    (vector-for-each
     (lambda (pixel)
       (write8 (as-byte (<color> b pixel)))
       (write8 (as-byte (<color> g pixel)))
       (write8 (as-byte (<color> r pixel))))
     (<image> pixels image))
    (close-port op)))

(define (read-tga filename)
  (let ([filename (string-append filename ".tga")])
    (call-with-input-file filename
      (lambda (ip)
        (define (read8)
          (char->integer (read-char ip)))
        (define (read16)
          (let* ([low (read8)]
                 [high (read8)])
            (+ (ash high 8) low)))
        (define (read-rgb pixel-depth)
          (let* ([b (as-float (read8))]
                 [g (as-float (read8))]
                 [r (as-float (read8))])
            (when (= pixel-depth 32)    ; attribute byte
              (read8))
            (make-color r g b)))
        (define (assert x expect msg)
          (unless (equal? x expect)
            (error #f "~a: ~a but expected ~a" msg x expect)))
        ;; Header
        (assert (read8) 0 "ID Length")
        (assert (read8) 0 "Color Map Type")
        (let ([image-type (read8)])     ; 2 or 10
          (unless (or (= image-type 2) (= image-type 10))
            (error #f "Image Type: ~a but expected 2 or 10" image-type))
          (do ([i 0 (+ i 1)]) ((= i 5))
            (assert (read8) 0 "Color Map Specification"))
          (let* ([xo (read16)]
                 [yo (read16)]
                 [width (read16)]
                 [height (read16)])
            (let ([pixel-depth (read8)]) ; 24 or 32
              (unless (or (= pixel-depth 24) (= pixel-depth 32))
                (error #f "Pixel Depth: ~a but expected 24 or 32" pixel-depth))
              (let ([image-descriptor (read8)])
                (unless (or (= image-descriptor 0) (= image-descriptor 32))
                  (error #f "Image Descriptor: ~a but expected 0 or 32"
                    image-descriptor))
                ;; Data
                (cond
                 [(= image-type 2)      ; Uncompressed
                  (assert image-descriptor 0 "Image Descriptor")
                  (make-image width height xo yo
                    (lambda (set-pixel)
                      (do ([y 0 (+ y 1)]) ((= y height))
                        (do ([x 0 (+ x 1)]) ((= x width))
                          (set-pixel x y (read-rgb pixel-depth))))))]
                 [(= image-type 10)     ; Run length encoded
                  (<image> make
                    [width width] [height height]
                    [x-origin xo] [y-origin yo]
                    [pixels
                     (let* ([size (fx* width height)]
                            [store (make-vector size #f)])
                       (let lp ([i 0])
                         (if (= i size)
                             (cond
                              [(= image-descriptor 0) store]
                              [(= image-descriptor 32) ; flip vertically
                               (let ()
                                 (define (index i j)
                                   (fx+ (fx* j width) i))
                                 (do ([y 0 (+ y 1)]) ((= y (/ height 2)))
                                   (do ([x 0 (+ x 1)]) ((= x width))
                                     (let* ([i1 (index x y)]
                                            [i2 (index x (- height y 1))]
                                            [c1 (vector-ref store i1)]
                                            [c2 (vector-ref store i2)])
                                       (vector-set! store i1 c2)
                                       (vector-set! store i2 c1))))
                                 store)])
                             (lp (let* ([packet (read8)]
                                        [run-length? (fxlogbit? 7 packet)]
                                        [length (+ (fxlogbit0 7 packet) 1)])
                                   (if run-length?
                                       (let ([color (read-rgb pixel-depth)])
                                         (do ([j 0 (+ j 1)]
                                              [i i (+ i 1)])
                                             ((= j length) i)
                                           (vector-set! store i color)))
                                       (do ([j 0 (+ j 1)]
                                            [i i (+ i 1)])
                                           ((= j length) i)
                                         (vector-set! store i (read-rgb pixel-depth)))))))))])])))))))))
