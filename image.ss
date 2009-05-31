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
