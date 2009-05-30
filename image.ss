(define (as-byte x)
  (exact (min (truncate (* (max 0 x) 255)) 255)))

(define (write-pixels-to-ppm width height pixels filename)
  (let* ([filename (string-append filename ".ppm")]
         [op (open-output-file filename 'replace)])
    (printf "opened ~s\n" filename)
    (fprintf op "P3\n")
    (fprintf op "~a ~a\n" width height)
    (fprintf op "~a\n" 255)
    (for-each
     (lambda (pixel)
       (fprintf op "~a ~a ~a "
         (as-byte (<color> r pixel))
         (as-byte (<color> g pixel))
         (as-byte (<color> b pixel))))
     pixels)
    (close-port op)))

(define (write-pixels-to-tga width height pixels filename)
  (let* ([filename (string-append filename ".tga")]
         [op (open-output-file filename 'replace)])
    (define (write8 x)
      (write-char (integer->char x) op))
    (define (write16 x)
      (write8 (logand x #xFF))
      (write8 (logand (ash x -8) #xFF)))
    (printf "opened ~s\n" filename)
    ;; Header
    (write8 0)                   ; ID Length
    (write8 0)                   ; Color Map Type
    (write8 2)                   ; Image Type: Uncompressed True-color
    (do ([i 0 (+ i 1)]) ((= i 5))       ; Color Map Specification
      (write8 0))
    (write16 0)                         ; Image Spec: X-origin
    (write16 0)                         ; Image Spec: Y-origin
    (write16 width)
    (write16 height)
    (write8 24)                         ; Pixel Depth; bits per pixel
    (write8 0)                          ; Image Descriptor
    ;; Data
    (for-each
     (lambda (pixel)
       (write8 (as-byte (<color> b pixel)))
       (write8 (as-byte (<color> g pixel)))
       (write8 (as-byte (<color> r pixel))))
     pixels)
    (close-port op)))
