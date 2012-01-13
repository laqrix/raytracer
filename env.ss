(define-syntax define-user-var
  (syntax-rules ()
    [(_ param var)
     (begin
       (define param (make-parameter #f))
       (define-syntax var
         (identifier-syntax
          [id (param)]
          [(set! id exp) (param exp)])))]))

(define-syntax define-lazy-user-var
  (syntax-rules ()
    [(_ param var)
     (begin
       (define param (make-parameter (delay #f)))
       (define-syntax var
         (identifier-syntax
          [id (force (param))]
          [(set! id exp) (param exp)])))]))

(define $scene (make-parameter #f))
(define $camera (make-parameter #f))
(define $display (make-parameter #f))
(define $depth (make-parameter #f))
(define $object (make-parameter #f))
(define $light (make-parameter #f))

(define-syntax Cs (identifier-syntax (object-color ($object))))
(define-syntax Os (identifier-syntax (object-opacity ($object))))

(define-user-var $E E)
(define-user-var $I I)
(define-user-var $L L)
(define-user-var $N N)
(define-user-var $Ng Ng)
(define-user-var $P P)

(define-user-var $Ci Ci)
(define-user-var $Oi Oi)

(define-lazy-user-var $s s)
(define-lazy-user-var $t t)
