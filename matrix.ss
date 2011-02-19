(define (make-matrix m n initializer)
  (let ([store (make-vector (fx* m n))])
    (define (index i j)
      (unless (and (fx<= 1 i m) (fx<= 1 j n))
        (errorf #f "matrix index (~a, ~a) out of bounds (~a, ~a)" i j m n))
      (fx+ (fx* (fx- i 1) n) j -1))
    (initializer m n
     (case-lambda
      [(i j) (vector-ref store (index i j))]
      [(i j x) (vector-set! store (index i j) x)]))
    (case-lambda
     [() (values m n)]
     [(i j) (vector-ref store (index i j))])))

(define (matrix-dimensions a) (a))

(define (matrix->list a)
  (let-values ([(m n) (matrix-dimensions a)])
    (let f1 ([i m] [rows '()])
      (if (fx> i 0)
          (let f2 ([j n] [row '()])
            (if (fx> j 0)
                (f2 (fx- j 1) (cons (a i j) row))
                (f1 (fx- i 1) (cons row rows))))
          rows))))

(define (list->matrix ls)
  (make-matrix (length ls) (length (car ls))
    (lambda (m n a)
      (do ([i 1 (fx+ i 1)] [rows ls (cdr rows)]) ((fx> i m))
        (do ([j 1 (fx+ j 1)] [cols (car rows) (cdr cols)]) ((fx> j n))
          (a i j (car cols)))))))

(define (matrix-identity n)
  (make-matrix n n
    (lambda (m n a)
      (do ([i 1 (fx+ i 1)]) ((fx> i m))
        (a i i 1)))))

(define matrix-add
  (case-lambda
   [(a1) a1]
   [(a1 a2)
    (let-values ([(m1 n1) (matrix-dimensions a1)]
                 [(m2 n2) (matrix-dimensions a2)])
      (unless (and (fx= m1 m2) (fx= n1 n2))
        (errorf 'matrix-add "mismatched sizes"))
      (make-matrix m1 n1
        (lambda (m n a)
          (do ([i 1 (fx+ i 1)]) ((fx> i m))
            (do ([j 1 (fx+ j 1)]) ((fx> j n))
              (a i j (+ (a1 i j) (a2 i j))))))))]
   [(a1 a2 . rest) (apply matrix-add (matrix-add a1 a2) rest)]))

(define matrix-sub
  (case-lambda
   [(a1 a2)
    (let-values ([(m1 n1) (matrix-dimensions a1)]
                 [(m2 n2) (matrix-dimensions a2)])
      (unless (and (fx= m1 m2) (fx= n1 n2))
        (errorf 'matrix-sub "mismatched sizes"))
      (make-matrix m1 n1
        (lambda (m n a)
          (do ([i 1 (fx+ i 1)]) ((fx> i m))
            (do ([j 1 (fx+ j 1)]) ((fx> j n))
              (a i j (- (a1 i j) (a2 i j))))))))]
   [(a1 a2 . rest) (apply matrix-sub (matrix-sub a1 a2) rest)]))

(define matrix-mul
  (case-lambda
   [(a1) a1]
   [(a1 a2)
    (let-values ([(m1 n1) (matrix-dimensions a1)]
                 [(m2 n2) (matrix-dimensions a2)])
      (unless (fx= n1 m2)
        (errorf 'matrix-mul "mismatched sizes"))
      (make-matrix m1 n2
        (lambda (m n a)
          (do ([i 1 (fx+ i 1)]) ((fx> i m))
            (do ([j 1 (fx+ j 1)]) ((fx> j n))
              (let dot ([k 1] [sum 0])
                (if (fx> k n1)
                    (a i j sum)
                    (dot (fx+ k 1) (+ (* (a1 i k) (a2 k j)) sum)))))))))]
   [(a1 a2 . rest) (apply matrix-mul (matrix-mul a1 a2) rest)]))

(define (matrix-transpose a)
  (let-values ([(m n) (matrix-dimensions a)])
    (make-matrix n m
      (lambda (n m t)
        (do ([i 1 (fx+ i 1)]) ((fx> i m))
          (do ([j 1 (fx+ j 1)]) ((fx> j n))
            (t j i (a i j))))))))

(define (ludcmp a0)
  (let-values ([(m n) (matrix-dimensions a0)])
    (unless (fx= m n)
      (errorf 'ludcmp "matrix must be square"))
    (let* ([indx (make-vector n)]
           [d 1]
           [a 
            (make-matrix m n
              (lambda (m n a)
                (let ([vv (make-vector n)])
                  (define (set-vv! i x) (vector-set! vv (fx- i 1) x))
                  (define (vv-ref i) (vector-ref vv (fx- i 1)))
                  ;; Loop over rows to get the implicit scaling information,
                  ;; copying a0 into a along the way
                  (do ([i 1 (fx+ i 1)]) ((fx> i n))
                    (let lp ([j 1] [big 0])
                      (cond
                       [(fx<= j n)
                        (let ([x (a0 i j)])
                          (a i j x)
                          (lp (fx+ j 1) (max (abs x) big)))]
                       [(zero? big) (errorf 'ludcmp "singular matrix")]
                       [else (set-vv! i (/ big))])))
                  ;; Loop over columns of Crout's method
                  (do ([j 1 (fx+ j 1)]) ((fx> j n))
                    (do ([i 1 (fx+ i 1)]) ((fx>= i j))
                      (let lp ([k 1] [sum (a i j)])
                        (if (fx< k i)
                            (lp (fx+ k 1) (- sum (* (a i k) (a k j))))
                            (a i j sum))))
                    (let lp1 ([i j] [big 0] [imax j])
                      (cond
                       [(fx<= i n)
                        (let lp2 ([k 1] [sum (a i j)])
                          (cond
                           [(fx< k j)
                            (lp2 (fx+ k 1) (- sum (* (a i k) (a k j))))]
                           [else
                            (a i j sum)
                            (let ([dum (* (vv-ref i) (abs sum))])
                              (if (>= dum big)
                                  (lp1 (fx+ i 1) dum i)
                                  (lp1 (fx+ i 1) big imax)))]))]
                       [else
                        (unless (fx= j imax)
                          (do ([k 1 (fx+ k 1)]) ((fx> k n))
                            (let ([dum (a imax k)])
                              (a imax k (a j k))
                              (a j k dum)))
                          (set! d (- d))
                          (set-vv! imax (vv-ref j)))
                        (vector-set! indx (fx- j 1) imax)
                        (when (zero? (a j j))
                          (errorf 'ludcmp "singular matrix"))
                        (unless (fx= j n)
                          (let ([pivot (/ (a j j))])
                            (do ([i (fx+ j 1) (fx+ i 1)]) ((fx> i n))
                              (a i j (* (a i j) pivot)))))]))))))])
      (values a indx d))))

(define (lubksb a indx b)
  (let-values ([(m n) (matrix-dimensions a)])
    (unless (fx= m n)
      (errorf 'lubksb "matrix must be square"))
    (let lp ([i 1] [ii #f])
      (when (fx<= i n)
        (let* ([ip (vector-ref indx (fx- i 1))]
               [sum (vector-ref b (fx- ip 1))])
          (vector-set! b (fx- ip 1) (vector-ref b (fx- i 1)))
          (if ii
              (let lp2 ([j ii] [sum sum])
                (if (fx<= j (fx- i 1))
                    (lp2 (fx+ j 1)
                      (- sum (* (a i j) (vector-ref b (fx- j 1)))))
                    (begin
                      (vector-set! b (fx- i 1) sum)
                      (lp (fx+ i 1) ii))))
              (begin
                (vector-set! b (fx- i 1) sum)
                (lp (fx+ i 1) (if (zero? sum) ii i)))))))
    (do ([i n (fx- i 1)]) ((fx= i 0))
      (let lp ([j (fx+ i 1)] [sum (vector-ref b (fx- i 1))])
        (if (fx<= j n)
            (lp (fx+ j 1) (- sum (* (a i j) (vector-ref b (fx- j 1)))))
            (vector-set! b (fx- i 1) (/ sum (a i i)))))))
  b)

(define (matrix-inverse a)
  (let-values ([(m n) (matrix-dimensions a)]
               [(a indx d) (ludcmp a)])
    (make-matrix m n
      (lambda (m n y)
        (do ([j 1 (fx+ j 1)]) ((fx> j n))
          (let ([col (make-vector n 0)])
            (vector-set! col (fx- j 1) 1)
            (lubksb a indx col)
            (do ([i 1 (fx+ i 1)]) ((fx> i n))
              (y i j (vector-ref col (fx- i 1))))))))))


#!eof

(matrix->list (matrix-inverse (make-matrix 3 3 (lambda(m n a) (a 1 1 1) (a 1 2 3) (a 1 3 1) (a 2 1 1) (a 2 2 1) (a 2 3 2) (a 3 1 2) (a 3 2 3) (a 3 3 4)))))

;results should be:
((2 9 -5) (0 -2 1) (-1 -3 2))

;another way to do it:
(matrix->list (matrix-inverse (list->matrix '((1 3 1) (1 1 2) (2 3 4)))))

(matrix->list (matrix-mul (list->matrix '((1 2 3))) (list->matrix '((2 3) (3 4) (4 5)))))
;result should be:
((20 26))
