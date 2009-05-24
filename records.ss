(define (find-source x)
  (call/cc
   (lambda (return)
     (parameterize
      ([#%$source-error-handler
        (lambda (who src start? msg . args)
          (let* ([source-rtd (record-type-descriptor src)]
                 [source-sfd (record-field-accessor source-rtd 'sfd)]
                 [source-bfp (record-field-accessor source-rtd 'bfp)]
                 [source-efp (record-field-accessor source-rtd 'efp)]
                 [sfd (source-sfd src)]
                 [fp (if start? (source-bfp src) (source-efp src))])
            (call-with-values (lambda () (#%$locate-source sfd fp))
              (case-lambda
               [() (return "")]
               [(path line char)
                (return (format " at line ~d of ~a" line path))]))))]
       [error-handler (lambda (who what . args) (return ""))])
      (syntax-error x "no source information for")))))

(define (identifier-symbol-eq? x y)
  (eq? (syntax->datum x) (syntax->datum y)))

(module ((match $match match-help match-record match-vector))
  (define-syntax (match x)
    (syntax-case x ()
      [(_ exp (pattern b1 b2 ...) ...)
       (with-syntax ([src (datum->syntax #'_ (find-source x))])
         #'(let ([x exp])
             ((or ($match x pattern b1 b2 ...) ...
                  (error 'match "no matching clause for ~s~a" x src)))))]))
  
  (define-syntax $match
    (syntax-rules (guard)
      [(_ e pat (guard g) b1 b2 ...)
       (match-help e pat (and g (lambda () b1 b2 ...)))]
      [(_ e pat b1 b2 ...)
       (match-help e pat (lambda () b1 b2 ...))]))

  (define-syntax (match-help x)
    (define (bad-pattern p)
      (syntax-error p "invalid match pattern"))
    (syntax-case x (unquote _ unquote-splicing quasiquote)
      [(match-help e (unquote _) body) #'body]
      [(match-help e (unquote v) body)
       (if (identifier? #'v)
           #'(let ([v e]) body)
           (bad-pattern #'(unquote v)))]
      [(match-help e (unquote-splicing v) body)
       (if (identifier? #'v)
           #'(and (equal? e v) body)
           (bad-pattern #'(unquote-splicing v)))]
      [(match-help e (quasiquote (record [field pattern] ...)) body)
       (for-all identifier? #'(record field ...))
       #'(let ([v e])
           (and (record is? v)
                (match-record v (record [field pattern] ...) body)))]
      [(match-help e (quasiquote q) body)
       (bad-pattern #'(quasiquote q))]
      [(match-help e s body)
       (identifier? #'s)
       #'(and (eq? e 's) body)]
      [(match-help e x body)
       (let ([x (syntax->datum #'x)])
         (or (number? x) (boolean? x) (char? x)))
       #'(and (eqv? e 'x) body)]
      [(match-help e s body)
       (string? (syntax->datum #'s))
       #'(and (let ([y e]) (and (string? y) (#3%string=? y s))) body)]
      [(match-help e () body)
       #'(and (null? e) body)]
      [(match-help e (x . y) body)
       #'(let ([v e])
           (and (pair? v)
                (match-help (#3%car v) x
                  (match-help (#3%cdr v) y body))))]
      [(match-help e #(x ...) body)
       #'(let ([v e])
           (and (vector? v)
                (#3%fx= (#3%vector-length v) (length '(x ...)))
                (match-vector v 0 (x ...) body)))]
      [(match-help e p body) (bad-pattern #'p)]))

  (define-syntax match-record
    (syntax-rules ()
      [(match-record v (record) body) body]
      [(match-record v (record [field pattern] . rest) body)
       (match-help (record no-check field v) pattern
         (match-record v (record . rest) body))]))

  (define-syntax match-vector
    (syntax-rules ()
      [(match-vector v i () body) body]
      [(match-vector v i (pattern . rest) body)
       (match-help (#3%vector-ref v i) pattern
         (match-vector v (+ i 1) rest body))]))
  )

(define-syntax (define-record x)
  (syntax-case x ()
    [(_ name field ...)
     (and (identifier? #'name)
          (let valid-fields? ([fields #'(field ...)] [seen '()])
            (syntax-case fields ()
              [(fn . rest)
               (and (identifier? #'fn)
                    (let ([f (syntax->datum #'fn)])
                      (when (memq f seen)
                        (syntax-error x (format "duplicate field ~a in" f)))
                      (valid-fields? #'rest (cons f seen))))]
              [() #t]
              [_ #f])))
     #'(define-syntax (name x)
         (define (valid-bindings? bindings seen)
           (syntax-case bindings ()
             [((fn fv) . rest)
              (and (identifier? #'fn)
                   (let ([f (syntax->datum #'fn)])
                     (when (memq f seen)
                       (syntax-error x (format "duplicate field ~a in" f)))
                     (unless (memq f '(field ...))
                       (syntax-error x (format "unknown field ~a in" f)))
                     (valid-bindings? #'rest (cons f seen))))]
             [() #t]
             [_ #f]))
         (define (make-record fields bindings)
           (if (snull? fields)
               '()
               (let* ([f (scar fields)]
                      [v (find-binding f bindings)])
                 (unless v
                   (syntax-error x
                     (format "missing field ~a in" (syntax->datum f))))
                 (cons v
                   (make-record (scdr fields) (remove-binding f bindings))))))
         (define (copy-record fields index bindings)
           (if (snull? fields)
               '()
               (let* ([f (scar fields)]
                      [v (find-binding f bindings)])
                 (if v
                     (cons v (copy-record (scdr fields) (+ index 1)
                               (remove-binding f bindings)))
                     (cons #`(#3%vector-ref src #,(datum->syntax f index))
                       (copy-record (scdr fields) (+ index 1) bindings))))))
         (define (find-binding f bindings)
           (syntax-case bindings ()
             [((fn fv) . rest)
              (if (identifier-symbol-eq? #'fn f)
                  #'fv
                  (find-binding f #'rest))]
             [() #f]))
         (define (remove-binding f bindings)
           (syntax-case bindings ()
             [((fn fv) . rest)
              (if (identifier-symbol-eq? #'fn f)
                  #'rest
                  #`((fn fv) #,@(remove-binding f #'rest)))]))
         (define (find-index fn fields index)
           (let ([f (scar fields)])
             (if (identifier-symbol-eq? f fn)
                 index
                 (find-index fn (scdr fields) (+ index 1)))))
         (define (snull? x) (syntax-case x () [() #t] [_ #f]))
         (define (scar x) (syntax-case x () [(x . _) #'x]))
         (define (scdr x) (syntax-case x () [(_ . y) #'y]))
         (syntax-case x ()
           [(name make . bindings)
            (and (eq? (syntax->datum #'make) 'make)
                 (valid-bindings? #'bindings '()))
            #`(vector 'name #,@(make-record #'(field ...) #'bindings))]
           [(name copy e . bindings)
            (and (eq? (syntax->datum #'copy) 'copy)
                 (valid-bindings? #'bindings '()))
            #`(let ([src e])
                (unless (name is? src)
                  (error 'record-copy "~s is not a ~a~a" src 'name
                    #,(find-source x)))
                (vector 'name #,@(copy-record #'(field ...) 1 #'bindings)))]
           [(name is? e)
            (eq? (syntax->datum #'is?) 'is?)
            #'(let ([x e])
                (and (vector? x)
                     (#3%fx= (#3%vector-length x) (length '(name field ...)))
                     (eq? (#3%vector-ref x 0) 'name)))]
           [(name fn e)
            (identifier-symbol-eq? #'fn #'field)
            #`(let ([x e])
                (unless (name is? x)
                  (error 'record-ref "~s is not a ~a~a" x 'name
                    #,(find-source x)))
                (#3%vector-ref x #,(find-index #'fn #'(field ...) 1)))]
           ...
           [(name no-check fn e)
            (and (eq? (syntax->datum #'no-check) 'no-check)
                 (identifier-symbol-eq? #'fn #'field))
            #`(#3%vector-ref e #,(find-index #'fn #'(field ...) 1))]
           ...))]))
