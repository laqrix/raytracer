(define source-rtd
  (let-values ([(src start?) (#%$syntax->src #'*)])
    (record-rtd src)))
(define source-sfd (csv7:record-field-accessor source-rtd 'sfd))
(define source-bfp (csv7:record-field-accessor source-rtd 'bfp))

(define source-file-descriptor-rtd
  (let-values ([(src start?) (#%$syntax->src #'*)])
    (record-rtd (source-sfd src))))
(define source-file-descriptor-name
  (csv7:record-field-accessor source-file-descriptor-rtd 'name))

(define (find-source x)
  (let-values ([(src start?) (#%$syntax->src x)])
    (if src
        (datum->syntax #'quote
          (format " ~a offset ~a of ~a"
            (if (eq? start? 'near) 'near 'at)
            (if start? (source-bfp src) (source-efp src))
            (source-file-descriptor-name (source-sfd src))))
        #'"")))

(define (snull? x) (syntax-case x () [() #t] [_ #f]))
(define (scar x) (syntax-case x () [(x . _) #'x]))
(define (scdr x) (syntax-case x () [(_ . y) #'y]))

(define (syntax-datum-eq? x y)
  (eq? (syntax->datum x) (syntax->datum y)))

(define (bad-syntax msg form subform)
  (raise
   (condition
    (make-message-condition msg)
    (make-syntax-violation form subform))))

(module ((match
          bad-match
          fail-false
          fail-match
          match-help
          match-one
          match-pattern
          match-record
          match-vector
          )
         match-let*)
  (define-syntax (match x)
    (syntax-case x ()
      [(_ exp (pattern b1 b2 ...) ...)
       #`(let ([v exp])
           ((or (match-pattern v pattern b1 b2 ...) ...
                (bad-match v #,(find-source x)))))]))

  (define-syntax match-pattern
    (syntax-rules ()
      [(_ e pat (guard g) b1 b2 ...)
       (eq? (datum guard) 'guard)
       (match-one e pat fail-false (and g (lambda () b1 b2 ...)))]
      [(_ e pat b1 b2 ...)
       (match-one e pat fail-false (lambda () b1 b2 ...))]))

  (define-syntax (match-let* x)
    (syntax-case x ()
      [(_ () b1 b2 ...)
       #'(let () b1 b2 ...)]
      [(_ ([pattern exp] . rest) b1 b2 ...)
       #'(match-one exp pattern fail-match (match-let* rest b1 b2 ...))]
      [(_ ([pattern (guard g) exp] . rest) b1 b2 ...)
       (eq? (datum guard) 'guard)
       #`(let ([v exp])
           (match-one v pattern fail-match
             (if g
                 (match-let* rest b1 b2 ...)
                 (bad-match v #,(find-source #'g)))))]))

  (define-syntax fail-false
    (syntax-rules ()
      [(_ v pattern) #f]))

  (define-syntax (fail-match x)
    (syntax-case x ()
      [(_ v pattern)
       #`(bad-match v #,(find-source #'pattern))]))

  (define (bad-match v src)
    (errorf #f "no matching clause for ~s~a" v src))

  (define-syntax (match-one x)
    (define (bad-pattern x)
      (syntax-error x "invalid match pattern"))
    (define (add-identifier id ids)
      (if (duplicate-id? id ids)
          (syntax-error id "duplicate pattern variable")
          (cons id ids)))
    (define (duplicate-id? id ids)
      (and (not (null? ids))
           (or (bound-identifier=? (car ids) id)
               (duplicate-id? id (cdr ids)))))
    (syntax-case x ()
      [(_ e pattern fail body)
       (lambda (lookup)
         (let check ([ids '()] [x #'pattern])
           (syntax-case x (unquote unquote-splicing quasiquote)
             [(unquote (v <= pattern))
              (and (identifier? #'v) (eq? (datum <=) '<=))
              (check (add-identifier #'v ids) #'pattern)]
             [(unquote v)
              (let ([s (datum v)])
                (cond
                 [(eq? s '_) ids]
                 [(symbol? s) (add-identifier #'v ids)]
                 [else (bad-pattern x)]))]
             [(unquote-splicing var)
              (if (identifier? #'var)
                  ids
                  (bad-pattern x))]
             [(quasiquote (record spec ...))
              (identifier? #'record)
              (let ([fields (lookup #'record #'fields)])
                (unless fields
                  (syntax-error x "unknown record type in pattern"))
                (let check-specs ([ids ids] [specs #'(spec ...)])
                  (syntax-case specs (unquote)
                    [() ids]
                    [((unquote field) . rest)
                     (identifier? #'field)
                     (if (memq (datum field) fields)
                         (check-specs (add-identifier #'field ids) #'rest)
                         (syntax-error x
                           (format "unknown field ~a in pattern"
                             (datum field))))]
                    [([field pattern] . rest)
                     (identifier? #'field)
                     (if (memq (datum field) fields)
                         (check-specs (check ids #'pattern) #'rest)
                         (syntax-error x
                           (format "unknown field ~a in pattern"
                             (datum field))))]
                    [_ (bad-pattern x)])))]
             [(quasiquote _) (bad-pattern x)]
             [lit
              (let ([x (datum lit)])
                (or (symbol? x) (number? x) (boolean? x) (char? x)
                    (string? x) (bytevector? x) (null? x)))
              ids]
             [(first . rest) (check (check ids #'first) #'rest)]
             [#(element ...) (fold-left check ids #'(element ...))]
             [_ (bad-pattern x)]))
         #'(match-help e pattern fail body))]))

  (define-syntax (match-help x)
    (define (get-pattern x)
      (syntax-case x ()
        [(_ e pattern fail body) #'pattern]))
    (syntax-case x (unquote unquote-splicing quasiquote)
      [(_ e (unquote (v <= pattern)) fail body)
       #'(let ([v e]) (match-help v pattern fail body))]
      [(_ e (unquote v) fail body)
       (if (eq? (datum v) '_)
           #'(begin e body)
           #'(let ([v e]) body))]
      [(_ e (unquote-splicing var) fail body)
       #`(let ([v e])
           (if (equal? v var)
               body
               (fail v #,(get-pattern x))))]
      [(_ e (quasiquote (record spec ...)) fail body)
       #`(let ([v e])
           (if (record is? v)
               (match-record v (record spec ...) fail body)
               (fail v #,(get-pattern x))))]
      [(_ e lit fail body)
       (let ([x (datum lit)])
         (or (symbol? x) (number? x) (boolean? x) (char? x)))
       #`(let ([v e])
           (if (eqv? v 'lit)
               body
               (fail v #,(get-pattern x))))]
      [(_ e s fail body)
       (string? (datum s))
       #`(let ([v e])
           (if (and (string? v) (#3%string=? v s))
               body
               (fail v #,(get-pattern x))))]
      [(_ e bv fail body)
       (bytevector? (datum bv))
       #`(let ([v e])
           (if (and (bytevector? v) (#3%bytevector=? v bv))
               body
               (fail v #,(get-pattern x))))]
      [(_ e () fail body)
       #`(let ([v e])
           (if (null? v)
               body
               (fail v #,(get-pattern x))))]
      [(_ e (first . rest) fail body)
       #`(let ([v e])
           (if (pair? v)
               (match-help (#3%car v) first fail
                 (match-help (#3%cdr v) rest fail body))
               (fail v #,(get-pattern x))))]
      [(_ e #(element ...) fail body)
       #`(let ([v e])
           (if (and (vector? v)
                    (#3%fx= (#3%vector-length v) (length '(element ...))))
               (match-vector v 0 (element ...) fail body)
               (fail v #,(get-pattern x))))]))

  (define-syntax match-record
    (syntax-rules (unquote)
      [(_ v (record) fail body) body]
      [(_ v (record (unquote field) . rest) fail body)
       (let ([field (record no-check field v)])
         (match-record v (record . rest) fail body))]
      [(_ v (record [field pattern] . rest) fail body)
       (match-help (record no-check field v) pattern fail
         (match-record v (record . rest) fail body))]))

  (define-syntax match-vector
    (syntax-rules ()
      [(_ v i () fail body) body]
      [(_ v i (pattern . rest) fail body)
       (match-help (#3%vector-ref v i) pattern fail
         (match-vector v (fx+ i 1) rest fail body))]))
  )

(define-syntax (define-record x)
  (syntax-case x ()
    [(_ name field ...)
     (and (identifier? #'name)
          (let valid-fields? ([fields #'(field ...)] [seen '()])
            (syntax-case fields ()
              [(fn . rest)
               (and (identifier? #'fn)
                    (let ([f (datum fn)])
                      (when (memq f seen)
                        (bad-syntax "duplicate field" x #'fn))
                      (valid-fields? #'rest (cons f seen))))]
              [() #t]
              [_ #f])))
     #'(begin
         (define-syntax (name x)
           (define (valid-bindings? bindings seen)
             (syntax-case bindings ()
               [((fn fv) . rest)
                (and (identifier? #'fn)
                     (let ([f (datum fn)])
                       (when (memq f seen)
                         (bad-syntax "duplicate field" x #'fn))
                       (unless (memq f '(field ...))
                         (bad-syntax "unknown field" x #'fn))
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
                (if (syntax-datum-eq? #'fn f)
                    #'fv
                    (find-binding f #'rest))]
               [() #f]))
           (define (remove-binding f bindings)
             (syntax-case bindings ()
               [((fn fv) . rest)
                (if (syntax-datum-eq? #'fn f)
                    #'rest
                    #`((fn fv) #,@(remove-binding f #'rest)))]))
           (define (find-index fn fields index)
             (let ([f (scar fields)])
               (if (syntax-datum-eq? f fn)
                   index
                   (find-index fn (scdr fields) (+ index 1)))))
           (syntax-case x ()
             [(name make . bindings)
              (and (eq? (datum make) 'make)
                   (valid-bindings? #'bindings '()))
              #`(vector 'name #,@(make-record #'(field ...) #'bindings))]
             [(name copy e . bindings)
              (and (eq? (datum copy) 'copy)
                   (valid-bindings? #'bindings '()))
              #`(let ([src e])
                  (unless (name is? src)
                    (errorf 'record-copy "~s is not a ~a~a" src 'name
                      #,(find-source x)))
                  (vector 'name #,@(copy-record #'(field ...) 1 #'bindings)))]
             [(name is? e)
              (eq? (datum is?) 'is?)
              #'(let ([x e])
                  (and (vector? x)
                       (#3%fx= (#3%vector-length x) (length '(name field ...)))
                       (eq? (#3%vector-ref x 0) 'name)))]
             [(name fn e)
              (syntax-datum-eq? #'fn #'field)
              #`(let ([x e])
                  (unless (name is? x)
                    (errorf 'record-ref "~s is not a ~a~a" x 'name
                      #,(find-source x)))
                  (#3%vector-ref x #,(find-index #'fn #'(field ...) 1)))]
             ...
             [(name no-check fn e)
              (and (eq? (datum no-check) 'no-check)
                   (syntax-datum-eq? #'fn #'field))
              #`(#3%vector-ref e #,(find-index #'fn #'(field ...) 1))]
             ...))
         (define-property name fields '(field ...)))]))
