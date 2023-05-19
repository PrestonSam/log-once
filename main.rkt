#lang racket/base

(require racket/function
         racket/sequence
         racket/match
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     racket/format
                     syntax/parse)
         #;macro-debugger/expand)

(provide gather-lifted
         unpack-lifted
         print-with-label
         log-syntax-as-datum
         log-expr
         log-defs
         log-once)

(define-syntax-parser log-def
  [(_ expr:expr)
   #`(printf "~a = ~v. " #,(~s (syntax->datum #'expr)) expr)])

(define (print-with-label message v)
  (printf "~a ~v\n" message v)
  v)

(define-syntax-parser gather-lifted
  [(_ log-box:id (~and full-stx ((~literal syntax-local-lift-expression) body-stx)))
   #`(let ([lifted-id full-stx])
       (hash-set! log-box (syntax->datum lifted-id) body-stx)
       lifted-id)])

(define (unpack-lifted/internal stx log-box)
  (for/list ([child-stx (in-syntax stx)])
    (match (syntax-e child-stx)
      [(? list? value)
       (unpack-lifted/internal value log-box)]
      [(? symbol? value)
       (match (hash-ref log-box value #f)
         [#f value]
         [unpacked-value (unpack-lifted/internal unpacked-value log-box)])]
      [v v])))

(define-syntax-parser unpack-lifted
  [(_ log-box:id (~optional (~seq #:message message:string)) parent-stx)
   #`(let ([output (unpack-lifted/internal parent-stx log-box)])
       (~? (displayln message))
       (println output)
       parent-stx)])

(define-syntax-parser log-expr
  [(_ expr:expr (~optional (~seq #:port port:expr)) (~optional (~seq #:with form)))
   #`(let ([val expr])
       (fprintf (~? port (current-output-port)) "~a = ~v\n" #,(~s (syntax->datum #'expr)) val)
       (~? form)
       val)])

(define-syntax-parser log-syntax-as-datum
  [(_ expr (~optional port:expr))
   #`(let ([val expr])
       (fprintf (~? port (current-output-port)) "~a =\n    ~v\n" #,(~s (syntax->datum #'expr)) (syntax->datum val))
       val)])

(begin-for-syntax
  (define (make-incrementor id)
    (with-syntax ([id id])
      #'(λ ()
          (set! id (add1 id))
          id))))

(define-syntax-parser log-defs
  [(_ (~or (~optional (~seq #:newline/def newline-def:boolean) #:defaults ([newline-def #'#f]))
           (~optional (~seq #:newline/defs newline-defs:boolean) #:defaults ([newline-defs #'#t]))
           (~optional (~seq #:when condition:expr))
           (~optional (~seq #:message message:str))
           (~optional (~seq #:return return-expr:expr))
           expr:expr) ...)
   #:attr intermediate-newline-clause (if (syntax-e #'(~? newline-def #f)) #'(newline) #f)
   #:attr ultimate-newline-clause (if (and (not(syntax-e #'(~? newline-def #f))) ; If newline/def then newline already provided
                                           (syntax-e #'(~? newline-defs #f))) ; If newline/defs but not newline/def then provide a newline
                                      #'(newline)
                                      #f)
   #'(begin
       (when (and (~? condition))
         (~? (printf "~a: " message))
         (~@ (log-def (~? expr))
             (~? intermediate-newline-clause)) ...
         (~? (log-def return-expr))
         (~? ultimate-newline-clause))
       (~? return-expr))])

(define-syntax-parser log-once 
  [(_ (~or (~optional (~seq #:skip-count target-skip-count:nat) #:defaults ([target-skip-count #'0]))
           (~optional (~seq #:log-count target-log-count:nat) #:defaults ([target-log-count #'1]))
           (~optional (~seq #:when condition:expr))
           (~optional (~seq #:message message:str))
           (~optional (~seq #:newline/def newline-def:boolean))
           (~optional (~seq #:newline/defs newline-defs:boolean))
           (~optional (~seq #:return return-expr:expr))
           expr) ...)
   #:with logged (syntax-local-lift-expression #'#f)
   #:with run-count (syntax-local-lift-expression #'0)
   #:with ++run-count (make-incrementor #'run-count)
   #:with log-count (syntax-local-lift-expression #'0)
   #:with ++log-count (make-incrementor #'log-count)
   #:with should-run?! (syntax-local-lift-expression
                        #'(λ ()
                            (and (> (++run-count) target-skip-count)
                                 (<= (++log-count) target-log-count))))
   #:with stop-logging?! (syntax-local-lift-expression
                          #'(λ ()
                              (when (<= target-log-count log-count)
                                (set! logged #t))))
   #'(and (not logged)
          (when (and (~? condition)
                     (should-run?!))
            (begin0
              (log-defs (~? (~@ #:message message))
                        (~? (~@ #:newline/def newline-def))
                        (~? (~@ #:newline/defs newline-defs))
                        (~? (~@ #:return return-expr))
                        expr ...)
              (stop-logging?!))))])

(module+ test
  (require rackunit
           racket/port)
  
  (for ([n (in-naturals)]
        [t '(test-one test-two test-three test-three-again test-four test-five test-six)]
        [results '("n = 0. t = 'test-one. \n"
                   "n = 1. t = 'test-two. \n"
                   "n = 2. t = 'test-three. \n"
                   "n = 3. t = 'test-three-again. \n"
                   "n = 4. t = 'test-four. \n"
                   "Test 5: n = 5. t = 'test-five. \n"
                   "n = 6. \nt = 'test-six. \n")])
    (define logs
      (thunk
       (log-once n t)
       (log-once n t #:skip-count 1)
       (log-once #:skip-count 2 n #:log-count 2 t)
       (log-once #:when (and (= n 4) (eq? t 'test-four)) n t)
       (log-once #:skip-count 5 n t #:message "Test 5" )
       (log-once n #:skip-count 6 t #:newline/def #t)))
    (check-equal? (with-output-to-string logs) results)))
