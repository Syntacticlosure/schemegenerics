#lang racket
(provide define-class define-instance)
(define zipWith
  (lambda (f a b)
    (if (or (null? a) (null? b))
        '()
        (cons (f (car a) (car b))
              (zipWith f (cdr a) (cdr b))))))

(define-syntax define-class
  (lambda (stx)
    (define generate-objects
    (lambda (stx)
      (syntax-case stx ()
        [(x r ...) #`((lambda (v) #t) #,@(generate-objects #'(r ...)))]
        [() #`()])))
    (syntax-case stx ()
      [(_ (class-name parameters ...)
          clauses ...) (with-syntax
                           ([table-name (datum->syntax
                                         stx
                                         (string->symbol
                                         (string-append "*" (symbol->string (syntax->datum #'class-name))
                                                       "-table*")))]
                            [extender-name (datum->syntax
                                         stx
                                         (string->symbol
                                         (string-append "*" (symbol->string (syntax->datum #'class-name))
                                                       "-extend*")))]
                            )
                           #`(begin (define table-name '())
                                    (define extender-name '())
                                    (define-class "expanding" class-name extender-name table-name (parameters ...)
                                      clauses ...)))]
      [(_ "expanding" class-name extender-name table-name (parameters ...)
          [(function p ...) res]
          clauses ...
          )
       (with-syntax ([(a ...) (generate-temporaries #'(p ...))])
       #`(begin
           (set! extender-name (cons (cons 'function (lambda (parameters ...)
                                                  (list p ...))) extender-name))
           (define-instance (class-name #,@(generate-objects #'(parameters ...)))
             [function res])
          (define function
         (lambda (a ...)
           (let loop ([rest-tb table-name])
             (if (null? rest-tb)
                 (error "Type Error:No instance for " (list a ...))
                 (if  (and (eq? 'function (cadar rest-tb))
                              (andmap (lambda (x) x) (zipWith (lambda (f v) (f v))
                              (caar rest-tb)(list a ...))))
                     ((cddar rest-tb) a ...)                      
                     (loop (cdr rest-tb)))
                 ))))
          (define-class "expanding" class-name extender-name table-name (parameters ...) clauses ...)))]
      [(_ "expanding" class-name extender-name table-name (parameters ...))
       #'(void)]
      )))

(define-syntax define-instance
  (lambda (stx)
    (syntax-case stx ()
      [(_ (class-name type ...) [function value] ...)
        (with-syntax
            ([table-name (datum->syntax
                                         stx
                                         (string->symbol
                                         (string-append "*" (symbol->string (syntax->datum #'class-name))
                                                       "-table*")))]
                            [extender-name (datum->syntax
                                         stx
                                         (string->symbol
                                         (string-append "*" (symbol->string (syntax->datum #'class-name))
                                                       "-extend*")))]
                            )
          #'(set! table-name (append
                                (list (cons ((cond ((assq  'function extender-name) =>
                                                                            cdr)
                                            (else (error "Can't find definition of " 'function
                                                         "in type class " 'class-name)))
                                             type ...)
                                      (cons 'function value)) ...) table-name)))])))

(define-class (Eq a)
  [(== a a) (lambda (x y) (not (/= x y)))]
  [(/= a a) (lambda (x y) (not (== x y)))]
  )

(define-instance (Eq number?)
  [== eqv?])

(define-instance (Eq pair?)
  [== (lambda (x y)
        (and (== (car x) (car y))
             (== (cdr x) (cdr y))))])

(define-instance (Eq list?)
  [== (lambda (x y) #t)])

(define-instance (Eq symbol?)
  [== eq?])

(define-class (mapable a)
  [(mmap procedure? a) (lambda (f l) (error "Can't find Map function"))])

(define-instance (mapable list?)
  [mmap map])

(define-instance (mapable vector?)
  [mmap vector-map])


(define-class (reference a index)
  [(ref a index) (lambda (a b) (error "Can't find Reference function"))])

(define-instance (reference list? number?)
  [ref list-ref])

(define-instance (reference vector? number?)
  [ref vector-ref])

(define-instance (reference list? symbol?)
  [ref (lambda (x y) (assq y x))])

(ref '(1 2 3 4) 3)
(ref '((a . 3) (b . 3) (c . 9)) 'a)

(mmap (lambda (x) x) '(1 2 3 4))
(mmap (lambda (x) x) #(1 2 3 4))

(== '(1 2 . 3) '(1 4 . 3))

(/= 1 2)





  


         