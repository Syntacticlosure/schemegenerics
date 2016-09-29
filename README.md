# schemegenerics
implementation of generic function in scheme inspired by haskell type classes
schemegenerics.rkt为scheme提供了泛型支持，可以定义generics function
灵感来源于haskell的type class

```
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
```
