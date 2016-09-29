# schemegenerics
implementation of generic function in scheme inspired by haskell type classes
schemegenerics.rkt为scheme定义类似于commonlisp中generics function(通用函数，不是指泛型)的宏
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
