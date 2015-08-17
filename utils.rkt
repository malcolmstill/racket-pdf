#lang racket

(provide (all-defined-out))

(define (interleave x y)
  (define (interleave-rec a b acc)
    (match a
      ['() (match b
             ['() (reverse acc)]
             [(list b1 bs ...) (interleave-rec a bs (cons b1 acc))])]
      [(list a1 as ...) (match b
                          ['() (interleave-rec as b (cons a1 acc))]
                          [(list b1 bs ...) (interleave-rec as bs (cons b1 (cons a1 acc)))])]))
  (interleave-rec x y '()))

(define (pair-off x)
  (when (not (even? (length x)))
    (error 'pair-off "list must have even number of elements"))
  (define (pair-off-rec x acc)
    (match x
      ['() (reverse acc)]
      [(list x y z ...) (pair-off-rec z (cons (cons x y) acc))]))
  (pair-off-rec x '()))
      

(define (odd x)
  (if (empty? x)
      x
      (even (cdr x))))

(define (even x)
  (if (empty? x)
      x
      (cons (car x) (odd (cdr x)))))