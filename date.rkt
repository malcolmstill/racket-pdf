#lang typed/racket

(provide (all-defined-out))

(: date : Integer Integer Integer Integer Integer Integer (U "+" "-" "Z") Integer Integer -> String)
(define (date YYYY MM DD HH mm SS O HHoff mmoff)
  (string-append
   (number->string YYYY)
   (number->string MM)
   (number->string DD)
   (number->string HH)
   (number->string mm)
   (number->string SS)
   O
   (number->string HHoff)
   (number->string mmoff)))