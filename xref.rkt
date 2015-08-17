#lang racket

(require racket/match
         "object.rkt")

(define object-0 (bytes-append #"0000000000 65535 f " line-feed))

(define (make-entry offset generation n/f)
  (bytes-append
   (string->bytes/utf-8 (~a offset #:width 10 #:pad-string "0" #:align 'right))
   #" "
   (string->bytes/utf-8 (~a generation #:width 5 #:pad-string "0" #:align 'right))
   #" "
   (match n/f
     ['n #"n"]
     ['f #"f"])
   #" " line-feed))

;(define (find-indirect-objects ))



;(define (xref . objects))