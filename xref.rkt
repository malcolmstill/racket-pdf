#lang typed/racket

(require racket/match)

(provide object-0
         make-entry
         xref)

(: line-feed Bytes)
(define line-feed (bytes 10))

(: object-0 Bytes)
(define object-0 (bytes-append #"0000000000 65535 f " line-feed))

(: make-entry : Integer Integer Symbol -> Bytes)
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

(: xref : Integer Integer (Listof Bytes) -> Bytes)
(define (xref initial n entries)
  (apply bytes-append
         #"xref" line-feed
         (string->bytes/utf-8 (number->string initial))
         #" "
         (string->bytes/utf-8 (number->string n)) line-feed
         object-0
         entries))