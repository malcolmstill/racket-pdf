#lang typed/racket

(require "object.rkt"
         "bytes.rkt")

(provide text->stream)

(: text->stream : Symbol Number Number Number String  -> Stream)
(define (text->stream font size x y string)
  (stream 
   (bytes-append
    #"BT " (->bytes font) #" " (->bytes size) #" Tf " (->bytes x) #" " (->bytes y) #" Td " (->bytes string) #" Tj ET")))