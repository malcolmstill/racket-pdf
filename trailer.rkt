#lang racket

(require "object.rkt")

#|

|#

(define trailer%
  (class* object% (pdf-object-interface)
    (init dict byte-offset)
    (define current-dict dict)
    (define current-byte-offset byte-offset)
    (super-new)

    (define/public (->bytes)
      (bytes-append
       #"trailer" line-feed
       (send current-dict ->bytes) line-feed
       #"startxref" line-feed
       (number->bytes current-byte-offset) line-feed
       #"%%EOF"))))
       

(define (trailer size
                 root
                 #:prev [prev #f]
                 #:encrypt [encrypt #f]
                 #:info [info #f]
                 #:id [id #f]
                 offset)
  (when (not (is-a? root indirect-reference%))
    (error 'trailer "root must be an idirect object"))
  (new trailer%
       [dict (dictionary
              "Size" size
              "Prev" prev
              "Root" root
              "Encrypt" encrypt
              "Info" info
              "ID" id)]
       [byte-offset offset]))