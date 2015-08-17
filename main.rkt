#lang racket/base

#|
PDF includes eight basic types of objects: Boolean values, Integer and Real numbers, Strings, Names, Arrays,
Dictionaries, Streams, and the null object.
|#

;(: pdf-version (Positive-Integer -> Bytes))
(define (pdf-heading major minor)
  (strings->bytes/utf-8
   (string-append "%PDF-"
		  (number->string major) "."
		  (number->string minor)
                  "%‚„œ")))

;(: end-of-file (-> Bytes))
(define (end-of-file)
  #"%%EOF")

