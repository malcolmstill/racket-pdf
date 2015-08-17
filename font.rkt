#lang racket/base

(require "object.rkt")



(define (font name base-font)
  (dictionary-object
   (dictionary-pair (name-object "Type") (name-object "Font"))
   (dictionary-pair (name-object "Shite")
                    (dictionary-object
                     (dictionary-pair (name-object "Type") (name-object "Font"))))))

;(define (font ..)
;  (dict
;   "Type" (name-object "Font")
;   "Shite" (dict
;            "Type" (array-object 1 2 3))))
            