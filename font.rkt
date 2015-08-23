#lang typed/racket

(require "object.rkt")

(provide (all-defined-out))

(define-type Font (List
                   (Pairof 'Font Dictionary)))

(define-type FontType (U Type1))

(: font-resources : Dictionary -> Dictionary)
(define (font-resources fonts)
  (list
   (cons 'Fonts fonts)))

(: make-font : Symbol FontType -> Dictionary)
(define (make-font name font-type)
  (dictionary
   name font-type))

(define-type Type1 (List
                    (Pairof 'Type 'Font)
                    (Pairof 'Subtype 'Type1)
                    (Pairof 'BaseFont Symbol)))

(: type1 : Symbol -> Type1)
(define (type1 name)
  (dictionary
   'Type 'Font
   'Subtype 'Type1
   'BaseFont name))