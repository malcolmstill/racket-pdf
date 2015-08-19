#lang typed/racket

(require racket/string
         racket/list)

(provide (all-defined-out))

(struct PDFNull ())

(define-type Array (Listof PDFObject)) ; not a Racket array

#|
Maybe we should use a Racket hash for dictionary but I don't think this would allow us to
define types for specific dictionaries.
|#
(define-type Dictionary (Listof (Pairof Symbol PDFObject)))
(define-predicate Dictionary? Dictionary)

#|
We're going to be defining a lot of dictionaries so let's introduce some syntax to help

Apparently we can't use macros to expand in define-type,
see https://unknownparallel.wordpress.com/2012/11/05/my-experience-with-typed-racket/

(define-syntax (Dictionaryof stx)
  (syntax-case stx ()
    [(_ pairs ...)
     (with-syntax ([(paired ...) (map (λ (pair)
                                        (let ([name (car pair)]
                                              [obj (cdr pair)])
                                         #'(Pairof #,name #,obj)))
                                      (reverse (foldl
                                                (λ (e a)
                                                  (if (or (null? a) (pair? (car a)))
                                                      (cons e a)
                                                      (cons (cons(car a) e) (cdr a)))) '() #'(pairs ...))))])
     #'(List paired ...))]))

|#

(define-syntax (dictionary stx)
  (syntax-case stx ()
    [(_ pairs ...)
     (with-syntax ([(paired ...)
                    (map (λ (pair)
                           (let ([name (car pair)]
                                 [obj (cdr pair)])
                             #`(cons #,name #,obj)))
                         (reverse (foldl
                                   (λ (e a)
                                     (if (or (null? a) (pair? (car a)))
                                         (cons e a)
                                         (cons (cons(car a) e) (cdr a)))) '() (syntax->list #'(pairs ...)))))])
     #'(list paired ...))]))


(struct IndirectObject ([obj-num : Positive-Integer]
                        [gen-num : Positive-Integer]
                        [object : PDFObject]))

#|
Might be useful at some future point to parameterise IndirectReference
on the type of reference?
|#
(struct IndirectReference ([obj-num : Positive-Integer]
                           [gen-num : Positive-Integer]))

(define-type PDFObject (U Symbol ; Let's represent PDF name objectes with symbols
                          String ; Likewise we'll represent PDF string objects with Racket's strings
                          PDFNull ; Don't think we can use Racket's null (the empty list)...let's make our own
                          Boolean 
                          ;HexString
                          Number
                          Array ; PDF arrays
                          Dictionary
                          IndirectObject
                          IndirectReference
                          ))

#|
(define stream%
  (class* object% (pdf-object-interface)
    (init dict stream)
    (define current-dict dict)
    (define current-stream stream)
    (super-new)

    (define/public (->bytes)
      (bytes-append
       (send current-dict ->bytes)
       #"stream" line-feed
       current-stream line-feed
       #"endstream" line-feed))))

(define (stream-object length
		       #:filter [filter #f]
		       #:decode-parms [decode-parms #f]
		       #:f [f #f]
		       #:f-filter [f-filter #f]
		       #:f-decode-parms [f-decode-parms #f]
		       #:dl [dl #f]
		       bytes)
  (new stream%
       [dict (dictionary
              "Length" length
              "Filter" (name-object filter)
              "DecodeParms" decode-parms
              "F" f
              "FFilter" f-filter
              "FDecodeParms" f-decode-parms
              "DL" dl)]
       [stream bytes]))
|#