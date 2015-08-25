#lang typed/racket

(require racket/string
         racket/list
         "versions.rkt")

(provide (all-defined-out))


#|
Indirect definition has to go before the Dictionary predicate for some reason?
|#
(struct (a) Indirect ([object : a]) #:transparent)

(struct PDFNull ())

#|
Maybe we should just use a vector?
|#
(define-type Array (Listof PDFObject)) ; not a Racket array
(define-predicate Array? Array)
(define-type Arrayof Listof)



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

#|
Maybe we need just Indirect. (Indirect (Dictionary ...))
|#


(struct IndirectObject ([obj-num : Nonnegative-Integer]
                        [gen-num : Nonnegative-Integer]
                        [object : PDFObject]) #:transparent)

#|
Might be useful at some future point to parameterise IndirectReference
on the type of reference?
|#
(struct IndirectReference ([obj-num : Nonnegative-Integer]
                           [gen-num : Nonnegative-Integer]) #:transparent)

(define-type PDFObject (U Symbol ; Let's represent PDF name objects with symbols
                          String ; Likewise we'll represent PDF string objects with Racket's strings
                          PDFNull ; Don't think we can use Racket's null (the empty list)...let's make our own
                          Boolean
                          Bytes ; 23/08/15, do we need this?
                          ;HexString
                          Real
                          Array ; PDF arrays
                          Dictionary
                          Stream
                          (Indirect PDFObject)
                          IndirectObject
                          IndirectReference))

(struct Stream ([dict : Dictionary] [data : Bytes]))

(define-type StreamDictionary (U (List
                                  (Pairof 'Length Nonnegative-Integer)
                                  (Pairof 'Filter (U Symbol Array PDFNull))
                                  (Pairof 'DecodeParms (U Dictionary Array PDFNull))
                                  (Pairof 'DL (U Nonnegative-Integer PDFNull)))
                                 (List
                                  (Pairof 'Length Nonnegative-Integer)
                                  (Pairof 'F String)
                                  (Pairof 'FFilter (U Symbol Array PDFNull))
                                  (Pairof 'FDecodeParms (U Dictionary Array PDFNull))
                                  (Pairof 'DL (U Nonnegative-Integer PDFNull)))))

#|
All stream objects must be indirect objects. The stream dictionary must be
a direct object within the stream object.
|#
(: stream (->* (Bytes) (#:filter (U Symbol Array)
                                 #:decode-parms (U Dictionary Array)
                                 #:dl Nonnegative-Integer) Stream))
(define (stream #:filter [filter (PDFNull)]
                #:decode-parms [decode-parms (PDFNull)]
                #:dl [dl (PDFNull)]
                bytes)
  (Stream
   (dictionary
    'Length (bytes-length bytes)
    'Filter filter
    'DecodeParms decode-parms
    'DL dl)
   bytes))

(: file-stream (->* (String) (#:filter (U Symbol Array)
                                      #:decode-parms (U Dictionary Array)
                                      #:dl Nonnegative-Integer) Stream))
(define (file-stream f
                     #:filter [filter (PDFNull)]
                     #:decode-parms [decode-parms (PDFNull)]
                     #:dl [dl (PDFNull)])
  (Stream
   (dictionary
    'Length 0
    'F f
    'FFilter filter
    'FDecodeParms decode-parms
    'DL dl)
   #""))


