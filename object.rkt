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
                           [gen-num : Nonnegative-Integer]))

(define-type PDFObject (U Symbol ; Let's represent PDF name objectes with symbols
                          String ; Likewise we'll represent PDF string objects with Racket's strings
                          PDFNull ; Don't think we can use Racket's null (the empty list)...let's make our own
                          Boolean 
                          ;HexString
                          Number
                          Array ; PDF arrays
                          Dictionary
                          Stream
                          IndirectObject
                          IndirectReference
                          ))

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