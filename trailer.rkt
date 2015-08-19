#lang typed/racket

(require "object.rkt")

(provide (all-defined-out))

#|

|#

(struct Trailer ([dict : Dictionary] [offset : Integer]))
(define-type TrailerDictionary (List
                                (Pairof 'Size Integer)
                                (Pairof 'Prev (U Integer PDFNull))
                                (Pairof 'Root IndirectReference)
                                (Pairof 'Encrypt (U Dictionary PDFNull))
                                (Pairof 'Info (U Dictionary PDFNull))
                                (Pairof 'ID Array)))
       
(: trailer (->* (Integer IndirectReference Integer)
                (#:prev Integer
                        #:encrypt Dictionary
                        #:info Dictionary
                        #:id Array) Trailer))
(define (trailer size
                 root
                 #:prev [prev (PDFNull)]
                 #:encrypt [encrypt (PDFNull)]
                 #:info [info (PDFNull)]
                 #:id [id (PDFNull)]
                 offset)
  (Trailer (dictionary
            'Size size
            'Prev prev
            'Root root
            'Encrypt encrypt
            'Info info
            'ID id)
           offset))