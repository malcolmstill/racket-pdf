#lang typed/racket

(require "object.rkt")

(provide trailer)

#|
Get error with the following. Moving to object.rkt seems to fix it?
../../../Applications/Racket v6.2/share/pkgs/typed-racket-lib/typed-racket/static-contracts/instantiate.rkt:88:10: hash-ref: no value found for key
  key: #<syntax n*13>

(define-type TrailerDictionary (List
                                (Pairof 'Size Integer)
                                (Pairof 'Prev (U Integer PDFNull))
                                (Pairof 'Root (Indirect Dictionary))
                                (Pairof 'Encrypt (U Dictionary PDFNull))
                                (Pairof 'Info (U Dictionary PDFNull))
                                (Pairof 'ID (U Array PDFNull))
                                ))

(struct Trailer ([dict : TrailerDictionary] [offset : Integer]) #:transparent)
|#

(: trailer (->* (Integer PDFObject Integer)
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

