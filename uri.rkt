#lang typed/racket

(require "object.rkt")

(provide (all-defined-out))

(define-type URIDictionary (List (Pairof 'S 'URI)
                                 (Pairof 'URI String)
                                 (Pairof 'IsMap (U PDFNull Boolean))))

(: uri (->* (String) (Boolean) URIDictionary))
(define (uri s [b (PDFNull)])
  (dictionary
   'S 'URI
   'URI s
   'IsMap b))

(define-type CatalogURIDictionary (List (Pairof 'S 'URI)
                                        (Pairof 'Base String)))

(: catalog-uri : String -> CatalogURIDictionary)
(define (catalog-uri s)
  (dictionary
   'S 'URI
   'Base s))