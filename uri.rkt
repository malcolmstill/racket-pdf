#lang typed/racket

(require "object.rkt")

(provide (all-defined-out))

(define-type URI (List (Pairof 'S 'URI)
                       (Pairof 'URI String)
                       (Pairof 'IsMap (U PDFNull Boolean))))

(: uri (->* (String) (Boolean) URI))
(define (uri s [b (PDFNull)])
  (dictionary
   'S 'URI
   'URI s
   'IsMap b))

(define-type CatalogURI (List (Pairof 'S 'URI)
                              (Pairof 'Base String)))

(: catalog-uri : String -> CatalogURI)
(define (catalog-uri s)
  (dictionary
   'S 'URI
   'Base s))