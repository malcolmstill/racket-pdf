#lang typed/racket

(require "object.rkt")

(provide uri
         catalog-uri)

(: uri (->* (String) (Boolean) URIDictionary))
(define (uri s [b (PDFNull)])
  (dictionary
   'S 'URI
   'URI s
   'IsMap b))


(: catalog-uri : String -> CatalogURIDictionary)
(define (catalog-uri s)
  (dictionary
   'S 'URI
   'Base s))