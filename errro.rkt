#lang typed/racket

(require "object.rkt")

(define-type Test (List
                     (Pairof 'Type 'Catalog)
                     (Pairof 'Pages (Indirect Dictionary))
                     (Pairof 'Else (U Number PDFNull))
                     (Pairof 'Array (U Array PDFNull))))

  (struct Container ([x : Test] [f : Number]))
  
  (: TestTets (->* ((Indirect Dictionary)) (#:arr Array #:opt Number) Test))
  (define (TestTets a #:arr [c (PDFNull)] #:opt [b (PDFNull)])
    (dictionary
     'Type 'Catalog
     'Pages a
     'Else b
     'Array c))

  (Container (TestTets (Indirect (dictionary)) #:arr (list)) 4)
