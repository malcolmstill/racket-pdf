#lang typed/racket

(require "object.rkt")

(provide (all-defined-out))
#|

Each page of the PDF is represented by a page object. The page object
is a dictionary.

|#



#|
(define-type PageObject
  (List
   (Pairof 'Type 'Page)
   (Pairof 'Parent 'IndirectReference)
   (Pairof 'LastModified (U String PDFNull))
   (Pairof 'Resources (U Dictionary PDFNull))
   (Pairof 'MediaBox (U (Arrayof Number) PDFNull))
   (Pairof 'CropBox (U (Arrayof Number) PDFNull))
   (Pairof 'BleedBox (U (Arrayof Number) PDFNull))
   (Pairof 'TrimBox (U (Arrayof Number) PDFNull))
   (Pairof 'ArtBox (U (Arrayof Number) PDFNull))
   (Pairof 'BoxColorInfo (U Dictionary PDFNull))
   (Pairof 'Contents (U Stream 
|#

