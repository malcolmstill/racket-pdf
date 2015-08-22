#lang typed/racket

(require "object.rkt"
         "catalog.rkt"
         "page.rkt"
         "build.rkt")

;(: contents Steam)


(: page1 Page)
(define page1 (page (PDFNull)
                    ;#:media-box (list 0 0 10 10)
                    ;#:contents contents
                    ))

(: pages PageTree)
(define pages (dictionary
                    'Type 'Pages
                    'Parent (PDFNull)
                    'MediaBox (list 0 0 612 794)
                    'Kids (list (Indirect page1)
                                (Indirect page1))
                    'Count 2))

(: cat Catalog)
(define cat (catalog (Indirect pages)))

(display (compile-pdf cat))

(write-pdf (compile-pdf cat) "test.pdf")