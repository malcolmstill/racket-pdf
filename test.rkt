#lang typed/racket

(require "object.rkt"
         "catalog.rkt"
         "page.rkt"
         "build.rkt")

(: page Dictionary)
(define page (dictionary
              'Type 'Page
              'MediaBox (list 0 0 595 842)))

(: pages PageTree)
(define pages (ann (dictionary
                    'Type 'Pages
                    'Parent (PDFNull)
                    'Kids (list (Indirect page))
                    'Count 1) PageTree))

(: cat Catalog)
(define cat (catalog (Indirect pages)))

(display (compile-pdf cat))

(write-pdf (compile-pdf cat) "test.pdf")