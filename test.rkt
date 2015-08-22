#lang typed/racket

(require "object.rkt"
         "catalog.rkt"
         "page.rkt"
         "build.rkt")

(: page1 Page)
(define page1 (page (PDFNull)
                    ;#:media-box (list 0 0 10 10)
                    ;#:contents contents
                    ))

(: page2 Page)
(define page2 (page (PDFNull)
                    #:media-box (list 0 0 1190 842)
                    ;#:contents contents
                    ))



(: pages PageTree)
(define pages (dictionary
                    'Type 'Pages
                    'Parent (PDFNull)
                    'MediaBox (list 0 0 595 842)
                    'Kids (list (Indirect page1)
                                (Indirect page1)
                                (Indirect page2))
                    'Count 3))

(: cat Catalog)
(define cat (catalog (Indirect pages)))

(display (compile-pdf cat))

(write-pdf (compile-pdf cat) "test.pdf")