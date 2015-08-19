#lang typed/racket

(require "object.rkt"
         "extensions.rkt"
         "uri.rkt"
         "versions.rkt")

(define-type PageLayout (U 'SinglePage
                           'OneColumn
                           'TwoColumnLeft
                           'TwoColumnRight
                           'TwoPageLeft
                           'TwoPageRight))
(define-predicate page-layout? PageLayout)

(define-type PageMode (U 'UseNone
                         'UseOutlines
                         'UseThumbs
                         'FullScreen
                         'UseOC
                         'UseAttachments))
(define-predicate page-mode? PageMode)

#|
(: catalog (->*
            (IndirectReference)
            (#:version (U Version PDFNull)
                       #:extensions (U Extensions PDFNull)
                       ; TODO #:page-labels
                       #:names (U Dictionary PDFNull)
                       #:dests (U Dictionary PDFNull)
                       #:viewer-preferences (U Dictionary PDFNull)
                       #:page-layout (U PageLayout PDFNull)
                       #:page-mode (U PageMode PDFNull)
                       #:outlines (U IndirectReference PDFNull)
                       #:threads (U IndirectReference PDFNull) ;(U Array PDFNull)
                       #:open-action (U Array Dictionary PDFNull)
                       #:aa (U Dictionary PDFNull)
                       #:uri (U CatalogURI PDFNull)
                       #:acroform (U Dictionary PDFNull)
                       #:metadeta (U PDFStream PDFNULL)
                       #:struct-tree-root (U Dictionary 
                       ) Dictionary))
(define (catalog #:version [version (PDFNull)]
                 #:extensions [extensions #f]
                 pages ; indirect reference
                 ; TODO #:pages-labels [page-labels #f]
                 #:names [names #f]
                 #:dests [dests #f] ; indirect-reference
                 #:view-preferences [view-preferences #f]
                 #:page-layout [page-layout #f]
                 )
  (dictionary
   'Type 'Catalog
|#
                 