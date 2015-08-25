#lang typed/racket

(require "object.rkt"
         "page.rkt")

(provide (all-defined-out))

(define-type NameDictionary (List
                             (Pairof 'Dests (U Dictionary PDFNull)) ; Name tree
                             (Pairof 'AP (U Dictionary PDFNull))
                             (Pairof 'JavaScript (U Dictionary PDFNull))
                             (Pairof 'Pages (U Dictionary PDFNull))
                             (Pairof 'Templates (U Dictionary PDFNull))
                             (Pairof 'IDS (U Dictionary PDFNull))
                             (Pairof 'URLS (U Dictionary PDFNull))
                             (Pairof 'EmbeddedFiles (U Dictionary PDFNull))
                             (Pairof 'AlternatePresentations (U Dictionary PDFNull))
                             (Pairof 'Renditions (U Dictionary PDFNull))))

