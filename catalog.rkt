#lang typed/racket

(require "object.rkt"
         "extensions.rkt"
         "namedict.rkt"
         "page.rkt"
         "uri.rkt"
         "versions.rkt"
         "viewer-preferences.rkt")

(provide (all-defined-out))

(define-type Catalog (List
                      (Pairof 'Type 'Catalog)
                      (Pairof 'Version (U Version PDFNull))
                      (Pairof 'Extensions (U ExtensionsDictionary PDFNull))
                      (Pairof 'Pages (Indirect Dictionary))
                      ;(Pairof 'PageLabels NumberTree)
                      (Pairof 'Names (U NameDictionary PDFNull))
                      (Pairof 'Dests (U IndirectReference PDFNull))
                      (Pairof 'ViewerPreferences (U ViewerPreferencesDictionary PDFNull))
                      (Pairof 'PageLayout (U PageLayout PDFNull))
                      (Pairof 'PageMode (U PageMode PDFNull))
                      (Pairof 'Outlines (U IndirectReference PDFNull)) ;Dictionary
                      (Pairof 'Threads (U IndirectReference PDFNull)) ; Array
                      (Pairof 'OpenAction (U Array Dictionary PDFNull))
                      (Pairof 'AA (U Dictionary PDFNull))
                      (Pairof 'URI (U CatalogURIDictionary PDFNull))
                      (Pairof 'AcroForm (U Dictionary PDFNull))
                      (Pairof 'Metadata (U Stream PDFNull))
                      (Pairof 'StructTreeRoot (U Dictionary PDFNull))
                      (Pairof 'MarkInfo (U Dictionary PDFNull))
                      (Pairof 'Lang (U String PDFNull))
                      (Pairof 'SpiderInfo (U Dictionary PDFNull))
                      (Pairof 'OutputIntents (U Array PDFNull))
                      (Pairof 'PieceInfo (U Dictionary PDFNull))
                      (Pairof 'OCProperties (U Array PDFNull))
                      (Pairof 'Perms (U Dictionary PDFNull))
                      (Pairof 'Legal (U Dictionary PDFNull))
                      (Pairof 'Requirements (U Array PDFNull))
                      (Pairof 'Collection (U Dictionary PDFNull))
                      (Pairof 'NeedsRendering (U Boolean PDFNull))))

(: catalog (->* ((Indirect Dictionary))
            (#:version Version
                       #:extensions ExtensionsDictionary
                       ; TODO #:page-labels
                       #:names NameDictionary
                       #:dests IndirectReference
                       #:viewer-preferences ViewerPreferencesDictionary
                       #:page-layout PageLayout
                       #:page-mode PageMode
                       #:outlines IndirectReference
                       #:threads IndirectReference ;(U Array PDFNull)
                       #:open-action (U Array Dictionary)
                       #:aa Dictionary
                       #:uri CatalogURIDictionary
                       #:acro-form Dictionary
                       #:metadata Stream
                       #:struct-tree-root Dictionary
                       #:mark-info Dictionary
                       #:lang String
                       #:spider-info Dictionary
                       #:output-intents Array
                       #:piece-info Dictionary
                       #:oc-properties Array
                       #:perms Dictionary
                       #:legal Dictionary
                       #:requirements Array
                       #:collection Dictionary
                       #:needs-rendering Boolean)
            Catalog))
(define (catalog #:version [version (PDFNull)]
                 #:extensions [extensions (PDFNull)]
                 pages
                 ; TODO #:pages-labels [page-labels #f]
                 #:names [names (PDFNull)]
                 #:dests [dests (PDFNull)]
                 #:viewer-preferences [viewer-preferences (PDFNull)]
                 #:page-layout [page-layout (PDFNull)]
                 #:page-mode [page-mode (PDFNull)]
                 #:outlines [outlines (PDFNull)]
                 #:threads [threads (PDFNull)]
                 #:open-action [open-action (PDFNull)]
                 #:aa [aa (PDFNull)]
                 #:uri [uri (PDFNull)]
                 #:acro-form [acro-form (PDFNull)]
                 #:metadata [metadata (PDFNull)]
                 #:struct-tree-root [struct-tree-root (PDFNull)]
                 #:mark-info [mark-info (PDFNull)]
                 #:lang [lang (PDFNull)]
                 #:spider-info [spider-info (PDFNull)]
                 #:output-intents [output-intents (PDFNull)]
                 #:piece-info [piece-info (PDFNull)]
                 #:oc-properties [oc-properties (PDFNull)]
                 #:perms [perms (PDFNull)]
                 #:legal [legal (PDFNull)]
                 #:requirements [requirements (PDFNull)]
                 #:collection [collection (PDFNull)]
                 #:needs-rendering [needs-rendering (PDFNull)])
  (dictionary
   'Type 'Catalog
   'Version version 
   'Extensions extensions
   'Pages pages
   'Names names
   'Dests dests
   'ViewerPreferences viewer-preferences
   'PageLayout page-layout
   'PageMode page-mode
   'Outlines outlines
   'Threads threads
   'OpenAction open-action
   'AA aa
   'URI uri
   'AcroForm acro-form
   'Metadata metadata
   'StructTreeRoot struct-tree-root
   'MarkInfo mark-info
   'Lang lang
   'SpiderInfo spider-info
   'OutputIntents output-intents
   'PieceInfo piece-info
   'OCProperties oc-properties
   'Perms perms
   'Legal legal
   'Requirements requirements
   'Collection collection
   'NeedsRendering needs-rendering))
