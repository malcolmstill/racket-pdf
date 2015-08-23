#lang typed/racket

(require racket/string
         racket/list
         "versions.rkt")

(provide (all-defined-out))


#|
Indirect definition has to go before the Dictionary predicate for some reason?
|#
(struct (a) Indirect ([object : a]) #:transparent)

(struct PDFNull ())

#|
Maybe we should just use a vector?
|#
(define-type Array (Listof PDFObject)) ; not a Racket array
(define-predicate Array? Array)
(define-type Arrayof Listof)



#|
Maybe we should use a Racket hash for dictionary but I don't think this would allow us to
define types for specific dictionaries.
|#
(define-type Dictionary (Listof (Pairof Symbol PDFObject)))
(define-predicate Dictionary? Dictionary)


#|
We're going to be defining a lot of dictionaries so let's introduce some syntax to help

Apparently we can't use macros to expand in define-type,
see https://unknownparallel.wordpress.com/2012/11/05/my-experience-with-typed-racket/

(define-syntax (Dictionaryof stx)
  (syntax-case stx ()
    [(_ pairs ...)
     (with-syntax ([(paired ...) (map (λ (pair)
                                        (let ([name (car pair)]
                                              [obj (cdr pair)])
                                         #'(Pairof #,name #,obj)))
                                      (reverse (foldl
                                                (λ (e a)
                                                  (if (or (null? a) (pair? (car a)))
                                                      (cons e a)
                                                      (cons (cons(car a) e) (cdr a)))) '() #'(pairs ...))))])
     #'(List paired ...))]))

|#

(define-syntax (dictionary stx)
  (syntax-case stx ()
    [(_ pairs ...)
     (with-syntax ([(paired ...)
                    (map (λ (pair)
                           (let ([name (car pair)]
                                 [obj (cdr pair)])
                             #`(cons #,name #,obj)))
                         (reverse (foldl
                                   (λ (e a)
                                     (if (or (null? a) (pair? (car a)))
                                         (cons e a)
                                         (cons (cons(car a) e) (cdr a)))) '() (syntax->list #'(pairs ...)))))])
     #'(list paired ...))]))

#|
Maybe we need just Indirect. (Indirect (Dictionary ...))
|#


(struct IndirectObject ([obj-num : Nonnegative-Integer]
                        [gen-num : Nonnegative-Integer]
                        [object : PDFObject]) #:transparent)

#|
Might be useful at some future point to parameterise IndirectReference
on the type of reference?
|#
(struct IndirectReference ([obj-num : Nonnegative-Integer]
                           [gen-num : Nonnegative-Integer]) #:transparent)

(define-type PDFObject (U Symbol ; Let's represent PDF name objects with symbols
                          String ; Likewise we'll represent PDF string objects with Racket's strings
                          PDFNull ; Don't think we can use Racket's null (the empty list)...let's make our own
                          Boolean 
                          ;HexString
                          Number
                          Array ; PDF arrays
                          Dictionary
                          Stream
                          (Indirect PDFObject)
                          IndirectObject
                          IndirectReference))

(struct Stream ([dict : Dictionary] [data : Bytes]))

(define-type StreamDictionary (U (List
                                  (Pairof 'Length Nonnegative-Integer)
                                  (Pairof 'Filter (U Symbol Array PDFNull))
                                  (Pairof 'DecodeParms (U Dictionary Array PDFNull))
                                  (Pairof 'DL (U Nonnegative-Integer PDFNull)))
                                 (List
                                  (Pairof 'Length Nonnegative-Integer)
                                  (Pairof 'F String)
                                  (Pairof 'FFilter (U Symbol Array PDFNull))
                                  (Pairof 'FDecodeParms (U Dictionary Array PDFNull))
                                  (Pairof 'DL (U Nonnegative-Integer PDFNull)))))

#|
All stream objects must be indirect objects. The stream dictionary must be
a direct object within the stream object.
|#
(: stream (->* (Bytes) (#:filter (U Symbol Array)
                                 #:decode-parms (U Dictionary Array)
                                 #:dl Nonnegative-Integer) Stream))
(define (stream #:filter [filter (PDFNull)]
                #:decode-parms [decode-parms (PDFNull)]
                #:dl [dl (PDFNull)]
                bytes)
  (Stream
   (dictionary
    'Length (bytes-length bytes)
    'Filter filter
    'DecodeParms decode-parms
    'DL dl)
   bytes))

(: file-stream (->* (String) (#:filter (U Symbol Array)
                                      #:decode-parms (U Dictionary Array)
                                      #:dl Nonnegative-Integer) Stream))
(define (file-stream f
                     #:filter [filter (PDFNull)]
                     #:decode-parms [decode-parms (PDFNull)]
                     #:dl [dl (PDFNull)])
  (Stream
   (dictionary
    'Length 0
    'F f
    'FFilter filter
    'FDecodeParms decode-parms
    'DL dl)
   #""))

#|
Having the following in a separate file gives this error:
../../../Applications/Racket v6.2/share/pkgs/typed-racket-lib/typed-racket/static-contracts/instantiate.rkt:88:10: hash-ref: no value found for key
  key: #<syntax n*13>

This appeared after I parameterised Indirect. This is a bug?
|#

(define-type TrailerDictionary (List
                                (Pairof 'Size Integer)
                                (Pairof 'Prev (U Integer PDFNull))
                                (Pairof 'Root PDFObject)
                                (Pairof 'Encrypt (U Dictionary PDFNull))
                                (Pairof 'Info (U Dictionary PDFNull))
                                (Pairof 'ID (U Array PDFNull))
                                ))

(struct Trailer ([dict : TrailerDictionary] [offset : Integer]) #:transparent)

#|
Same goes for the following:
|#

(define-type ExtensionsDictionary (List (Pairof 'Type 'DeveloperExtensions)
                                        (Pairof 'BaseVersion Version)
                                        (Pairof 'ExtensionLevel Integer)))

(define-type PageMode (U 'UseNone
                         'UseOutlines
                         'UseThumbs
                         'FullScreen
                         'UseOC
                         'UseAttachments))
(define-predicate page-mode? PageMode)

(define-type PageBoundaries (U 'MediaBox
                               'CropBox
                               'BleedBox
                               'TrimBox
                               'ArtBox))
(define-predicate page-boundaries? PageBoundaries)

(define-type PageTree
  (List
   (Pairof 'Type 'Pages)
   (Pairof 'Parent (U (Indirect Dictionary) PDFNull))
   (Pairof 'MediaBox (U (Arrayof Number) PDFNull))
   (Pairof 'Kids (Arrayof (U (Indirect PageTree) (Indirect Dictionary))))
   (Pairof 'Count Positive-Integer)))

(define-type PageTree? PageTree)

(define-type PageLayout (U 'SinglePage
                           'OneColumn
                           'TwoColumnLeft
                           'TwoColumnRight
                           'TwoPageLeft
                           'TwoPageRight))
(define-predicate page-layout? PageLayout)

(define-type ViewerPreferencesDictionary
  (List
   (Pairof 'HideToolbar (U Boolean PDFNull))
   (Pairof 'HideMenubar (U Boolean PDFNull))
   (Pairof 'HideWindowUI (U Boolean PDFNull))
   (Pairof 'FitWindow (U Boolean PDFNull))
   (Pairof 'CenterWindow (U Boolean PDFNull))
   (Pairof 'DisplayDocTitle (U Boolean PDFNull))
   (Pairof 'NonFullScreenPageMode (U PageMode PDFNull))
   (Pairof 'Direction (U 'L2R 'R2L PDFNull))
   (Pairof 'ViewArea (U PageBoundaries PDFNull))
   (Pairof 'ViewClip (U PageBoundaries PDFNull))
   (Pairof 'PrintArea (U PageBoundaries PDFNull))
   (Pairof 'PrintClip (U PageBoundaries PDFNull))
   (Pairof 'PrintScaling (U 'None 'AppDefault PDFNull))
   (Pairof 'Duplex (U 'Simplex
                      'DuplexFlipShortEdge
                      'DuplexFlipLongEdge
                      'PDFNull))
   (Pairof 'PickTrayByPDFSize (U Boolean PDFNull))
   (Pairof 'PrintPageRange (U Array PDFNull))
   (Pairof 'NumCopies (U Nonnegative-Integer PDFNull))))

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

(define-type URIDictionary (List (Pairof 'S 'URI)
                                 (Pairof 'URI String)
                                 (Pairof 'IsMap (U PDFNull Boolean))))

(define-type CatalogURIDictionary (List (Pairof 'S 'URI)
                                        (Pairof 'Base String)))

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

