#lang typed/racket

(require "object.rkt")

(provide (all-defined-out))

#|
Each page of the PDF is represented by a page object. The page object
is a dictionary.
|#

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
   (Pairof 'MediaBox (U (Arrayof Real) PDFNull))
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

#|
Is my type pedantry too much?

Maybe the optional values taking up space will cause issues later on with memory.

Leave it for the moment and we can always just go back to plain dictionaries for
everything once it's working. For the moment it will probably help me get things
correct. Correctness then worry about performance.

If we could use macros to expand into type defitions we could generate all
permutations of optional dictionary entries. Though that could potentially generate
a lot of type defintions. TODO: work out how many permutations that would be.
|#

#|
Let's build a page. Note that /Parent *has* to be an IndirectReference but this will
be passed down by the parent when compile-pdf is called.
|#
(define-type Page
  (List
   (Pairof 'Type 'Page)
   (Pairof 'Parent (U IndirectReference PDFNull))
   (Pairof 'LastModified (U String PDFNull))
   (Pairof 'Resources (U Dictionary PDFNull))
   (Pairof 'MediaBox (U (Arrayof Real) PDFNull))
   (Pairof 'CropBox (U (Arrayof Real) PDFNull))
   (Pairof 'BleedBox (U (Arrayof Real) PDFNull))
   (Pairof 'TrimBox (U (Arrayof Real) PDFNull))
   (Pairof 'ArtBox (U (Arrayof Real) PDFNull))
   (Pairof 'BoxColorInfo (U Dictionary PDFNull))
   (Pairof 'Contents (U (Indirect Stream) (Arrayof (Indirect Stream)) PDFNull))
   (Pairof 'Rotate (U Real PDFNull))
   (Pairof 'Group (U Dictionary PDFNull))
   (Pairof 'Thumb (U Stream PDFNull))
   (Pairof 'B (U Array PDFNull))
   (Pairof 'Dur (U Real PDFNull))
   (Pairof 'Trans (U Dictionary PDFNull))
   (Pairof 'Annots (U (Arrayof (Indirect Dictionary)) PDFNull))
   (Pairof 'AA (U Dictionary PDFNull))
   (Pairof 'Metadata (U Stream PDFNull))
   (Pairof 'PieceInfo (U Dictionary PDFNull))
   (Pairof 'StructParents (U Integer PDFNull))
   (Pairof 'ID (U String PDFNull)) ; spec says "byte string"
   (Pairof 'PZ (U Real PDFNull))
   (Pairof 'SeparationInfo (U Dictionary PDFNull))
   (Pairof 'Tabs (U 'R 'C 'S PDFNull))
   (Pairof 'TemplateInstantiated (U Symbol PDFNull))
   (Pairof 'PresSteps (U Dictionary PDFNull))
   (Pairof 'UserUnit (U Real PDFNull)) ; default to 1/72 inch (1 pt)
   (Pairof 'VP (U Dictionary PDFNull))))

(define-predicate Page? Page)


(: page (->* ((U IndirectReference PDFNull)) (#:last-modified String
                                         #:resources Dictionary
                                         #:media-box (Arrayof Real)
                                         #:crop-box (Arrayof Real) 
                                         #:bleed-box (Arrayof Real)
                                         #:trim-box (Arrayof Real)
                                         #:art-box (Arrayof Real)
                                         #:box-color-info Dictionary
                                         #:contents (U (Indirect Stream) (Arrayof (Indirect Stream)))
                                         #:rotate Real
                                         #:group Dictionary
                                         #:thumb Stream
                                         #:b Array
                                         #:dur Real
                                         #:trans Dictionary
                                         #:annots (Arrayof (Indirect Dictionary))
                                         #:aa Dictionary
                                         #:metadata Stream
                                         #:piece-info Dictionary
                                         #:struct-parents Integer
                                         #:id String
                                         #:pz Real
                                         #:separation-info Dictionary
                                         #:tabs (U 'R 'C 'S)
                                         #:template-instantiated Symbol
                                         #:pres-steps Dictionary
                                         #:user-unit Real
                                         #:vp Dictionary) Page))
(define (page parent
              #:last-modified [last-modified (PDFNull)]
              #:resources [resources (PDFNull)]
              #:media-box [media-box (PDFNull)]
              #:crop-box [crop-box (PDFNull)]
              #:bleed-box [bleed-box (PDFNull)]
              #:trim-box [trim-box (PDFNull)]
              #:art-box [art-box (PDFNull)]
              #:box-color-info [box-color-info (PDFNull)]
              #:contents [contents (PDFNull)]
              #:rotate [rotate (PDFNull)]
              #:group [group (PDFNull)]
              #:thumb [thumb (PDFNull)]
              #:b [b (PDFNull)]
              #:dur [dur (PDFNull)]
              #:trans [trans (PDFNull)]
              #:annots [annots (PDFNull)]
              #:aa [aa (PDFNull)]
              #:metadata [metadata (PDFNull)]
              #:piece-info [piece-info (PDFNull)]
              #:struct-parents [struct-parents (PDFNull)]
              #:id [id (PDFNull)]
              #:pz [pz (PDFNull)]
              #:separation-info [separation-info (PDFNull)]
              #:tabs [tabs (PDFNull)]
              #:template-instantiated [template-instantiated (PDFNull)]
              #:pres-steps [pres-steps (PDFNull)]
              #:user-unit [user-unit (PDFNull)]
              #:vp [vp (PDFNull)])
  (dictionary
   'Type 'Page
   'Parent parent
   'LastModified last-modified
   'Resources resources
   'MediaBox media-box
   'CropBox crop-box
   'BleedBox bleed-box
   'TrimBox trim-box
   'ArtBox art-box
   'BoxColorInfo box-color-info
   'Contents contents
   'Rotate rotate
   'Group group
   'Thumb thumb
   'B b
   'Dur dur 
   'Trans trans
   'Annots annots
   'AA aa
   'Metadata metadata
   'PieceInfo piece-info
   'StructParents struct-parents
   'ID id
   'PZ pz
   'SeparationInfo separation-info
   'Tabs tabs
   'TemplateInstantiated template-instantiated
   'PresSteps pres-steps
   'UserUnit user-unit
   'VP vp))


   
   
   
#|
(: pages->balanced-tree : (Listof Page) -> PageTree)
(define (pages->balanced-tree pages)
  )|#
