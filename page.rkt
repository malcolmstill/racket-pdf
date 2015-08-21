#lang typed/racket

(require "object.rkt")

(provide (all-defined-out))

#|
Each page of the PDF is represented by a page object. The page object
is a dictionary.
|#

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


(define-type Page
  (List
   (Pairof 'Type 'Page)
   (Pairof 'Parent (Indirect Dictionary))
   (Pairof 'LastModified (U String PDFNull))
   (Pairof 'Resources (U Dictionary PDFNull))
   (Pairof 'MediaBox (U (Arrayof Number) PDFNull))
   (Pairof 'CropBox (U (Arrayof Number) PDFNull))
   (Pairof 'BleedBox (U (Arrayof Number) PDFNull))
   (Pairof 'TrimBox (U (Arrayof Number) PDFNull))
   (Pairof 'ArtBox (U (Arrayof Number) PDFNull))
   (Pairof 'BoxColorInfo (U Dictionary PDFNull))
   (Pairof 'Contents (U Stream (Arrayof Stream) PDFNull))
   (Pairof 'Rotate (U Integer PDFNull))
   (Pairof 'Group (U Dictionary PDFNull))
   (Pairof 'Thumb (U Stream PDFNull))
   (Pairof 'B (U Array PDFNull))
   (Pairof 'Dur (U Number PDFNull))
   (Pairof 'Trans (U Dictionary PDFNull))
   (Pairof 'Annots (U (Arrayof (Indirect Dictionary)) PDFNull))
   (Pairof 'AA (U Dictionary PDFNull))
   (Pairof 'Metadata (U Stream PDFNull))
   (Pairof 'PieceInfo (U Dictionary PDFNull))
   (Pairof 'StructParents (U Integer PDFNull))
   (Pairof 'ID (U String PDFNull)) ; spec says "byte string"
   (Pairof 'PZ (U Number PDFNull))
   (Pairof 'SeparationInfo (U Dictionary PDFNull))
   (Pairof 'Tabs (U 'R 'C 'S PDFNull))
   (Pairof 'TemplateInstantiated (U Symbol PDFNull))
   (Pairof 'PresSteps (U Dictionary PDFNull))
   (Pairof 'UserUnit (U Number PDFNull)) ; default to 1/72 inch (1 pt)
   (Pairof 'VP (U Dictionary PDFNull))))
   
   
   

