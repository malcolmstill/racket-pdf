#lang typed/racket

(require "object.rkt"
         "page.rkt")

(provide (all-defined-out))

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