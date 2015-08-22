#lang racket

(define (pairspec z)
  (match z
    [(list 'Pairof nm (list-no-order U y PDFNull)) (list 'Pairof nm y)]
    [(list 'Pairof nm (list-no-order PDFNull y ...)) (list 'Pairof nm (remove* (list 'PDFNull) y))]
    [else z]))

(define (generate-dictionary name typespec)
  (match typespec
    [(list 'List (list 'Pairof nm (list-no-order PDFNull y ...)) z ...)
     (cons 'List 
           (map pairspec z))]
    [(list 'List (list 'Pairof nm type ...)) type]))

(define (make-racket-name name)
  (string->symbol
   (string-downcase
    (list->string
     (reverse (foldl (λ (ch ch-list)
                       (if (empty? ch-list)
                           (cons ch ch-list)
                           (cond
                             [(and (char-upper-case? ch) (char-upper-case? (car ch-list))) (cons ch ch-list)]
                             [(and (char-upper-case? ch) (char-lower-case? (car ch-list))) (cons ch (cons #\- ch-list))]
                             [(and (char-lower-case? ch) (char-upper-case? (car ch-list))) (cons ch (cons #\- ch-list))]
                             [else (cons ch ch-list)])))
                     '() (string->list name)))))))

(define (symbol->keyword symbol)
  (string->keyword (symbol->string symbol)))

#;
(generate-dictionary
 '(List
   (Pairof 'Type 'Page)
   (Pairof 'Parent IndirectReference)
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
   (Pairof 'ID (U String PDFNull))
   (Pairof 'PZ (U Number PDFNull))
   (Pairof 'SeparationInfo (U Dictionary PDFNull))
   (Pairof 'Tabs (U 'R 'C 'S PDFNull))
   (Pairof 'TemplateInstantiated (U Symbol PDFNull))
   (Pairof 'PresSteps (U Dictionary PDFNull))
   (Pairof 'UserUnit (U Number PDFNull))
   (Pairof 'VP (U Dictionary PDFNull))))
