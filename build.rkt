#lang typed/racket

(require "bytes.rkt"
         "object.rkt"
         ;"catalog.rkt"
         ;"page.rkt"
         "trailer.rkt"
         "xref.rkt")

(provide (all-defined-out))

#|
build.rkt. Here we put together the objects of our PDF file
|#

(: object-count Nonnegative-Integer)
(define object-count 0)

(: object-list (Listof PDFObject))
(define object-list (list))

#|
resolve-indirect traverses the PDFObject tree replacing any (Indirect obj)
with an (IndirectReference on gn) and conses (IndirectObject on gn obj)
onto object-list. In the future we may want to add an optimiser that makes
objects indirect if they can be (increases the size of the PDF but speeds up
access time?)
|#
(: resolve-indirect : PDFObject -> PDFObject)
(define (resolve-indirect obj)
  (cond
    [(Dictionary? obj) (map (λ ([key-value : (Pairof Symbol PDFObject)])
                              (cons (car key-value) (resolve-indirect (cdr key-value)))) obj)]
    [(Array? obj) (map resolve-indirect obj)]
    [(Indirect? obj) (begin
                       (set! object-count (+ object-count 1))
                       (let [(obj-no object-count)]
                         (set! object-list
                               (cons (IndirectObject obj-no 0 (resolve-indirect (Indirect-object obj)))
                                     object-list))
                         (IndirectReference obj-no 0)))]
    [else obj]))

#|
compile-pdf. This is where all our hard work gets rewarded.
|#
(: compile-pdf : Catalog -> Bytes)
(define (compile-pdf cat)
  (set! object-list '())
  (set! object-count 0)
  (define root (resolve-indirect (Indirect cat)))
  
  (define heading (pdf-heading 1 7))
  (define heading-length (bytes-length heading))

  ; Convert our objects to bytes and calculate the byte-length of each object
  (define object-bytes (map ->bytes object-list))
  (define object-byte-lengths (map bytes-length object-bytes))

  (define start-of-xref (foldl (λ ([l : Integer] [count : Integer])
                                 (+ l count)) 0 (cons heading-length object-byte-lengths)))
  ;(define xrefs (map (λ ([offset-1 : Integer])
   ;                    (make-entry (+ heading-length offset-1 1) 0 'n)) object-byte-lengths))
  (define xrefs (foldl (λ ([l : Integer] [ls : (Listof Integer)])
                         (cons (+ l (car ls)) ls)) (list heading-length) object-byte-lengths))
  ;(display xrefs)
  (define xref-table (xref 0 (+ object-count 1) (map (λ ([x : Integer])
                                                       (make-entry x 0 'n)) (reverse (cdr xrefs)))))

  ; Smash all our bytes together:
  (bytes-append
   (apply bytes-append heading
          object-bytes)
   xref-table
   (trailer->bytes (trailer (+ 1 object-count) root start-of-xref))))

(: write-pdf : Bytes String -> Void)
(define (write-pdf bytes filename)
  (define file (open-output-file filename #:mode 'binary #:exists 'replace))
  (display bytes file)
  (close-output-port file))

;(write-pdf (compile-pdf (catalog (ann (Indirect (dictionary
 ;                                                'Type 'Pages
  ;                                               'Kids (list)
   ;                                              'Count 0)) (Indirect Dictionary)))) "test.pdf")


