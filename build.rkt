#lang typed/racket

(require "bytes.rkt"
         "object.rkt"
         ;"catalog.rkt"
         "page.rkt"
         "trailer.rkt"
         "xref.rkt")

(provide (all-defined-out))

#|
build.rkt. Here we put together the objects of our PDF file
|#

(: object-count Nonnegative-Integer)
(define object-count 0)

(: object-list (Listof IndirectObject))
(define object-list (list))

(: has-parent? : PDFObject -> Boolean)
(define (has-parent? object)
  (or (Page? object)))


#|
resolve-indirect traverses the PDFObject tree replacing any (Indirect obj)
with an (IndirectReference on gn) and conses (IndirectObject on gn obj)
onto object-list. In the future we may want to add an optimiser that makes
objects indirect if they can be (increases the size of the PDF but speeds up
access time?)
|#
(: resolve-indirect : PDFObject PDFObject -> PDFObject)
(define (resolve-indirect obj parent)
  (cond
    [(Page? obj) (map (λ ([key-value : (Pairof Symbol PDFObject)])
                        (match key-value
                          [(cons 'Parent _) (cons 'Parent parent)]
                          [else (cons (car key-value) (resolve-indirect (cdr key-value) parent))])) obj)]
    [(Dictionary? obj) (map (λ ([key-value : (Pairof Symbol PDFObject)])
                              (cons (car key-value) (resolve-indirect (cdr key-value) parent))) obj)]
    [(Array? obj) (map (λ ([o : PDFObject])
                           (resolve-indirect o parent)) obj)]
    [(Indirect? obj) (begin
                          (set! object-count (+ object-count 1))
                          (let* ([obj-no object-count]
                                 [ir (IndirectReference obj-no 0)])
                            (cond
                              [(has-parent? (Indirect-object obj)) 
                               (set! object-list
                                     (cons (IndirectObject obj-no 0 (resolve-indirect (Indirect-object obj) parent))
                                           object-list))]
                              [else (set! object-list
                                     (cons (IndirectObject obj-no 0 (resolve-indirect (Indirect-object obj) ir))
                                           object-list))])
                            ir))]
    [else obj]))

#|
We could reorder the object list so it's in order (I'm not sure this is general
enough). Instead we can build a map with keys being the object no and values
being the byte length of the object
|#
(: object-map : (HashTable Nonnegative-Integer Integer) (Listof IndirectObject) -> (HashTable Nonnegative-Integer Integer))
(define (object-map om object-list)
  (if (empty? object-list)
      om
      (object-map
       (let ([object (car object-list)])
         (hash-set om (IndirectObject-obj-num object) (bytes-length (->bytes object)))) (cdr object-list))))

#|
compile-pdf. This is where all our hard work gets rewarded.
|#
(: compile-pdf : Catalog -> Bytes)
(define (compile-pdf cat)
  (set! object-list '())
  (set! object-count 0)
  (define root (resolve-indirect (Indirect cat) (PDFNull)))
  ;(define obj-map (object-map (ann (hash) (HashTable Nonnegative-Integer Integer)) object-list))
  
  (define heading (pdf-heading 1 7))
  (define heading-length (bytes-length heading))

  ; Convert our objects to bytes and calculate the byte-length of each object
  ; We need to reorder 
  (define object-bytes (map ->bytes object-list))
  (define object-byte-lengths (map bytes-length object-bytes))

  (define start-of-xref (foldl (λ ([l : Integer] [count : Integer])
                                 (+ l count))
                               0
                               (cons heading-length object-byte-lengths)))
  
  (define byte-length-sums (foldl (λ ([l : Integer] [ls : (Listof Integer)])
                                    (cons (+ l (car ls)) ls)) (list heading-length) object-byte-lengths))
  (define xrefs (map (λ ([x : Integer] [o : IndirectObject])
                       (cons (IndirectObject-obj-num o) x)) (reverse (cdr byte-length-sums)) object-list))

  (define xref-table (xref 0 (+ object-count 1) (map (λ ([x : (Pairof Nonnegative-Integer Integer)])
                                                       (make-entry (cdr x) 0 'n))
                                                     (sort xrefs (λ ([x1 : (Pairof Nonnegative-Integer Integer)]
                                                                     [x2 : (Pairof Nonnegative-Integer Integer)])
                                                                   (< (car x1) (car x2)))))))

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

