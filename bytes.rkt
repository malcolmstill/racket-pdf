#lang typed/racket

(require "object.rkt"
         "trailer.rkt"
         typed/rackunit)

(provide ->bytes)

#|
bytes.rkt provides functions for generating the byte representations
of PDF objects.
|#

(: line-feed Bytes)     
(define line-feed (bytes 10))

(: process-name-string (String -> Bytes))
(define (process-name-string s)
  (string->bytes/utf-8 (string-replace (string-replace s "#" "#23") " " "#20")))

(: number->bytes (Number -> Bytes))
(define (number->bytes number)
  (string->bytes/utf-8 (number->string number)))

(: dictionary->bytes (-> Dictionary Bytes))
(define (dictionary->bytes dict)
  (define filtered-elements (filter (λ ([pair : (Pairof Symbol PDFObject)])
                                      (not (PDFNull? (cdr pair))))
                                    dict))
  (bytes-append
   (apply bytes-append
          #"<< "
          (add-between
           (map (λ ([pair : (Pairof Symbol PDFObject)])
                  (bytes-append (->bytes (car pair))
                                #" "
                                (->bytes (cdr pair)))) filtered-elements)
           line-feed))
   #" >>" line-feed))

(: array->bytes : Array -> Bytes)
(define (array->bytes array)
  (bytes-append
   (apply bytes-append #"[" (add-between (map ->bytes array) #" ")) #"]"))

(: indirect-object->bytes : IndirectObject -> Bytes)
(define (indirect-object->bytes io)
  (match io
    [(IndirectObject obj-num gen-num object)
     (bytes-append (->bytes obj-num)
                   #" "
                   (->bytes gen-num)
                   #" obj" line-feed
                   (->bytes object) line-feed
                   #"endobj" line-feed)]))

(: indirect-reference->bytes : IndirectReference -> Bytes)
(define (indirect-reference->bytes ir)
  (match ir
    [(IndirectReference obj-num gen-num)
     (bytes-append (->bytes obj-num)
                   #" "
                   (->bytes gen-num)
                   #" R")]))

(: stream->bytes : Stream -> Bytes)
(define (stream->bytes stream)
  (bytes-append
   (dictionary->bytes (Stream-dict stream))
   #"stream" line-feed
   (Stream-data stream) line-feed
   #"endstream" line-feed))

(: ->bytes (PDFObject -> Bytes))
(define (->bytes obj)
  (cond
    [(string? obj) (bytes-append #"(" (string->bytes/utf-8 obj) #")")]
    [(number? obj) (number->bytes obj)]
    [(symbol? obj) (bytes-append #"/" (process-name-string (symbol->string obj)))]
    [(PDFNull? obj) #"null"]
    [(null? obj) #"[]"] ; empty list
    [(Dictionary? obj) (dictionary->bytes obj)]
    [(list? obj) (array->bytes obj)]
    [(IndirectObject? obj) (indirect-object->bytes obj)]
    [(IndirectReference? obj) (indirect-reference->bytes obj)]
    [(Stream? obj) (stream->bytes obj)]
    ;[(Trailer? obj) (trailer->bytes obj)]
    [(false? obj) #"false"]
    [#t #"true"]))

(: trailer->bytes : Trailer -> Bytes)
(define (trailer->bytes trailer)
  (bytes-append
   #"trailer" line-feed
   (dictionary->bytes (Trailer-dict trailer))
   #"startxref" line-feed
   (number->bytes (Trailer-offset trailer)) line-feed
   #"%%EOF"))

(module+ test
  (check-equal? (->bytes 'Name1) #"/Name1")
  (check-equal? #"/ASomewhatLongerName" (->bytes 'ASomewhatLongerName))
  (check-equal? #"/A;Name_With-Various***Characters?" (->bytes '|A;Name_With-Various***Characters?|))
  (check-equal? #"/1.2" (->bytes '|1.2|))
  (check-equal? #"/$$" (->bytes '$$))
  (check-equal? #"/@pattern" (->bytes '@pattern))
  (check-equal? #"/.notdef" (->bytes '.notdef))
  (check-equal? #"/Lime#20Green" (->bytes '|Lime Green|))
  (check-equal? #"/paired()parentheses" (->bytes '|paired()parentheses|))
  (check-equal? #"/The_Key_of_F#23_Minor" (->bytes 'The_Key_of_F#_Minor)))