#lang racket

(provide (all-defined-out))

(define line-feed (bytes 10))

(define (number->bytes number)
  (string->bytes/utf-8 (number->string number)))

(define pdf-object-interface (interface () ->bytes))

(define name-object%
  (class* object% (pdf-object-interface)
    (init name)
    (define current-name name)
    (super-new)
    (define/public (->bytes)
      (define (process-name-string s)
        (string-replace s "#" "#23"))
      (bytes-append #"/" (string->bytes/utf-8 (process-name-string current-name))))))

(define literal-string-object%
  (class* object% (pdf-object-interface)
    (init string)
    (define current-string string)
    (super-new)
    (define/public (->bytes)
      (bytes-append #"(" (string->bytes/utf-8 current-string) #")"))))

(define number-object%
  (class* object% (pdf-object-interface)
    (init number)
    (define current-number number)
    (super-new)
    (define/public (->bytes)
     (number->bytes current-number))))

(define array-object%
  (class* object% (pdf-object-interface)
    (init array)
    (define current-array array)
    (super-new)
    (define/public (->bytes)
      (bytes-append (apply bytes-append #"[" (map (λ (o) (send o ->bytes)) current-array) #"]")))))

(define indirect-object%
  (class* object% (pdf-object-interface)
    (init object-number generation-number object)
    (define current-object-number object-number)
    (define current-generation-number generation-number)
    (define current-object object)
    (super-new)
    (define/public (->bytes)
      (bytes-append (number->bytes current-object-number)
                    #" "
                    (number->bytes current-generation-number)
                    #" obj" line-feed
                    (send current-object ->bytes) line-feed
                    #"endobj" line-feed))))

(define indirect-reference%
  (class* object% (pdf-object-interface)
    (init object-number generation-number)
    (define current-object-number object-number)
    (define current-generation-number generation-number)
    (super-new)
    (define/public (->bytes)
      (bytes-append (number->bytes current-object-number)
                    #" "
                    (number->bytes current-generation-number)
                    #" R"))))
    
;(define (hexadecimal-string hex-string)
;  (bytes-append #"<" hex-string #">"))

;(define (dictionary-object . dict)
 ; (bytes-append (apply bytes-append #"<< " (add-between dict line-feed)) #" >>" line-feed))

;(define (dictionary-object h)
;  (add-between (map obj->bytes (hash->list h)) line-feed)
