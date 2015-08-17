#lang racket

(require racket/string
         racket/list
         "utils.rkt")

(provide (all-defined-out))

(define line-feed (bytes 10))

(define-syntax (define-bytes stx)
  (syntax-case stx ()
    [(_ (name args ... . else) entries ...)
     #'(define (name args ... . else)
         (bytes-append entries ...))]))

(define (number->bytes number)
  (string->bytes/utf-8 (number->string number)))

(define (string->bytes str)
  (bytes-append #"(" (string->bytes/utf-8 str) #")"))

(define pdf-object-interface (interface () ->bytes))

(define (any->bytes x)
  (cond
    [(number? x) (number->bytes x)]
    [(string? x) (string->bytes x)]
    [(bytes? x) x]
    [(object? x) (send x ->bytes)]))

(define name-object%
  (class* object% (pdf-object-interface)
    (init name)
    (define current-name name)
    (super-new)
    (define/public (->bytes)
      (define (process-name-string s)
        (string-replace s "#" "#23"))
      (bytes-append #"/" (string->bytes/utf-8 (process-name-string current-name))))))

(define (name-object str)
  (when (not (string? str))
    (error 'name-object "name must be string"))
  (new name-object% [name str]))

#|
Do we need a literal-string-object or just convert
Racket strings to the appropriate bytes?
|#
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
      (bytes-append (apply bytes-append #"[" (map any->bytes current-array) #"]")))))

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
                    (any->bytes current-object) line-feed
                    #"endobj" line-feed))))

(define (indirect-object object-number generation-number object)
  (new indirect-object% [object-number object-number]
       [generation-number generation-number]
       [object object]))

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

(define (indirect-reference object-number generation-number)
  (new indirect-reference% [object-number object-number]
       [generation-number generation-number]))

(define dictionary%
  (class* object% (pdf-object-interface)
    (init items)
    (define current-items items)
    (super-new)
    (define/public (->bytes)
      (bytes-append
       (apply bytes-append
              #"<< "
              (add-between
               (map (λ (p)
                      (bytes-append (any->bytes (car p))
                                    #" "
                                    (any->bytes (cdr p))))
                    current-items)
               line-feed))
       #" >>" line-feed))))

(define (dictionary . items)
  (when (or (zero? (length items)) (odd? (length items)))
    (error 'dictionary-object
           "failed because number of entries must be non-zero and even"))
  (new dictionary% [items (map (λ (x)
                                 (match x
                                   [(cons name obj) (cons (name-object name) obj)]))
                               (filter cdr (pair-off items)))]))
  
(define stream%
  (class* object% (pdf-object-interface)
    (init dict stream)
    (define current-dict dict)
    (define current-stream stream)
    (super-new)

    (define/public (->bytes)
      (bytes-append
       (send current-dict ->bytes)
       #"stream" line-feed
       current-stream line-feed
       #"endstream" line-feed))))

(define (stream-object length
		       #:filter [filter #f]
		       #:decode-parms [decode-parms #f]
		       #:f [f #f]
		       #:f-filter [f-filter #f]
		       #:f-decode-parms [f-decode-parms #f]
		       #:dl [dl #f]
		       bytes)
  (new stream%
       [dict (dictionary
              "Length" length
              "Filter" (name-object filter)
              "DecodeParms" decode-parms
              "F" f
              "FFilter" f-filter
              "FDecodeParms" f-decode-parms
              "DL" dl)]
       [stream bytes]))
