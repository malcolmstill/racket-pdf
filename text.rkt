#lang typed/racket

(require "object.rkt"
         "bytes.rkt")

(provide text->stream
         text->paragraph)

(: text->stream : Symbol Real Real Real Real (U String Array) -> Stream)
(define (text->stream font size leading x y string)
  (stream 
   (bytes-append
    #"BT " (->bytes font) #" " (->bytes size) #" Tf " (->bytes x) #" " (->bytes y) #" Td " (->bytes leading) #" TL " (->bytes string) (cond
                                                                                                                                       [(string? string) #" Tj "]
                                                                                                                                       [else #" TJ "]) #"ET")))

(: split-paragraph : String Real -> (Listof (Listof String)))
(define (split-paragraph string width)
  (reverse (foldl (λ ([word : String]
                      [lines : (Listof (Listof String))])
                    (if (empty? lines)
                        (cons (list word) lines)
                        (if (> (apply + (string-length word) (map string-length (car lines))) width)
                            (cons (list word) (cons (car lines) (cdr lines)))
                            (cons (reverse (cons word (reverse (car lines)))) (cdr lines)))))
                  '() (string-split string))))

#|
(: flatten-lines : (Listof (U Bytes (Listof Bytes))) -> Bytes)
(define (flatten-lines list)
  (apply bytes-append
         (reverse (foldl (λ ([line/break : (U Bytes (Listof Bytes))]
                             [lines : (Listof Bytes)])
                           (cond
                             [(bytes? line/break) (cons line/break lines)]
                             [else (append (reverse line/break) lines)])) '() list))))
|#

(: text->paragraph : Symbol Real Real Real Real Real String -> Stream)
(define (text->paragraph font size width leading x y string)
  (stream
   (bytes-append
     #"BT " (->bytes font) #" "
     (->bytes size) #" Tf "
     (->bytes x) #" " (->bytes y) #" Td "
     (->bytes leading) #" TL "
     (->bytes (add-between (map (λ ([line : (Listof String)])
                                  (->bytes (apply string-append (add-between line " "))))
                                (split-paragraph string width)) #" ' "))
     (cond
       [(string? string) #" Tj "]
       [else #" TJ "]) #"ET")))