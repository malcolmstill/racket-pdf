#lang typed/racket

(require "object.rkt"
         "versions.rkt")

(provide (all-defined-out))

(define-type ExtensionsDictionary (List (Pairof 'Type 'DeveloperExtensions)
                                        (Pairof 'BaseVersion Version)
                                        (Pairof 'ExtensionLevel Integer)))

(: extensions : Version Integer -> ExtensionsDictionary)
(define (extensions version level)
  (dictionary
   'Type 'DeveloperExtensions
   'BaseVersion version
   'ExtensionLevel level))