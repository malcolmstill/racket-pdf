#lang typed/racket

(require "object.rkt"
         "versions.rkt")

(provide extensions)

(: extensions : Version Integer -> ExtensionsDictionary)
(define (extensions version level)
  (dictionary
   'Type 'DeveloperExtensions
   'BaseVersion version
   'ExtensionLevel level))