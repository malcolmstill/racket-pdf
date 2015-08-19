#lang racket

(require "object.rkt")

(define (encrypt-dictionary filter
                            #:sub-filter [sub-filter #f]
                            #:v [v #f]
                            #:length [length #f]
                            #:cf [cf #f]
                            #:stmf [stmf #f]
                            #:strf [strf #f]
                            #:eff [eff #f])
  (dictionary
   'Filter filter
   'SubFilter sub-filter
   'V v
   'Length length
   'CF cf
   'StmF stmf
   'StrF strf
   'EFF eff))
