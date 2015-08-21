#lang typed/racket

(require "object.rkt")

#|
Do we need a name tree type?

Or do we just have a function that takes a
|#


(define-type Tree (U leaf node))
(struct leaf ([val : Number]))
(struct node ([left : Tree] [right : Tree]))