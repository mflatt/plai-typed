#lang setup/infotab

(define collection 'multi)

(define deps '("base"
               "plai"))
(define build-deps '("racket-doc"
                     "rackunit-lib"
                     "scheme-doc"
                     ["scribble-lib" #:version "1.16"]))

(define version "1.2")
