#lang setup/infotab

(define pkg-desc "documentation part of \"graph\"")
(define version "0.3.1")

(define collection 'multi)

(define deps '(("racket" "5.3.2") "base"))
(define build-deps
  '("graph-lib" "math-lib" "rackunit-lib" "racket-doc" "scribble-lib" "data-doc" "math-doc"))

(define update-implies '("graph-lib"))
