#lang setup/infotab

(define collection 'multi)

(define deps '(("base" "6.0")))

(define build-deps
  '(("graph-lib" "0.5.1")
    "racket-doc"
    "math-doc" "math-lib"
    "scribble-lib"))

(define update-implies '("graph-lib"))

(define pkg-desc "documentation part of \"graph\"")

(define pkg-authors '(stchang))

(define version "0.5.1")
