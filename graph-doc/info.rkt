#lang setup/infotab

(define collection 'multi)

(define deps '(("base" "6.0")))

(define build-deps
  '("graph-lib"
    "racket-doc"
    "scribble-lib"))

(define update-implies '("graph-lib"))

(define pkg-desc "documentation part of \"graph\"")

(define pkg-authors '(stchang))

(define version "0.4.0")
