#lang setup/infotab

(define-collection 'multi)

(define deps '(("base" "6.0") "data-lib" "math-lib"))
(define build-deps '("rackunit-lib"))

(define pkg-desc "tests for graph-lib package ")
(define version "0.4.0")
