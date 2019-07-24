#lang setup/infotab

(define collection 'multi)

(define deps '(("base" "6.0") "graph-lib" "graph-doc" "graph-test"))
(define implies '("graph-lib" "graph-doc" "graph-test"))

(define pkg-desc "A meta-package for graph-lib, graph-doc, and graph-test.")

(define pkg-authors '(stchang))

(define version "0.4.0")

