#lang racket
(require "../main.rkt" "../graph-unweighted.rkt")
(require rackunit)

(define butterfly 
  (mk-unweighted-graph/undirected '((1 2) (2 3) (1 3) (3 4) (3 5) (4 5))))

(check-equal?
 (graphviz butterfly)
 "digraph G {\n\t1;\n\t2;\n\t3;\n\t4;\n\t5;\n\tsubgraph U {\n\t\tedge [dir=none];\n\t\t1 -> 2;\n\t\t1 -> 3;\n\t\t2 -> 3;\n\t\t3 -> 4;\n\t\t3 -> 5;\n\t\t4 -> 5;\n\t}\n\tsubgraph D {\n\t}\n}\n")

(define-values (buttr-num-colors buttr-coloring) (coloring/greedy butterfly))

(check-equal?
 (graphviz butterfly #:colors buttr-coloring)
 "digraph G {\n\t1 [color=\"0.0 1.0 1.0\"];\n\t2 [color=\"0.333 1.0 1.0\"];\n\t3 [color=\"0.666 1.0 1.0\"];\n\t4 [color=\"0.0 1.0 1.0\"];\n\t5 [color=\"0.333 1.0 1.0\"];\n\tsubgraph U {\n\t\tedge [dir=none];\n\t\t1 -> 2;\n\t\t1 -> 3;\n\t\t2 -> 3;\n\t\t3 -> 4;\n\t\t3 -> 5;\n\t\t4 -> 5;\n\t}\n\tsubgraph D {\n\t}\n}\n")

(check-equal?
 (graphviz butterfly #:colors (coloring/brelaz butterfly))
 "digraph G {\n\t1 [color=\"0.333 1.0 1.0\"];\n\t2 [color=\"0.666 1.0 1.0\"];\n\t3 [color=\"0.0 1.0 1.0\"];\n\t4 [color=\"0.333 1.0 1.0\"];\n\t5 [color=\"0.666 1.0 1.0\"];\n\tsubgraph U {\n\t\tedge [dir=none];\n\t\t1 -> 2;\n\t\t1 -> 3;\n\t\t2 -> 3;\n\t\t3 -> 4;\n\t\t3 -> 5;\n\t\t4 -> 5;\n\t}\n\tsubgraph D {\n\t}\n}\n")