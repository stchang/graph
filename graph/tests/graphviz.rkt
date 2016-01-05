#lang racket
(require "../main.rkt" "../graph-unweighted.rkt")
(require rackunit)

(define butterfly 
  (mk-unweighted-graph/undirected '((1 2) (2 3) (1 3) (3 4) (3 5) (4 5))))

(define butterfly-viz (graphviz butterfly))
 ;"digraph G {\n\t1;\n\t2;\n\t3;\n\t4;\n\t5;\n\tsubgraph U {\n\t\tedge [dir=none];\n\t\t1 -> 2;\n\t\t1 -> 3;\n\t\t2 -> 3;\n\t\t3 -> 4;\n\t\t3 -> 5;\n\t\t4 -> 5;\n\t}\n\tsubgraph D {\n\t}\n}\n"
(check-true (string-contains? butterfly-viz "1 -> 2"))
(check-true (string-contains? butterfly-viz "1 -> 3"))
(check-true (string-contains? butterfly-viz "2 -> 3"))
(check-true (string-contains? butterfly-viz "3 -> 4"))
(check-true (string-contains? butterfly-viz "3 -> 5"))
(check-true (string-contains? butterfly-viz "4 -> 5"))

(define-values (buttr-num-colors buttr-coloring) (coloring/greedy butterfly))

(define bf-viz-color (graphviz butterfly #:colors buttr-coloring))
(check-true (string-contains? bf-viz-color "1 [color=\"0.0 1.0 1.0\"]"))
(check-true (string-contains? bf-viz-color "2 [color=\"0.333 1.0 1.0\"]"))
(check-true (string-contains? bf-viz-color "3 [color=\"0.666 1.0 1.0\"]"))
(check-true (string-contains? bf-viz-color "4 [color=\"0.0 1.0 1.0\"]"))
(check-true (string-contains? bf-viz-color "5 [color=\"0.333 1.0 1.0\"]"))
(check-true (string-contains? bf-viz-color "1 -> 2"))
(check-true (string-contains? bf-viz-color "1 -> 3"))
(check-true (string-contains? bf-viz-color "2 -> 3"))
(check-true (string-contains? bf-viz-color "3 -> 4"))
(check-true (string-contains? bf-viz-color "3 -> 5"))
(check-true (string-contains? bf-viz-color "4 -> 5"))

#;(check-equal?
 (graphviz butterfly #:colors buttr-coloring)
 "digraph G {\n\t1 [color=\"0.0 1.0 1.0\"];\n\t2 [color=\"0.333 1.0 1.0\"];\n\t3 [color=\"0.666 1.0 1.0\"];\n\t4 [color=\"0.0 1.0 1.0\"];\n\t5 [color=\"0.333 1.0 1.0\"];\n\tsubgraph U {\n\t\tedge [dir=none];\n\t\t1 -> 2;\n\t\t1 -> 3;\n\t\t2 -> 3;\n\t\t3 -> 4;\n\t\t3 -> 5;\n\t\t4 -> 5;\n\t}\n\tsubgraph D {\n\t}\n}\n")

(define bf-viz-color-brelaz (graphviz butterfly #:colors (coloring/brelaz butterfly)))
(check-true (string-contains? bf-viz-color-brelaz "1 [color=\"0.333 1.0 1.0\"]"))
(check-true (string-contains? bf-viz-color-brelaz "2 [color=\"0.666 1.0 1.0\"]"))
(check-true (string-contains? bf-viz-color-brelaz "3 [color=\"0.0 1.0 1.0\"]"))
(check-true (string-contains? bf-viz-color-brelaz "4 [color=\"0.333 1.0 1.0\"]"))
(check-true (string-contains? bf-viz-color-brelaz "5 [color=\"0.666 1.0 1.0\"]"))
(check-true (string-contains? bf-viz-color-brelaz "1 -> 2"))
(check-true (string-contains? bf-viz-color-brelaz "1 -> 3"))
(check-true (string-contains? bf-viz-color-brelaz "2 -> 3"))
(check-true (string-contains? bf-viz-color-brelaz "3 -> 4"))
(check-true (string-contains? bf-viz-color-brelaz "3 -> 5"))
(check-true (string-contains? bf-viz-color-brelaz "4 -> 5"))

#;(check-equal?
 (graphviz butterfly #:colors (coloring/brelaz butterfly))
 "digraph G {\n\t1 [color=\"0.333 1.0 1.0\"];\n\t2 [color=\"0.666 1.0 1.0\"];\n\t3 [color=\"0.0 1.0 1.0\"];\n\t4 [color=\"0.333 1.0 1.0\"];\n\t5 [color=\"0.666 1.0 1.0\"];\n\tsubgraph U {\n\t\tedge [dir=none];\n\t\t1 -> 2;\n\t\t1 -> 3;\n\t\t2 -> 3;\n\t\t3 -> 4;\n\t\t3 -> 5;\n\t\t4 -> 5;\n\t}\n\tsubgraph D {\n\t}\n}\n")