#lang racket/base
(require "../main.rkt" "../graph-unweighted.rkt")
(require rackunit)

(define (string-contains? input s) (regexp-match? s input))

(define butterfly 
  (mk-unweighted-graph/undirected '((1 2) (2 3) (1 3) (3 4) (3 5) (4 5))))

(define butterfly-viz (graphviz butterfly))
;; eg,
;; digraph G {
;; 	5;
;; 	1;
;; 	2;
;; 	3;
;; 	4;
;; 	subgraph U {
;; 		edge [dir=none];
;; 		5 -> 3;
;; 		5 -> 4;
;; 		1 -> 3;
;; 		1 -> 2;
;; 		2 -> 3;
;; 		3 -> 4;
;; 	}
;; 	subgraph D {
;; 	}
;; }
(define (check-butterfly-viz-output viz-str)
  (check-true (or (string-contains? viz-str "1 -> 2")
                  (string-contains? viz-str "2 -> 1")))
  (check-true (or (string-contains? viz-str "1 -> 3")
                  (string-contains? viz-str "3 -> 1")))
  (check-true (or (string-contains? viz-str "2 -> 3")
                  (string-contains? viz-str "3 -> 2")))
  (check-true (or (string-contains? viz-str "3 -> 4")
                  (string-contains? viz-str "4 -> 3")))
  (check-true (or (string-contains? viz-str "3 -> 5")
                  (string-contains? viz-str "5 -> 3")))
  (check-true (or (string-contains? viz-str "4 -> 5")
                  (string-contains? viz-str "5 -> 4"))))
(check-butterfly-viz-output butterfly-viz)

(define-values (buttr-num-colors buttr-coloring) (coloring/greedy butterfly))

(define bf-viz-color (graphviz butterfly #:colors buttr-coloring))
;; eg,
;; digraph G {
;; 	5 [color="0.333 1.0 1.0"];
;; 	1 [color="0.666 1.0 1.0"];
;; 	2 [color="0.333 1.0 1.0"];
;; 	3 [color="0.0 1.0 1.0"];
;; 	4 [color="0.666 1.0 1.0"];
;; 	subgraph U {
;; 		edge [dir=none];
;; 		5 -> 3;
;; 		5 -> 4;
;; 		1 -> 3;
;; 		1 -> 2;
;; 		2 -> 3;
;; 		3 -> 4;
;; 	}
;; 	subgraph D {
;; 	}
;; }
(check-true (string-contains? bf-viz-color "[color=\"0.0 1.0 1.0\"]"))
(check-true (string-contains? bf-viz-color "[color=\"0.333 1.0 1.0\"]"))
(check-true (string-contains? bf-viz-color "[color=\"0.666 1.0 1.0\"]"))
(check-true (string-contains? bf-viz-color "[color=\"0.0 1.0 1.0\"]"))
(check-true (string-contains? bf-viz-color "[color=\"0.333 1.0 1.0\"]"))
(check-butterfly-viz-output bf-viz-color)

(define bf-viz-color-brelaz 
  (graphviz butterfly #:colors (coloring/brelaz butterfly)))
(check-true (string-contains? bf-viz-color-brelaz "[color=\"0.333 1.0 1.0\"]"))
(check-true (string-contains? bf-viz-color-brelaz "[color=\"0.666 1.0 1.0\"]"))
(check-true (string-contains? bf-viz-color-brelaz "[color=\"0.0 1.0 1.0\"]"))
(check-true (string-contains? bf-viz-color-brelaz "[color=\"0.333 1.0 1.0\"]"))
(check-true (string-contains? bf-viz-color-brelaz "[color=\"0.666 1.0 1.0\"]"))
(check-butterfly-viz-output bf-viz-color-brelaz)
