#lang racket/base
(require graph graph/graph-unweighted rackunit)

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
  (check-true (string-contains? viz-str "label=\"1\""))
  (check-true (string-contains? viz-str "label=\"2\""))
  (check-true (string-contains? viz-str "label=\"3\""))
  (check-true (string-contains? viz-str "label=\"4\""))
  (check-equal? (length (regexp-match* #rx"->" viz-str))
                6))
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

(define fruit-labeled
  (weighted-graph/directed '(["tastier" "tomato" "potato"]
                             [10 "apple" "tomato"]
                             [11 "apple" "potato"]
                             [better "banana" "apple" ])))
(check-equal? (edge-weight fruit-labeled "tomato" "potato") "tastier")
(check-equal? (edge-weight fruit-labeled "apple" "tomato") 10)
(check-equal? (edge-weight fruit-labeled "apple" "potato") 11)
(check-equal? (edge-weight fruit-labeled "banana" "apple") 'better)

(define fruit-labeled-viz (graphviz fruit-labeled))
;; eg,
;; digraph G {
;; 	node0 [label="banana"];
;; 	node1 [label="tomato"];
;; 	node2 [label="apple"];
;; 	node3 [label="potato"];
;; 	subgraph U {
;; 		edge [dir=none];
;; 	}
;; 	subgraph D {
;; 		node0 -> node2 [label="better"];
;; 		node1 -> node3 [label="tastier"];
;; 		node2 -> node1 [label="10"];
;; 		node2 -> node3 [label="11"];
;; 	}
;; }
(check-true (string-contains? fruit-labeled-viz "\\[label=\"better\"\\]"))
(check-true (string-contains? fruit-labeled-viz "\\[label=\"tastier\"\\]"))
(check-true (string-contains? fruit-labeled-viz "\\[label=\"10\"\\]"))
(check-true (string-contains? fruit-labeled-viz "\\[label=\"11\"\\]"))

(define cities-labeled
  (weighted-graph/undirected '([far "Berlin" "New York"]
                               ["near" "New York" "Boston"]
                               [1000 "Berlin" "Boston"]
                               ["very far" "New York" "Sydney"])))
(define (check-undirected-edge-label-equal? gr u v label)
  (check-equal? (edge-weight gr u v) label)
  (check-equal? (edge-weight gr v u) label))
(check-undirected-edge-label-equal? cities-labeled "Berlin" "New York" 'far)
(check-undirected-edge-label-equal? cities-labeled "New York" "Boston" "near")
(check-undirected-edge-label-equal? cities-labeled "Berlin" "Boston" 1000)
(check-undirected-edge-label-equal? cities-labeled "New York" "Sydney" "very far")

(define cities-labeled-viz (graphviz cities-labeled))
;; eg,
;; digraph G {
;; 	node0 [label="Sydney"];
;; 	node1 [label="Berlin"];
;; 	node2 [label="New York"];
;; 	node3 [label="Boston"];
;; 	subgraph U {
;; 		edge [dir=none];
;; 		node0 -> node2 [label="very far"];
;; 		node1 -> node3 [label="1000"];
;; 		node1 -> node2 [label="far"];
;; 		node2 -> node3 [label="near"];
;; 	}
;; 	subgraph D {
;; 	}
;; }
(check-true (string-contains? cities-labeled-viz "\\[label=\"very far\"\\]"))
(check-true (string-contains? cities-labeled-viz "\\[label=\"1000\"\\]"))
(check-true (string-contains? cities-labeled-viz "\\[label=\"far\"\\]"))
(check-true (string-contains? cities-labeled-viz "\\[label=\"near\"\\]"))
