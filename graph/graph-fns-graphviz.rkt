#lang racket/base

(require racket/port racket/format racket/set racket/unsafe/ops)
(require "gen-graph.rkt" "graph-weighted.rkt")

(provide graphviz)

(define-syntax-rule (first x) (unsafe-car x))
(define-syntax-rule (second x) (unsafe-car (unsafe-cdr x)))

;; Return a graphviz definition for a graph
;; Pass a hash of vertex -> exact-nonnegative-integer? as coloring to color the nodes
(define (graphviz g #:colors [colors #f])
  (with-output-to-string
   (λ ()
    (define weighted? (weighted-graph? g))
    (printf "digraph G {\n")
    ; Add vertices, color them using evenly spaced HSV colors if given colors
    (define color-count (and colors (add1 (apply max (hash-values colors)))))
    (for ([v (in-vertices g)])
      (cond
        [(and color-count (hash-ref colors v #f))
         (printf "\t~a [color=\"~a 1.0 1.0\"];\n" v 
                 (~a #:max-width 5
                     (exact->inexact (/ (hash-ref colors v #f) color-count))))]
        [else
         (printf "\t~a;\n" v)]))
        
    ; Write undirected edges as one subgraph
    (printf "\tsubgraph U {\n")
    (printf "\t\tedge [dir=none];\n")
    (define undirected-edges
      (for/fold ([added (set)]) 
                ([e (in-edges g)]
                 #:when (and (not (set-member? added e))
                             (has-edge? g (second e) (first e))
                             (= (edge-weight g (first e) (second e))
                                (edge-weight g (second e) (first e)))))
        (printf "\t\t~a -> ~a~a;\n" 
          (first e) 
          (second e) 
          (if weighted? (format " [label=~a]" (edge-weight g (first e) (second e))) ""))
        (set-add (set-add added e) (list (second e) (first e)))))
    (printf "\t}\n")
        
    ; Write directed edges as another subgraph
    (printf "\tsubgraph D {\n")
    (for ([e (in-edges g)] #:unless (set-member? undirected-edges e))
      (printf "\t\t~a -> ~a~a;\n" 
        (first e) 
        (second e) 
        (if weighted? (format " [label=~a]" (edge-weight g (first e) (second e))) "")))
    (printf "\t}\n")
    
    (printf "}\n"))))
