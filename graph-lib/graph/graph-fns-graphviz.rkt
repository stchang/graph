#lang racket/base

(require racket/port racket/format racket/set racket/unsafe/ops racket/pretty)
(require "gen-graph.rkt" "graph-weighted.rkt")

(provide graphviz)

(define-syntax-rule (first x) (unsafe-car x))
(define-syntax-rule (second x) (unsafe-car (unsafe-cdr x)))

(define (sanatize-name name)
  (cond
    [(string? name) (with-output-to-string
                        (λ () (write name)))]
    [(symbol? name) (sanatize-name (symbol->string name))]
    [(number? name) (sanatize-name (number->string name))]
    [else (sanatize-name
           (with-output-to-string
               (λ () (pretty-print name))))]))

;; Return a graphviz definition for a graph
;; Pass a hash of vertex -> exact-nonnegative-integer? as coloring to color the nodes
(define (graphviz g
                  #:colors [colors #f]
                  #:output [port #f])
   (define (generate-graph)
     (parameterize ([current-output-port (or port (current-output-port))])
       (define weighted? (weighted-graph? g))
       (define node-count 0)
       (define node-id-table (make-hash))
       (define (node-id-table-ref! node)
         (hash-ref! node-id-table node
                    (λ ()
                      (begin0 (format "node~a" node-count)
                              (set! node-count (add1 node-count))))))
       (printf "digraph G {\n")
       ; Add vertices, color them using evenly spaced HSV colors if given colors
       (define color-count (and colors (add1 (apply max (hash-values colors)))))
    (for ([v (in-vertices g)])
      (cond
        [(and color-count (hash-ref colors v #f))
         (printf "\t~a [label=~a,color=\"~a 1.0 1.0\"];\n" (node-id-table-ref! v)
                 (sanatize-name v)
                 (~a #:max-width 5
                     (exact->inexact (/ (hash-ref colors v #f) color-count))))]
        [else
         (printf "\t~a [label=~a];\n"
                 (node-id-table-ref! v)
                 (sanatize-name v))]))
        
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
          (node-id-table-ref! (first e))
          (node-id-table-ref! (second e))
          (if weighted? (format " [label=\"~a\"]" (edge-weight g (first e) (second e))) ""))
        (set-add (set-add added e) (list (second e) (first e)))))
    (printf "\t}\n")
        
    ; Write directed edges as another subgraph
    (printf "\tsubgraph D {\n")
    (for ([e (in-edges g)] #:unless (set-member? undirected-edges e))
      (printf "\t\t~a -> ~a~a;\n" 
        (node-id-table-ref! (first e))
        (node-id-table-ref! (second e))
        (if weighted? (format " [label=\"~a\"]" (edge-weight g (first e) (second e))) "")))
    (printf "\t}\n")
    (printf "}\n")))
  (if port
      (generate-graph)
      (with-output-to-string generate-graph)))
