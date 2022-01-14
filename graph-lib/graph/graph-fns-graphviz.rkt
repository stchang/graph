#lang racket/base

(require racket/format
         (only-in racket/list
                  remove-duplicates)
         racket/port
         racket/pretty
         racket/set
         racket/string
         racket/unsafe/ops
         "gen-graph.rkt"
         "graph-weighted.rkt")

(provide graphviz)

(define-syntax-rule (first x) (unsafe-car x))
(define-syntax-rule (second x) (unsafe-car (unsafe-cdr x)))

(define (sanitize-name name)
  (cond
    [(string? name) name]
    [(symbol? name) (symbol->string name)]
    [(number? name) (number->string name)]
    [else (pretty-format name)]))

(define (color-attr colors color-count v)
  (if (and color-count (hash-ref colors v #f))
      (let* ([percent (/ (hash-ref colors v #f) color-count)]
             [str (~a #:max-width 5 (exact->inexact percent))]
             [triple (format "~a 1.0 1.0" str)])
        `([color ,triple]))
      null))

(define (weight-attr weighted? g e)
  (if weighted?
      (let ([weight (edge-weight g (first e) (second e))])
        `([label ,(sanitize-name weight)]))
      null))

(define (vertex-attrs-get-val attrs v)
  (for/list ([attr (in-list attrs)])
    (define get (cadr attr))
    (list (car attr) (get v))))

(define (edge-attrs-get-val attrs e)
  (for/list ([attr (in-list attrs)])
    (define get (cadr attr))
    (list (car attr) (get (first e) (second e)))))

(define (attrs->string attrs)
  (define attrs* (remove-duplicates attrs #:key car))
  (define attr-strs
    (for/list ([attr (in-list attrs*)])
      (format "~a=~s" (car attr) (cadr attr))))
  (if (null? attrs)
      ""
      (format "[~a]" (string-join attr-strs ","))))

;; Return a graphviz definition for a graph
;; Pass a hash of vertex -> exact-nonnegative-integer? as coloring to color the nodes
(define (graphviz g
                  #:colors [colors #f]
                  #:edge-attributes [edge-attrs null]
                  #:vertex-attributes [vertex-attrs null]
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
        (define attrs
          (append (vertex-attrs-get-val vertex-attrs v)
                  `([label ,(sanitize-name v)])
                  (color-attr colors color-count v)))
        (printf "\t~a ~a;\n"
                (node-id-table-ref! v)
                (attrs->string attrs)))

      ; Write undirected edges as one subgraph
      (printf "\tsubgraph U {\n")
      (printf "\t\tedge [dir=none];\n")
      (define undirected-edges
        (for/fold ([added (set)])
                  ([e (in-edges g)]
                   #:when (and (not (set-member? added e))
                               (has-edge? g (second e) (first e))
                               (equal? (edge-weight g (first e) (second e))
                                       (edge-weight g (second e) (first e)))))
          (define attrs
            (append (edge-attrs-get-val edge-attrs e)
                    (weight-attr weighted? g e)))
          (printf "\t\t~a -> ~a ~a;\n"
                  (node-id-table-ref! (first e))
                  (node-id-table-ref! (second e))
                  (attrs->string attrs))
          (set-add (set-add added e) (list (second e) (first e)))))
      (printf "\t}\n")

      ; Write directed edges as another subgraph
      (printf "\tsubgraph D {\n")
      (for ([e (in-edges g)] #:unless (set-member? undirected-edges e))
        (define attrs
          (append (edge-attrs-get-val edge-attrs e)
                  (weight-attr weighted? g e)))
        (printf "\t\t~a -> ~a ~a;\n"
                (node-id-table-ref! (first e))
                (node-id-table-ref! (second e))
                (attrs->string attrs)))
      (printf "\t}\n")
      (printf "}\n")))
  (if port
      (generate-graph)
      (with-output-to-string generate-graph)))
