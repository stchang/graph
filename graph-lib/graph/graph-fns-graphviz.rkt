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
;; Parameters:
;; #:colors: hashof vertex -> exact-nonnegative-integer? as coloring to color the nodes
;; - each color int c will be used as evenly-spaced H in HSV color format as follows:
;;   (/ c (add1 (apply max (hash-values colors))))
;; #:hsv-colors: hashof vertex -> (list H S V) or #f where H,S,V are nums in range [0,1]
;; - this color is directly used as HSV color in the generated graph
;; #:mk-label: fn that converts vertices into a string label
;; #:ouput: Output graph will be written to the port, if one is given
;; #:rankdir: corresponds to graphviz rankdir parameter, default = "TB"
;; - https://graphviz.org/docs/attrs/rankdir/
;; #:dirs: corresponds to graphviz dirType
;; - if a single string is given, then that is used for all edges
;; - if a hash from edge to string is given, then each edge has its own dir
;; - if #f, then try to infer the correct direction
;; #:indent: amount of space to use for one indent level
(define (graphviz g
                  #:colors [colors #f]
                  #:hsv-colors [hsv-colors #f]
                  #:mk-label [mk-label (lambda (x) x)]
                  #:output [port #f]
                  #:rankdir [rankdir "TB"]
                  #:dirs [dirs #f]
                  #:indent [indent "\t"])
  (define node-count 0)
      (define node-id-table (make-hash))
  (define (node-id-table-ref! node)
    (hash-ref! node-id-table node
               (λ ()
                 (begin0 (format "node~a" node-count)
                   (set! node-count (add1 node-count))))))

  (define weighted? (weighted-graph? g))

  (define (print-edge e #:dir [dir #f] #:label [label #f] #:indent [indent indent])
    (display indent)

    ;; print nodes
    (printf "~a -> ~a"
            (node-id-table-ref! (first e))
            (node-id-table-ref! (second e)))

    ;; print attrs
    (when (or weighted? label)
      (printf " [label=\"~a\"]"
              (if weighted?
                  (edge-weight g (first e) (second e))
                  label)))

    (when dir (printf " [dir=~a]" dir))

    (printf ";\n"))
            
  (define (generate-graph)
    (parameterize ([current-output-port (or port (current-output-port))])
      (printf "digraph G {\n")

      (display indent)
      (printf "rankdir=~v;\n\n" rankdir)

      ;; print vertices -------------------------------------------------------
      ; Add vertices, color them using evenly spaced HSV colors if given colors
      (define color-count (and colors (add1 (apply max (hash-values colors)))))

      (for ([v (in-vertices g)])
        (display indent)
        ;; print node name
        (printf "~a " (node-id-table-ref! v))

        ;; print attrs
        (printf "[")

        ;; label
        (printf "label=~a" (sanatize-name (mk-label v)))

        ;; colors
        (cond
          [hsv-colors
           (define hsv (hash-ref hsv-colors v #f))
           (when hsv
             (apply printf ",color=\"~a ~a ~a\"" hsv))]
          [(and color-count (hash-ref colors v #f))
           (printf ",color=\"~a 1.0 1.0\""
                   (~a #:max-width 5
                       (exact->inexact (/ (hash-ref colors v #f) color-count))))])

        (printf "]")

        (printf ";\n"))

      ;; print edges -------------------------------------------------------
      (printf "\n")
      (cond
        [(hash? dirs)
         (for ([e (in-edges g)])
           (print-edge e #:dir (hash-ref dirs e #f) #:indent indent))]
        [else
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
             (print-edge e #:indent (string-append indent indent))
             (set-add (set-add added e) (list (second e) (first e)))))
         (printf "\t}\n")
        
         ; Write directed edges as another subgraph
         (printf "\tsubgraph D {\n")
         (for ([e (in-edges g)] #:unless (set-member? undirected-edges e))
           (print-edge e #:indent (string-append indent indent)))
         (printf "\t}\n")])
      
      (printf "}\n")))

  (if port
      (generate-graph)
      (with-output-to-string generate-graph)))
