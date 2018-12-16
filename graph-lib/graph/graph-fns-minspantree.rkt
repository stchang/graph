#lang racket/base

;; deprecated: see graph-fns-spantree.rkt

(provide mst-kruskal mst-prim)

(require (only-in "graph-fns-spantree.rkt"
                  [min-st-prim mst-prim]
                  [min-st-kruskal mst-kruskal]))
