#lang racket

(require "graph-property.rkt"
         "utils.rkt"
         "gen-graph.rkt"
         "../queue/gen-queue.rkt"
         (only-in "../queue/fifo.rkt" mk-empty-fifo))

(require (for-syntax syntax/parse syntax/parse/experimental/template)
         racket/stxparam)

(provide (all-defined-out))

;; ----------------------------------------------------------------------------
;; bfs and dfs

;; bfs : Graph Vertex -> [Hashof Vertex -> Number] [Hashof Vertex -> Vertex]
;; s is the source vertex
;; returns 2 values: hash mapping vertices to distances and
;;  hash mapping vertices to predecessor
(define (bfs G s)
  (define-vertex-property G d #:init +inf.0)
  (define-vertex-property G π #:init #f)

  (do-bfs G s 
    #:init (d-set! s 0) (π-set! s #f)
    #:discover (d-set! $to (add1 (d $from))) (π-set! $to $from)
    #:return (values (d->hash) (π->hash))))

;; default Q is from data/queue
;; see also bfs clients prim and dijkstra
(define (bfs/generalized G s #:init-queue [Q (mk-empty-fifo)]
                             #:break [break? (λ _ #f)]
                             #:init [init void]
                             #:visit? [custom-visit?-fn #f]
                             #:discover [discover void]
                             #:visit [visit void]
                             #:return [finish void])
  ; v ∈ visited means v has been seen and enqueued, ie it's no longer "white"
  (define discovered (set)) 
  (define (mark-discovered! v) (set! discovered (set-add discovered v)))
  (define visit? (or custom-visit?-fn 
                     (λ (G s u v) (not (set-member? discovered v)))))
  
  (init G s)
  (enqueue! Q s) ; source vertex s is always visited
  (mark-discovered! s)
  (for ([u (in-queue Q)])
    (visit G s u)
    (for ([v (in-neighbors G u)] #:when (visit? G s u v) #:break (break?))
      (mark-discovered! v)
      (discover G s u v)
      (enqueue! Q v)))
  (finish G s))

;; TODO: there is potential ambiguity here in the binding clauses
;; If the programmer omits the bindings, but the first expr happens to have the
;; form (id1 id2), then there will be a syntax error for $to and $from bc
;; they won't be bound.
;;
;; cleaner syntax for bfs/generalized
(define-syntax (do-bfs stx)
  (syntax-parse stx 
    [(_ G s 
      (~or (~optional (~seq #:init-queue Q:expr))
           (~optional (~seq #:break break?:expr))
           (~optional (~seq #:init init:expr ...))
           (~or (~optional 
                 (~seq #:visit? (v?-from:id v?-to:id) visit?:expr v?rst:expr ...))
                (~optional 
                 (~seq #:visit? v?exp:expr ...)))
           (~or (~optional 
                 (~seq #:discover (disc-from:id disc-to:id) disc:expr discrst:expr ...))
                (~optional 
                 (~seq #:discover discexp:expr ...)))
           (~or (~optional (~seq #:visit (v:id) visit:expr visitrst:expr ...))
                (~optional (~seq #:visit visexp:expr ...)))
           (~optional (~seq #:return return:expr ...))) ...)
     (template
      (bfs/generalized G s 
       (?? (?@ #:init-queue Q))
       (?? (?@ #:break break?))
       (?? (?@ #:init (λ _ init ...)))
       (?? (?@ #:visit? (λ (G s v?-from v?-to) visit? v?rst ...)))
       (?? (?@ #:visit? 
               (λ (G s from to)
                 (syntax-parameterize ([$from (syntax-id-rules () [_ from])]
                                       [$to (syntax-id-rules () [_ to])])
                   v?exp ...))))
       (?? (?@ #:discover (λ (G s disc-from disc-to) disc discrst ...)))
       (?? (?@ #:discover 
               (λ (G s from to) 
                 (syntax-parameterize ([$from (syntax-id-rules () [_ from])]
                                       [$to (syntax-id-rules () [_ to])])
                   discexp ...))))
       (?? (?@ #:visit (λ (G s v) visit visitrst ...)))
       (?? (?@ #:visit (λ (G s v-new) 
                         (syntax-parameterize ([$v (syntax-id-rules () [_ v-new])])
                           visexp ...))))
       (?? (?@ #:return (λ _ return ...)))))]))
             

;; bfs-based fns --------------------------------------------------------------
;; (see also prim and dijkstra)

;; returns the path in G from s to v with the fewest vertices in between
;; ie, the shortest path from s to v for undirected graphs, 
;;     or graphs where all edges have the same weight
(define (fewest-vertices-path G s v)
  (cond
    [(vertex=? G s v) (list s)]
    [else
     (define-vertex-property G π #:init #f)
     (define-graph-property found-v? #f)
     
     (do-bfs G s #:break get-found-v?
       #:discover
       (when (vertex=? G $to v) (found-v?-set! #t))
       (π-set! $to $from)
       #:return
       (and found-v?
            (let loop ([path null] [v v])
              (if (vertex=? G v s) 
                  (cons s path) 
                  (loop (cons v path) (π v))))))]))

           
;; dfs ------------------------------------------------------------------------

(define (dfs G)
  ;; d[u] = discovery time, π[u] = pred, f[u] = finishing time
  (define-vertex-properties G d π f)
  (define-graph-property time 0)

  (do-dfs G
    #:prologue (time-set! (add1 time)) (d-set! $to time) (π-set! $to $from)
    #:epilogue (time-set! (add1 time)) (f-set! $to time)
    #:return (values (d->hash) (π->hash) (f->hash))))

(define (dfs/generalized G #:order [order (λ (vs) vs)]
                           #:break [break? (λ _ #f)]
                           #:init [init void]
                           #:visit? [custom-visit?-fn #f]
                           #:prologue [prologue void]
                           #:epilogue [epilogue void]
                           #:process-unvisited? [process-unvisited? (λ _ #f)]
                           #:process-unvisited [process-unvisited void]
                           #:return [finish void])
  (define visited (set))
  (define (mark-visited! v) (set! visited (set-add visited v)))
  (define visit? (or custom-visit?-fn (λ (G u v) (not (set-member? visited v)))))
  
  (init G)
  
  ;; inner loop: keep following (unvisited) links
  (define (do-visit parent u)
    (mark-visited! u)
    (prologue G parent u)
    (for ([v (in-neighbors G u)] #:break (break?))
      (cond [(visit? G u v) (do-visit u v)]
            [(process-unvisited? G u v) (process-unvisited G u v)]))
    (epilogue G parent u))
  
  ;; outer loop: picks a new start node when previous search reaches dead end
  (for ([u (order (in-vertices G))] #:break (break?))
    (cond [(visit? G #f u) (do-visit #f u)]
          [(process-unvisited? G #f u) (process-unvisited G #f u)]))
  
  (finish G))

;; TODO: there is potential ambiguity here in the binding clauses
;; If the programmer omits the bindings, but the first expr happens to have the
;; form (id1 id2), then there will be a syntax error for $to and $from bc
;; they won't be bound.
;;
;; cleaner syntax for dfs/generalized
(define-syntax (do-dfs stx)
  (syntax-parse stx 
    [(_ G 
     (~or (~optional (~seq #:order order:expr))
          (~optional (~seq #:break break?:expr))
          (~optional (~seq #:init init:expr ...))
          (~or (~optional 
                (~seq #:visit? (v?-from:id v?-to:id) visit?:expr v?rst:expr ...))
               (~optional 
                (~seq #:visit? v?exp:expr ...)))
          (~or (~optional 
                (~seq #:prologue (pro-from:id pro-to:id) pro:expr prorst:expr ...))
               (~optional 
                (~seq #:prologue proexp:expr ...)))
          (~or (~optional 
                (~seq #:epilogue (epi-from:id epi-to:id) epi:expr epirst:expr ...))
               (~optional 
                (~seq #:epilogue epiexp:expr ...)))
          (~or (~optional 
                (~seq #:process-unvisited? (pu?-from:id pu?-to:id) pu?:expr pu?rst:expr ...))
               (~optional 
                (~seq #:process-unvisited? pu?exp:expr ...)))
          (~or (~optional 
                (~seq #:process-unvisited (pu-from:id pu-to:id) pu:expr purst:expr ...))
               (~optional 
                (~seq #:process-unvisited puexp:expr ...)))
          (~optional (~seq #:return return:expr ...))) ...)
     (template
      (dfs/generalized G 
        (?? (?@ #:order order))
        (?? (?@ #:break break?))
        (?? (?@ #:init (λ _ init ...)))
        (?? (?@ #:visit? (λ (G v?-from v?-to) visit? v?rst ...)))
        (?? (?@ #:visit? 
                (λ (G from to) 
                  (syntax-parameterize ([$from (syntax-id-rules () [_ from])]
                                        [$to (syntax-id-rules () [_ to])])
                           v?exp ...))))
        (?? (?@ #:prologue (λ (G pro-from pro-to) pro prorst ...)))
        (?? (?@ #:prologue 
                (λ (G from to) 
                  (syntax-parameterize ([$from (syntax-id-rules () [_ from])]
                                        [$to (syntax-id-rules () [_ to])])
                             proexp ...))))
        (?? (?@ #:epilogue (λ (G epi-from epi-to) epi epirst ...)))
        (?? (?@ #:epilogue 
                (λ (G from to) 
                  (syntax-parameterize ([$from (syntax-id-rules () [_ from])]
                                        [$to (syntax-id-rules () [_ to])])
                             epiexp ...))))
        (?? (?@ #:process-unvisited? (λ (G pu?-from pu?-to) pu? pu?rst ...)))
        (?? (?@ #:process-unvisited? 
                (λ (G from to) 
                  (syntax-parameterize ([$from (syntax-id-rules () [_ from])]
                                        [$to (syntax-id-rules () [_ to])])
                                       pu?exp ...))))
        (?? (?@ #:process-unvisited (λ (G pu-from pu-to) pu purst ...)))
        (?? (?@ #:process-unvisited 
                (λ (G from to) 
                  (syntax-parameterize ([$from (syntax-id-rules () [_ from])]
                                        [$to (syntax-id-rules () [_ to])])
                                      puexp ...))))
        (?? (?@ #:return (λ _ return ...)))))]))

;; dfs-based fns --------------------------------------------------------------

(define (dag? G)
  (define-vertex-property G color #:init WHITE)
  (define-graph-property not-dag? #f)
  
  (do-dfs G #:break get-not-dag?
    #:visit? (white? (color $to))
    #:prologue (color-set! $to GRAY)
    #:epilogue (color-set! $to BLACK)
    #:process-unvisited? (gray? (color $to))
    #:process-unvisited (not-dag?-set! #t)
    #:return (not not-dag?)))

(define (tsort G)
  (define sorted null) 

  (do-dfs G
    #:epilogue (set! sorted (cons $to sorted)) ; add finished
    #:return sorted))
  
;; tarjan algorithm for strongly connected components
(define (scc G)
  (define i 0)
  (define-vertex-properties G index lowlink)

  (define S null)   
  (define (S-push x) (set! S (cons x S)))
  
  (define SCC null)
  (define (build-SCC? v) (= (lowlink v) (index v)))
  (define (build-SCC v)
    (define-values (new-scc S-rst) (splitf-at S (λ (w) (not (vertex=? G w v)))))
    (set! SCC (cons (cons v new-scc) SCC))
    (set! S (cdr S-rst)))

  (do-dfs G 
    #:prologue (index-set! $to i) (lowlink-set! $to i) (add1! i) (S-push $to)
    #:epilogue 
    (when (build-SCC? $to) (build-SCC $to))
    (when $from (lowlink-set! $from (min (lowlink $from) (lowlink $to))))
    #:process-unvisited? (member $to S)
    #:process-unvisited (lowlink-set! $from (min (lowlink $from) (index $to)))
    #:return SCC))


      