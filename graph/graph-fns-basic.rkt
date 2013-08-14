#lang racket

(require "hash-utils.rkt"
         "utils.rkt"
         "gen-graph.rkt"
         "../queue/gen-queue.rkt"
         (only-in "../queue/fifo.rkt" mk-empty-fifo))

(require (for-syntax syntax/parse))

(provide (all-defined-out))

;; ----------------------------------------------------------------------------
;; bfs and dfs

;; bfs : Graph Vertex -> [Hashof Vertex -> Number] [Hashof Vertex -> Vertex]
;; s is the source vertex
;; returns 2 values: hash mapping vertices to distances and
;;  hash mapping vertices to predecessor
(define (bfs G s)
  (define-hashes d π)

  (do-bfs G s 
    #:init 
      (for ([u (in-vertices G)]) (d-set! u +inf.0) (π-set! u #f))
      (d-set! s 0) (π-set! s #f)
    #:discover (from to)
      (d-set! to (add1 (d from))) (π-set! to from)
    #:return (values d π)))

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
  (define visit? (or custom-visit?-fn (λ (G s u v) (not (set-member? discovered v)))))
  
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

;; cleaner syntax for bfs/generalized
(define-syntax (do-bfs stx)
  (syntax-parse stx 
    [(_ G s (~optional (~seq #:init-queue Q:expr))
            (~optional (~seq #:break break?:expr))
            (~optional (~seq #:init init:expr ...))
            (~optional (~seq #:visit? (visit?-from:id visit?-to:id) visit?:expr ...))
            (~optional (~seq #:discover (discover-from:id discover-to:id) discover:expr ...))
            (~optional (~seq #:visit (v:id) visit:expr ...))
            (~optional (~seq #:return return:expr ...)))
     #`(bfs/generalized 
        G s 
        #,@(if (attribute Q) #'(#:init-queue Q) '())
        #,@(if (attribute break?) #'(#:break break?) '())
        #,@(if (attribute init) #'(#:init (λ _ init ...)) '())
        #,@(if (attribute visit?) #'(#:visit? (λ (G s visit?-from visit?-to) visit? ...)) '())
        #,@(if (attribute discover) #'(#:discover (λ (G s discover-from discover-to) discover ...)) '())
        #,@(if (attribute visit) #'(#:visit (λ (G s v) visit ...)) '())
        #,@(if (attribute return) #'(#:return (λ _ return ...)) '()))]))
             

;; bfs-based fns --------------------------------------------------------------
;; (see also prim and dijkstra)

;; returns the path in G from s to v with the fewest vertices in between
;; ie, the shortest path from s to v for undirected graphs, 
;;     or graphs where all edges have the same weight
(define (fewest-vertices-path G s v)
  (define-hashes color π)
  (define found-v #f)
  (define (found-v?) found-v)

  (do-bfs G s #:break found-v?
    #:init
      (for ([u (in-vertices G)]) (π-set! u #f))
      (π-set! s #f)
    #:discover (to from)
      (when (equal? from v) (set! found-v #t))
      (π-set! from to)
    #:return
      (if found-v?
          (let loop ([path null] [v v])
            (if (equal? v s) (cons s path) (loop (cons v path) (π v))))
          (error 'shortest-path "no path from ~a to ~a" s v))))

           
;; dfs ------------------------------------------------------------------------

(define (dfs G)
  ;; d[u] = discovery time, f[u] = finishing time
  (define-hashes d π f) (define time 0)

  (do-dfs G
    #:prologue (parent u) (add1! time) (d-set! u time) (π-set! u parent)
    #:epilogue (parent u) (add1! time) (f-set! u time)
    #:return (values d π f)))

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

;; cleaner syntax for dfs/generalized
(define-syntax (do-dfs stx)
  (syntax-parse stx 
    [(_ G (~optional (~seq #:order order:expr))
          (~optional (~seq #:break break?:expr))
          (~optional (~seq #:init init:expr ...))
          (~optional (~seq #:visit? (visit?-from:id visit?-to:id) visit?:expr ...))
          (~optional (~seq #:prologue (prologue-from:id prologue-to:id) prologue:expr ...))
          (~optional (~seq #:epilogue (epilogue-from:id epilogue-to:id) epilogue:expr ...))
          (~optional (~seq #:process-unvisited? 
                           (process-unvisited?-from:id process-unvisited?-to:id) 
                           process-unvisited?:expr ...))
          (~optional (~seq #:process-unvisited 
                           (process-unvisited-from:id process-unvisited-to:id)
                           process-unvisited:expr ...))
          (~optional (~seq #:return return:expr ...)))
     #`(dfs/generalized G 
        #,@(if (attribute order) #'(#:order order) '())
        #,@(if (attribute break?) #'(#:break break?) '())
        #,@(if (attribute init) #'(#:init (λ _ init ...)) '())
        #,@(if (attribute visit?) #'(#:visit? (λ (G visit?-from visit?-to) visit? ...)) '())
        #,@(if (attribute prologue) #'(#:prologue (λ (G prologue-from prologue-to) prologue ...)) '())
        #,@(if (attribute epilogue) #'(#:epilogue (λ (G epilogue-from epilogue-to) epilogue ...)) '())
        #,@(if (attribute process-unvisited?) 
               #'(#:process-unvisited? 
                  (λ (G process-unvisited?-from process-unvisited?-to) 
                    process-unvisited? ...))
               '())
        #,@(if (attribute process-unvisited) 
               #'(#:process-unvisited 
                  (λ (G process-unvisited-from process-unvisited-to)
                    process-unvisited ...))
               '())
        #,@(if (attribute return) #'(#:return (λ _ return ...)) '()))]))

;; dfs-based fns --------------------------------------------------------------

(define (dag? G)
  (define-hashes color)
  (define not-dag #f)
  (define (not-dag?) not-dag)
  
  (do-dfs G #:break not-dag?
    #:init (for ([u (in-vertices G)]) (color-set! u WHITE))
    #:visit? (from to) (white? (color to))
    #:prologue (parent v) (color-set! v GRAY)
    #:epilogue (parent v) (color-set! v BLACK)
    #:process-unvisited? (from to) (gray? (color to))
    #:process-unvisited (from to) (set! not-dag #t)
    #:return (not not-dag)))

(define (tsort G)
  (define sorted null) 

  (do-dfs G
    #:epilogue (parent v) (set! sorted (cons v sorted)) ; add finished
    #:return sorted))
  
;; tarjan algorithm for strongly connected components
(define (scc G)
  (define i 0)
  (define-hashes index lowlink)

  (define S null)   
  (define (S-push x) (set! S (cons x S)))
  
  (define SCC null)
  (define (build-SCC? v) (= (lowlink v) (index v)))
  (define (build-SCC v)
    (define-values (new-scc S-rst) (splitf-at S (λ (w) (not (equal? w v)))))
    (set! SCC (cons (cons v new-scc) SCC))
    (set! S (cdr S-rst)))

  (do-dfs G 
    #:prologue (parent v)
      (index-set! v i) (lowlink-set! v i) (add1! i) (S-push v)
    #:epilogue (parent v)
      (when (build-SCC? v) (build-SCC v))
      (when parent (lowlink-set! parent (min (lowlink parent) (lowlink v))))
    #:process-unvisited? (from to) (member to S)
    #:process-unvisited (from to) (lowlink-set! from (min (lowlink from) (index to)))
    #:return SCC))


      