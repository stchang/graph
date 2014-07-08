#lang racket/base

(require "graph-property.rkt"
         "utils.rkt"
         "gen-graph.rkt"
         "../queue/gen-queue.rkt"
         (only-in "../queue/fifo.rkt" mk-empty-fifo))

(require (for-syntax syntax/parse syntax/parse/experimental/template)
         (only-in racket/list splitf-at) racket/stxparam  racket/unsafe/ops)

(provide (all-defined-out))

;; ----------------------------------------------------------------------------
;; bfs and dfs

;; bfs : Graph Vertex -> [Hashof Vertex -> Number] [Hashof Vertex -> Vertex]
;; s is the source vertex
;; returns 2 values: hash mapping vertices to distances and
;;  hash mapping vertices to predecessor
(define (bfs G s)
  (define-vertex-property G d #:init +inf.0) ; d = num edges
  (define-vertex-property G π #:init #f)

  (do-bfs G s 
    #:init (d-set! s 0) (π-set! s #f)
    #:discover (d-set! $v (unsafe-add1 (d $from))) (π-set! $v $from)
    #:return (values (d->hash) (π->hash))))

;; default Q is from data/queue
;; see also bfs clients prim and dijkstra
;; NOTE: The keyword names are not quite accurate but I'm leaving them as is
;;       for backwards compatibility. 
;;   - Specifically, #:discover should be #:on-enqueue, since it's called every
;;   time a vertex is enqueued to be visited and not just the first time.
;;   It's correct in the default case, however, because a vertex is not enqueued
;;   if it's already been discovered, but not for other cases like dij or prim.
;;   - #:visit? is ok but sometimes reads funny if #:visit is not specified. It
;;   could alternatively be called #:enqueue?. And following this naming heme, 
;;   #:visit could also be #:on-dequeue.
;;
;; For #:visit?, seen/discovered status is still the default behavior but the
;; tracking of seen vertices is moved to do-bfs.
(define (bfs/generalized G s #:init-queue [Q (mk-empty-fifo)]
                             #:break [break? (λ _ #f)]
                             #:init [init void]
                             #:visit? [enqueue? (λ _ #t)]
                             #:discover [on-enqueue (λ (G s u v acc) acc)]
                             #:visit [visit (λ (G s u acc) acc)]
                             #:return [finish (λ (G s acc) acc)])
  (enqueue! Q s) ; source vertex s is always visited
  (define broken? #f)
  
  (define new-acc 
    (for/fold ([acc (init G s)]) ([u (in-queue Q)] #:break broken?)
      (for/fold ([inner-acc (visit G s u acc)])
        ([v (in-neighbors G u)] 
         #:when (enqueue? G s u v) 
         #:break (or broken? (and (break? G s u v) (set! broken? #t))))
        (begin0 (on-enqueue G s u v inner-acc)
                (enqueue! Q v)))))
  
  (finish G s new-acc))

;; TODO: there is potential ambiguity here in the clauses that bind
;; If the programmer omits the bindings, but the first expr happens to have the
;; form (id1 id2), then there will be a syntax error for $to and $from bc
;; they won't be bound.
;;
;; cleaner syntax for bfs/generalized
(define-syntax (do-bfs stx)
  (syntax-parse stx 
    [(_ G s 
      (~or (~optional (~seq #:init-queue Q:expr))
           (~or (~optional (~seq #:break (b?-from:id b?-to:id) b?:expr b?rst:expr ...))
                (~optional (~seq #:break b?exp:expr ...)))
           (~optional (~seq #:init init:expr ...))
           (~or (~optional 
                 (~seq #:visit? (v?-from:id v?-to:id) visit?:expr v?rst:expr ...))
                (~optional (~seq #:visit? v?exp:expr ...))
                (~optional 
                 (~seq #:enqueue? (e?-from:id e?-to:id) enq?:expr e?rst:expr ...))
                (~optional (~seq #:enqueue? e?exp:expr ...)))
           (~or (~optional 
                 (~seq #:discover (disc-from:id disc-to:id disc-acc:id)
                       disc:expr discrst:expr ...))
                (~optional (~seq #:discover discexp:expr ...))
                (~optional 
                 (~seq #:on-enqueue (onenq-from:id onenq-to:id onenq-acc:id)
                       onenq:expr onenqrst:expr ...))
                (~optional (~seq #:on-enqueue onenqexp:expr ...)))
           (~or (~optional (~seq #:visit (v:id vacc:id)
                                 visit:expr visitrst:expr ...))
                (~optional (~seq #:visit visexp:expr ...))
                (~optional (~seq #:on-dequeue (ondeqv:id ondeqacc:id)
                                 ondeq:expr ondeqrst:expr ...))
                (~optional (~seq #:on-dequeue ondeqexp:expr ...)))
           (~or (~optional (~seq #:return (retacc:id) ret:expr retrst:expr ...))
                (~optional (~seq #:return return:expr ...)))) ...)
     (template
      (let ([broken? #f])
        (define-vertex-properties G discovered? visited?)
        (define (mark-discovered! u) (discovered?-set! u #t))
        (define (mark-visited! u) (visited?-set! u #t))
        (mark-discovered! s)
        (syntax-parameterize 
         ([$discovered? (syntax-rules () [(_ u) (discovered?-defined? u)])]
          [$seen? (syntax-rules () [(_ u) (discovered?-defined? u)])]
          [$visited? (syntax-rules () [(_ u) (visited?-defined? u)])])
        (bfs/generalized G s 
          (?? (?@ #:init-queue Q))
          (?? (?@ #:break (λ (G s b?-from b?-to) 
                            (or broken? 
                                (let ([res (let () b? b?rst ...)])
                                  (and res (set! broken? #t)))))))
          (?? (?@ #:break
                  (λ (G s from to)
                    (syntax-parameterize ([$from (syntax-id-rules () [_ from])]
                                          [$to (syntax-id-rules () [_ to])]
                                          [$v (syntax-id-rules () [_ to])])
                      (or broken?
                          (let ([res (let () b?exp ...)])
                            (and res (set! broken? #t))))))))
          (?? (?@ #:init (λ _ init ...)))
          ;; #:visit? possible clauses
          (??
           ;; #:visit?
           (?@ #:visit? (λ (G s v?-from v?-to) visit? v?rst ...))
           (??
            (?@ #:visit? 
                (λ (G s from to)
                  (syntax-parameterize ([$from (syntax-id-rules () [_ from])]
                                        [$to (syntax-id-rules () [_ to])]
                                        [$v (syntax-id-rules () [_ to])])
                                       v?exp ...)))
            (??
             ;; #:enqueue?
             (?@ #:visit? (λ (G s e?-from e?-to) enq? e?rst ...))
             (??
              (?@ #:visit? 
                  (λ (G s from to)
                    (syntax-parameterize ([$from (syntax-id-rules () [_ from])]
                                          [$to (syntax-id-rules () [_ to])]
                                          [$v (syntax-id-rules () [_ to])])
                                         e?exp ...)))
              ;; else
              (?@ #:visit? (λ (G s from to) (not (discovered?-defined? to))))))))
          ;; #:discover possible clauses
          ;; #:discover
          (?? (?@ #:discover (λ (G s disc-from disc-to disc-acc) 
                               (mark-discovered! disc-to) disc discrst ...))
              (?? (?@ #:discover 
                      (λ (G s from to acc) 
                        (mark-discovered! to)
                        (syntax-parameterize ([$from (syntax-id-rules () [_ from])]
                                              [$to (syntax-id-rules () [_ to])]
                                              [$v (syntax-id-rules () [_ to])]
                                              [$acc (syntax-id-rules () [_ acc])])
                                             discexp ...)))
                  ;; #:on-enqueue
                  (?? (?@ #:discover (λ (G s onenq-from onenq-to onenq-acc) 
                                       (mark-discovered! onenq-to) onenq onenqrst ...))
                      (?? (?@ #:discover 
                              (λ (G s from to acc) 
                                (mark-discovered! to)
                                (syntax-parameterize ([$from (syntax-id-rules () [_ from])]
                                                      [$to (syntax-id-rules () [_ to])]
                                                      [$v (syntax-id-rules () [_ to])]
                                                      [$acc (syntax-id-rules () [_ acc])])
                                                     onenqexp ...)))
                          ;; else
                          (?@ #:discover (λ (G s from to acc) (mark-discovered! to) acc))))))
          ;; #:visit possible clauses
          ;; #:visit
          (?? (?@ #:visit (λ (G s v vacc) (mark-visited! v) visit visitrst ...))
              (?? (?@ #:visit (λ (G s u acc) 
                                (mark-visited! u)
                                (syntax-parameterize ([$v (syntax-id-rules () [_ u])]
                                                      [$acc (syntax-id-rules () [_ acc])])
                                                     visexp ...)))
                  ;; #:on-dequeue
                  (?? (?@ #:visit (λ (G s ondeqv ondeqacc) 
                                    (mark-visited! ondeqv) ondeq ondeqrst ...))
                      (?? (?@ #:visit (λ (G s u acc) 
                                        (mark-visited! u)
                                        (syntax-parameterize ([$v (syntax-id-rules () [_ u])]
                                                              [$acc (syntax-id-rules () [_ acc])])
                                                             ondeqexp ...)))
                          ;; else
                          (?@ #:visit (λ (G s u acc) (mark-visited! u) acc))))))
          (?? (?@ #:return (λ (G s retacc) ret retrst ...)))
          (?? (?@ #:return 
                   (λ _
                     (syntax-parameterize ([$broke? (syntax-id-rules () [_ broken?])]
                                           [$acc (syntax-id-rules () [_ acc])])
                                          return ...))))))))]))
             

;; bfs-based fns --------------------------------------------------------------
;; (see also prim and dijkstra)

;; returns the path in G from src to targ with the fewest vertices in between
;; ie, the shortest path from src to targ for undirected graphs, 
;;     or graphs where all edges have the same weight
(define (fewest-vertices-path G src targ)
  (cond
    [(vertex=? G src targ) (list src)]
    [else
     (define-vertex-property G π #:init #f)
     (do-bfs G src 
      #:break (and (vertex=? G $v targ) (π-set! $v $from))
      #:discover (π-set! $v $from)
      #:return
      (and $broke?
           (let loop ([path null] [v targ]) ; reverse the found path
             (if (vertex=? G v src) 
                 (unsafe-cons-list src path) 
                 (loop (unsafe-cons-list v path) (π v))))))]))

           
;; dfs ------------------------------------------------------------------------

(define (dfs G)
  ;; d[u] = discovery time, π[u] = pred, f[u] = finishing time
  (define-vertex-properties G d π f)

  (do-dfs G
   #:init 0 ; time
   #:prologue (from v time) 
    (d-set! v time) (π-set! v from) 
    (unsafe-add1 time)
   #:epilogue (from v time)
    (f-set! v time)
    (unsafe-add1 time)
   #:return (values (d->hash) (π->hash) (f->hash))))

;; TODO: don't export this?
(define (dfs/generalized G #:order [order (λ (x) x)]
                           #:break [break? (λ _ #f)]
                           #:init [init void]
                           #:inner-init [inner-init (λ (acc) acc)]
                           #:visit? [custom-visit?-fn #f]
                           #:prologue [prologue (λ (G u v acc) acc)]
                           #:epilogue [epilogue (λ (G u v acc) acc)]
                           #:process-unvisited? [process-unvisited? (λ _ #f)]
                           #:process-unvisited [process-unvisited (λ (G u v acc) acc)]
                           #:combine [combine (λ (x acc) x)]
                           #:return [finish (λ (G acc) acc)])
  (define-vertex-property G visited?)
  (define (mark-visited! v) (visited?-set! v #t))
  (define visit? (or custom-visit?-fn (λ (G u v) (not (visited?-defined? v)))))
  (define broken? #f) ; redundant with do-dfs but no speed penalty
  
  ;; inner loop: keep following (unvisited) links
  (define (do-visit parent u acc)
    (unless custom-visit?-fn (mark-visited! u))
    (define new-acc
      (for/fold ([acc (prologue G parent u acc)])
                ([v (in-neighbors G u)] 
                 #:break (or broken? (and (break? G u v) (set! broken? #t))))
        (cond [(visit? G u v) (do-visit u v acc)]
              [(process-unvisited? G u v) (process-unvisited G u v acc)]
              [else acc])))
    (epilogue G parent u new-acc))
  
  ;; outer loop: picks a new start node when previous search reaches dead end
  (define new-acc 
    (for/fold ([acc (init G)]) 
              ([u (order (get-vertices G))] 
               #:break (or broken? (and (break? G #f u) (set! broken? #t))))
      (cond [(visit? G #f u) (combine (do-visit #f u (inner-init acc)) acc)]
            [(process-unvisited? G #f u) (process-unvisited G #f u acc)]
            [else acc])))
  
  (finish G new-acc))

;; TODO: there is potential ambiguity here in the binding clauses
;; If the programmer omits the bindings, but the first expr happens to have the
;; form (id1 id2 id3), then there will be a syntax error for $to and $from bc
;; they won't be bound.
;;
;; cleaner syntax for dfs/generalized
(define-syntax (do-dfs stx)
  (syntax-parse stx 
    [(_ G 
      (~or 
       (~optional (~seq #:order order:expr))
       (~or (~optional (~seq #:break (b?-from:id b-?to:id) 
                             b?:expr b?rst:expr ...))
            (~optional (~seq #:break b?exp:expr ...)))
       (~optional (~seq #:init init:expr ...))
       (~optional (~seq #:inner-init iinit:expr ...))
       (~or (~optional (~seq #:visit? (v?-from:id v?-to:id)
                             visit?:expr v?rst:expr ...))
            (~optional (~seq #:visit? v?exp:expr ...)))
       (~or (~optional (~seq #:prologue (pro-from:id pro-to:id pro-acc:id)
                             pro:expr prorst:expr ...))
            (~optional (~seq #:prologue proexp:expr ...)))
       (~or (~optional (~seq #:epilogue (epi-from:id epi-to:id epi-acc:id)
                             epi:expr epirst:expr ...))
            (~optional (~seq #:epilogue epiexp:expr ...)))
       (~or (~optional (~seq #:process-unvisited? (pu?-from:id pu?-to:id)
                             pu?:expr pu?rst:expr ...))
            (~optional (~seq #:process-unvisited? pu?exp:expr ...)))
       (~or (~optional (~seq #:process-unvisited (pu-from:id pu-to:id pu-acc:id)
                             pu:expr purst:expr ...))
            (~optional (~seq #:process-unvisited puexp:expr ...)))
       (~optional (~seq #:combine combine))
       (~or (~optional (~seq #:return (ret-acc:id) ret:expr retrst:expr ...))
            (~optional (~seq #:return retexp:expr ...)))) ...)
     (template
      (let ([broken? #f])
      (dfs/generalized G 
       (?? (?@ #:order order))
       (?? (?@ #:break (λ (G b?-from b?-to) 
                         (or broken?
                             (let ([res (let () b? b?rst ...)])
                               (and res (set! broken? #t)))))))
       (?? (?@ #:break 
               (λ (G from to)
                 (syntax-parameterize ([$from (syntax-id-rules () [_ from])]
                                       [$to (syntax-id-rules () [_ to])]
                                       [$v (syntax-id-rules () [_ to])])
                                      (or broken?
                                          (let ([res (let () b?exp ...)])
                                            (and res (set! broken? #t))))))))
       (?? (?@ #:init (λ _ init ...)))
       (?? (?@ #:inner-init (λ _ iinit ...)))
       (?? (?@ #:visit? (λ (G v?-from v?-to) visit? v?rst ...)))
       (?? (?@ #:visit? 
               (λ (G from to) 
                 (syntax-parameterize ([$from (syntax-id-rules () [_ from])]
                                       [$to (syntax-id-rules () [_ to])]
                                       [$v (syntax-id-rules () [_ to])])
                                      v?exp ...))))
       (?? (?@ #:prologue (λ (G pro-from pro-to pro-acc) pro prorst ...)))
       (?? (?@ #:prologue 
               (λ (G from to acc) 
                 (syntax-parameterize ([$from (syntax-id-rules () [_ from])]
                                       [$to (syntax-id-rules () [_ to])]
                                       [$v (syntax-id-rules () [_ to])]
                                       [$acc (syntax-id-rules () [_ acc])])
                                      proexp ...))))
       (?? (?@ #:epilogue (λ (G epi-from epi-to epi-acc) epi epirst ...)))
       (?? (?@ #:epilogue 
               (λ (G from to acc) 
                 (syntax-parameterize ([$from (syntax-id-rules () [_ from])]
                                       [$to (syntax-id-rules () [_ to])]
                                       [$v (syntax-id-rules () [_ to])]
                                       [$acc (syntax-id-rules () [_ acc])])
                                      epiexp ...))))
       (?? (?@ #:process-unvisited? (λ (G pu?-from pu?-to) pu? pu?rst ...)))
       (?? (?@ #:process-unvisited? 
               (λ (G from to) 
                 (syntax-parameterize ([$from (syntax-id-rules () [_ from])]
                                       [$to (syntax-id-rules () [_ to])]
                                       [$v (syntax-id-rules () [_ to])])
                                      pu?exp ...))))
        (?? (?@ #:process-unvisited (λ (G pu-from pu-to pu-acc) pu purst ...)))
        (?? (?@ #:process-unvisited 
                (λ (G from to acc) 
                  (syntax-parameterize ([$from (syntax-id-rules () [_ from])]
                                        [$to (syntax-id-rules () [_ to])]
                                        [$v (syntax-id-rules () [_ to])]
                                        [$acc (syntax-id-rules () [_ acc])])
                                       puexp ...))))
        (?? (?@ #:combine combine))
        (?? (?@ #:return (λ (G ret-acc) ret retrst ...)))
        (?? (?@ #:return 
                (λ (G acc) 
                  (syntax-parameterize ([$broke? (syntax-id-rules () [_ broken?])]
                                        [$acc (syntax-id-rules () [_ acc])])
                                       retexp ...)))))))]))

;; dfs-based fns --------------------------------------------------------------

(define (dag? G)
  (define-vertex-property G color #:init WHITE)
  (do-dfs G 
   #:break (gray? (color $v)) ; seeing a gray vertex means a loop
   #:visit? (white? (color $v))
   #:prologue (color-set! $v GRAY)
   #:epilogue (color-set! $v BLACK)
   #:return (not $broke?))) ; didnt break means no loop = acyclic

(define (tsort G) (do-dfs G #:init null #:epilogue (unsafe-cons-list $v $acc)))
  
;; tarjan algorithm for strongly connected components
;; G must be directed
(define (scc G [=fn #f])
  (define eq (or =fn (λ (u v) (vertex=? G u v))))
  (define i 0)
  (define-vertex-properties G index lowlink in-S?)

  (define S null) ; intermediate stack
  (define-syntax-rule (S-push x) 
    (begin (in-S?-set! x #t) (set! S (unsafe-cons-list x S))))
  
  (define SCC null)
  (define-syntax-rule (build-SCC? v) (unsafe-fx= (lowlink v) (index v)))
  (define-syntax-rule (build-SCC v)
    (let-values ([(new-scc S-rst)
                  (splitf-at S (λ (w) (in-S?-set! w #f) (not (eq w v))))])
      (set! S (unsafe-cdr S-rst)) ; scc always has >=1 element (ie v)
      (set! SCC (unsafe-cons-list (unsafe-cons-list v new-scc) SCC))))

  (do-dfs G 
   #:visit? (not (lowlink-defined? $v))
   #:prologue (S-push $v) (index-set! $v i) (lowlink-set! $v i) (unsafe-add1! i)
   #:epilogue 
    (define llv (lowlink $v))
    (when (unsafe-fx= llv (index $v)) (build-SCC $v))
    (when $from 
      (define llfrom (lowlink $from))
      (when (unsafe-fx< llv llfrom) (lowlink-set! $from llv)))
   #:process-unvisited? (and $from (in-S? $v))
   #:process-unvisited  
    (let ([llfrom (lowlink $from)] [iv (index $v)])
      (when (unsafe-fx< iv llfrom) (lowlink-set! $from iv)))
    #:return SCC))

;; connected components
;; G is undirected
;; cc : graph -> [Listof [Listof vertices]]
(define (cc G)
  (do-dfs G #:init null ; final result is list of lists
            #:inner-init null ; each cc is list of vertices
            #:prologue (unsafe-cons-list $v $acc)
            #:combine unsafe-cons-list))

;; compute cc using bfs
;; TODO: speed up by not using member
(define (cc/bfs G)
  (define (v-in-ccs? ccs v) (for/or ([cc ccs]) (member v cc)))
  (for/fold ([ccs null]) ([v (in-vertices G)] #:unless (v-in-ccs? ccs v))
    (cons (do-bfs G v #:init null #:visit (unsafe-cons-list $v $acc)) ccs)))