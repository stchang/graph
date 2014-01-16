#lang scribble/manual
@(require scribble/eval
          (for-label graph
                     racket/contract/base
                     racket/generic
                     data/queue
                     racket))

@title{Racket Generic Graph Library}

@defmodule[graph]

Generic graph library for Racket. 

Requires Racket 5.3.2 or later (due to @racket[define-generics] and @racket[enqueue-front!] in @racketmodname[data/queue]).

@(define the-eval (make-base-eval))
@(the-eval '(require graph))

@author[@author+email["Stephen Chang" "stchang@racket-lang.org"]]

@; generic interface ----------------------------------------------------------
@section{Generic Graph Interface}

@defthing[gen:graph any/c]{
  A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{generic interface} (see @secref[#:doc '(lib "scribblings/reference/reference.scrbl")]{struct-generics}) that defines a @deftech{graph}. To supply method implementations, a struct should use the @racket[#:methods] form.}

@defproc[(graph? [g any/c]) boolean?]{
  Returns @racket[#t] if @racket[g] implements @racket[gen:graph] (ie, is a @tech{graph}) and @racket[#f] otherwise.}

A @tech{graph} has the following methods:

@itemize[
 
  @item{@defproc[(has-vertex? [g graph?] [v any/c]) boolean?]{Indicates whether a vertex is in the given graph.}}
  @item{@defproc[(has-edge? [g graph?] [u any/c] [v any/c]) boolean?]{Indicates whether an edge is in the given graph.}}
  @item{@defproc[(add-vertex! [g graph?] [v any/c]) void?]{Imperatively adds a vertex to a graph.}}
  @item{@defproc[(add-edge! [g graph?] [u any/c] [v any/c] [weight number? 1]) void?]{Imperatively adds an undirected edge and optional weight to a graph.}}
  @item{@defproc[(add-directed-edge! [g graph?] [u any/c] [v any/c] [weight number? 0]) void?]{Imperatively adds a directed, optionally weighted edge to a graph.}}
  @item{@defproc[(remove-edge! [g graph?] [u any/c] [v any/c]) void?]{Imperatively removes the undirected edge.}}
  @item{@defproc[(remove-directed-edge! [g graph?] [u any/c] [v any/c]) void?]{Imperatively removes the directed edge.}}
  @item{@defproc[(in-vertices [g graph?]) list?]{Returns a list of vertices in the graph.}}
  @item{@defproc[(in-neighbors [g graph?] [v any/c]) sequence?]{Returns a sequence of vertex @racket[v]'s neighbors in the graph.}}
  @item{@defproc[(in-edges [g graph?]) sequence?]{Returns a sequence of edges in the graph.}}
  @item{@defproc[(edge-weight [g graph?] [u any/c] [v any/c]) number?]{Returns the weight of the edge in the graph (if it has one).}}
  @item{@defproc[(transpose [g graph?]) graph?]{Returns a new graph where the edges of the original graph are reversed.}}
  @item{@defproc[(graph-copy [g graph?]) graph?]{Returns a copy of the given graph.}}
  ]


@; graph constructors ---------------------------------------------------------
@section{Graph Constructors}

@; unweighted graphs ----------------------------------------------------------
@subsection{Unweighted Graphs}

@defproc[(unweighted-graph? [g any/c]) boolean?]{Indicates whether a graph is an unweighted graph.}

@defproc[(unweighted-graph/undirected [edges (listof (list/c any/c any/c))])
         (and/c graph? unweighted-graph?)]{
  Creates an unweighted graph that implements @racket[gen:graph] from a list of undirected edges. Each edge is represented by a list of two vertices. Vertices can be any racket values comparable with @racket[equal?].
@examples[#:eval the-eval
  (define g (unweighted-graph/undirected '((a b) (c d))))
  (graph? g)
  (unweighted-graph? g)
  (has-edge? g 'a 'b)
  (has-edge? g 'b 'a)
  ]
}
                                          
@defproc[(unweighted-graph/directed [edges (listof (list/c any/c any/c))])
         (and/c graph? unweighted-graph?)]{
Creates an unweighted graph that implements @racket[gen:graph] from a list of directed edges. Each edge is represented by a list of two vertices, where the first vertex in the list is the source and the second is the destination. Vertices can be any racket values comparable with @racket[equal?].
@examples[#:eval the-eval
  (define g (unweighted-graph/directed '((a b) (c d))))
  (graph? g)
  (unweighted-graph? g)
  (has-edge? g 'a 'b)
  (has-edge? g 'b 'a)
  ]}

@defproc[(unweighted-graph/adj [edges (listof list?)])
         (and/c graph? unweighted-graph?)]{
Creates an unweighted graph that implements @racket[gen:graph] from an adjacency list. Each element of the list is a list of vertices where the first vertex is the source and the rest are destinations. Vertices can be any racket values comparable with @racket[equal?].
@examples[#:eval the-eval
  (define g (unweighted-graph/adj '((a b c) (b c d))))
  (graph? g)
  (unweighted-graph? g)
  (has-edge? g 'a 'b)
  (has-edge? g 'b 'a)
  ]}
             
@; weighted graphs ------------------------------------------------------------
@subsection{Weighted Graphs}

@defproc[(weighted-graph? [g any/c]) boolean?]{Indicates whether a graph is a weighted graph.}

@defproc[(weighted-graph/undirected [edges (listof (list/c number? any/c any/c))])
         (and/c graph? weighted-graph?)]{
  Creates a weighted graph that implements @racket[gen:graph] from a list of weighted, undirected edges. Each edge is represented by a list of one number and two vertices, where the number is the weight. Vertices can be any racket values comparable with @racket[equal?].
@examples[#:eval the-eval
  (define g (weighted-graph/undirected '((10 a b) (20 b c))))
  (graph? g)
  (weighted-graph? g)
  (has-edge? g 'a 'b)
  (has-edge? g 'b 'a)
  (edge-weight g 'a 'b)
  (edge-weight g 'b 'a)
  ]
}
                                          
@defproc[(weighted-graph/directed [edges (listof (list/c number? any/c any/c))])
         (and/c graph? weighted-graph?)]{
Creates a weighted graph that implements @racket[gen:graph] from a list of weighted, directed edges. Each edge is represented by a list of one number and two vertices, where the number is the weight, and the first vertex in the list is the source and the second vertex is the destination. Vertices can be any racket values comparable with @racket[equal?]. Non-existent edges return an infinite weight, even for non-existent vertices.
@examples[#:eval the-eval
  (define g (weighted-graph/directed '((10 a b) (20 b c))))
  (graph? g)
  (weighted-graph? g)
  (has-edge? g 'a 'b)
  (has-edge? g 'b 'a)
  (edge-weight g 'a 'b)
  (edge-weight g 'b 'a)
  (edge-weight g 'b 'd)
  ]}

                                        
@; basic graph fns ------------------------------------------------------------

@section{Basic Graph Functions}

@; bfs ------------------------------------------------------------------------
@subsection{Breadth-first Search}

@defproc[(bfs [g graph?] [source any/c])
         (values (hash/c any/c number? #:immutable #f) (hash/c any/c any/c #:immutable #f))]{
 Standard textbook breadth-first search, ie as in CLRS. Takes two arguments, a graph and a source vertex. Returns two values, a hash table mapping a vertex to the distance (in terms of number of vertices) from the source, and a hash table mapping a vertex to its predecessor in the search.}
                                                                                            
@defproc[(bfs/generalized 
          [g graph?] [source any/c]
          [#:init-queue Q queue? (mk-empty-fifo)]
          [#:break break? (-> boolean?) (λ _ #f)]
          [#:init init (-> graph? [source any/c] void?) void]
          [#:visit? custom-visit?-fn (-> graph? [source any/c] [from any/c] [to any/c] boolean?) #f]
          [#:discover discover (-> graph? [source any/c] [from any/c] [to any/c] void?) void]
          [#:visit visit (-> graph? [source any/c] [v any/c] void?) void]
          [#:return finish (-> graph? [source any/c] any) void]) any]{
Generalized breadth-first search. Partially inspired by the C++ Boost Graph Library. See Lee et al. OOPSLA 1999 @cite["GGCL"]. Here is the rough implementation:
@racketblock[  
  (init G s)
  (enqueue! Q s)
  (mark-discovered! s)
  (for ([u (in-queue Q)])
    (visit G s u)
    (for ([v (in-neighbors G u)] #:when (visit? G s u v) #:break (break?))
      (mark-discovered! v)
      (discover G s u v)
      (enqueue! Q v)))
  (finish G s)]
Utilizes a queue that implements @racket[gen:queue]. A vertex is @deftech{discovered} when it gets added to the queue. If no @racket[custom-visit?-fn] is provided, then the function does not visit vertices that have already been discovered. When a vertex is dequeued, then @racket[visit] is called. The result of @racket[bfs/generalized] is the result of @racket[finish].}

@defform/subs[(do-bfs graph source maybe-init-queue maybe-break maybe-init maybe-visit? maybe-discover maybe-visit maybe-return)
              ([graph graph?]
               [maybe-init-queue (code:line) (code:line #:init-queue queue)]
               [queue queue?]
               [maybe-break (code:line) (code:line #:break break?)]
               [break? (-> boolean?)]
               [maybe-init (code:line) (code:line #:init init-exp ...)]
               [init-exp expr]
               [maybe-visit? (code:line) (code:line #:visit? (from to) visit?-exp ...)]
               [visit?-exp expr]
               [maybe-discover (code:line) (code:line #:discover (from to) discover-exp ...)]
               [discover-exp expr]
               [maybe-visit (code:line) (code:line #:visit (v) visit-exp ...)]
               [visit-exp expr]
               [maybe-return (code:line) (code:line #:return return-exp ...)]
               [return-exp expr]
               [from identifier?] [to identifier?] [v identifier?])]{
Cleaner syntax for @racket[bfs/generalized]. Essentially, this form eliminates the need to define separate functions and then pass them into @racket[bfs/generalized]'s keyword arguments. Instead, the bodies of those functions are inserted right after the corresponding keywords.
                   
For example, below is Dijkstra's algorithm, implemented with @racket[do-bfs]. In the code, @racket[d] is a hash mapping a vertex to the intermediate shortest distance from the source and @racket[π] is a hash mapping a vertex to its predecessor in the shortest path from the source. The algorithm utilizes a priority queue that is sorted according to intermediate shortest paths. A node is "visited" when it improves one of the paths.

@racketblock[
(define (dijkstra G s) 
  (define-hashes d π)
  (define (w u v) (edge-weight G u v))

  (do-bfs G s #:init-queue (mk-empty-priority (λ (u v) (< (d u) (d v))))
    #:init
      (for ([v (in-vertices G)]) (d-set! v +inf.0) (π-set! v #f))
      (d-set! s 0)
    #:visit? (to from) (> (d from) (+ (d to) (w to from)))
    #:discover (to from)
      (d-set! from (+ (d to) (w to from)))
      (π-set! from to)
    #:return (values d π)))]

The @racket[#:visit] clause binds two identifiers, representing the searched nodes and @racket[#:discover] is similar. This form is somewhat brittle since @racket[#:init] and @racket[#:return] don't bind any ways and instead @racket[G] and @racket[s] are captured from the context but this hasnt been a problem in practice.

@racket[bfs], @racket[fewest-vertices-path], @racket[mst-prim], and @racket[dijkstra] all use this form.}
               
@defproc[(fewest-vertices-path [G graph?] [source any/c] [target any/c]) (or/c list? #f)]{
Consumes a graph and two vertices, and returns the shortest path (in terms of number of vertices) between the two vertices. The result is a list of vertices, or @racket[#f] if there is no path.}

@; dfs ------------------------------------------------------------------------
@subsection{Depth-first Search}

@defproc[(dfs [g graph?])
         (values (hash/c any/c number? #:immutable #f) (hash/c any/c any/c #:immutable #f)
                 (hash/c any/c number? #:immutable #f))]{
Standard textbook depth-first search algorith, ie like in CLRS. Consumes a graph and returns three hashes: one that maps a vertex to its "discovery time", another that maps a vertex to its predecessor in the search, and a third that maps a vertex to its "finishing time".}
                                                        
                                                        
@defproc[(dfs/generalized 
          [g graph?]
          [#:order order (-> (list/c any/c) (list/c any/c)) (λ (x) x)]
          [#:break break? (-> boolean?) (λ _ #f)]
          [#:init init (-> graph? void?) void]
          [#:visit? custom-visit?-fn (-> graph? [from any/c] [to any/c] boolean?) #f]
          [#:prologue prologue (-> graph? [parent any/c] [v any/c] void?) void]
          [#:epilogue epilogue (-> graph? [parent any/c] [v any/c] void?) void]
          [#:process-unvisited? process-unvisited? 
                                (-> graph? [from any/c] [to any/c] boolean?) (λ _ #f)]
          [#:process-unvisited process-unvisited
                               (-> graph? [from any/c] [to any/c] void?) void]
          [#:return finish (-> graph? any) void]) any]{
Generalized depth-first search. Partially inspired by the C++ Boost Graph Library. See Lee et al. OOPSLA 1999 @cite["GGCL"]. Here is the rough implementation:
@racketblock[  
  (init G)

  (define (do-visit parent u)
    (mark-visited! u)
    (prologue G parent u)
    (for ([v (in-neighbors G u)] #:break (break?))
      (cond [(visit? G u v) (do-visit u v)]
            [(process-unvisited? G u v) (process-unvisited G u v)]))
    (epilogue G parent u))
  
  (for ([u (order (in-vertices G))] #:break (break?))
    (cond [(visit? G #f u) (do-visit #f u)]
          [(process-unvisited? G #f u) (process-unvisited G #f u)]))
  
  (finish G)]
The @racket[do-visit] function is the inner loop that keeps following edges depth-first until the search gets stuck. The "visit" part of the code is separated into two functions, the @racket[prologue], which represents the descending part of the visit, and @racket[epilogue], which gets called on the way back up. The outer @racket[for] loop picks a new start to restart the search when it gets stuck. Nodes that are already visited are marked and are not searched again. The algorithm terminates when all nodes are visited. The result of @racket[dfs/generalized] is the result of @racket[finish]. 

The @racket[order] function is a sorting function that specifies where to start the search and @racket[break] aborts the search and returns when it is true. @racket[process-unvisited?] and @racket[process-unvisited] specify code to run when a node is not visited.}

@defform/subs[(do-dfs graph maybe-order maybe-break maybe-init maybe-visit? maybe-prologue maybe-epilogue maybe-process-unvisited? maybe-process-unvisited maybe-return)
              ([graph graph?]
               [maybe-order (code:line) (code:line #:init-queue order)]
               [order (-> list? list?)]
               [maybe-break (code:line) (code:line #:break break?)]
               [break? (-> boolean?)]
               [maybe-init (code:line) (code:line #:init init-exp ...)]
               [init-exp expr]
               [maybe-visit? (code:line) (code:line #:visit? (from to) visit?-exp ...)]
               [visit?-exp expr]
               [maybe-prologue (code:line) (code:line #:prologue (parent v) prologue-exp ...)]
               [prologue-exp expr]
               [maybe-epilogue (code:line) (code:line #:epilogue (parent v) epilogue-exp ...)]
               [epilogue-exp expr]
               [maybe-process-unvisited? 
                (code:line) 
                (code:line #:process-unvisited? (from to) process-unvisited?-exp ...)]
               [process-unvisited?-exp expr]
               [maybe-process-unvisited
                (code:line) 
                (code:line #:process-unvisited (from to) process-unvisited-exp ...)]
               [process-unvisited-exp expr]
               [maybe-return (code:line) (code:line #:return return-exp ...)]
               [return-exp expr]
               [from identifier?] [to identifier?] [parent identifier?] [v identifier?])]{
Analogous to @racket[do-bfs]. Nicer syntax for @racket[dfs/generalized]. Below is @racket[dag?] as an example, which indicates whether a graph is directed and acyclic. The function uses the classic three vertex coloring, where white indicates unseen, gray indicates discovered, and black indicates done. Encountering a gray node while searching indicates a cycle, so the function keeps track of a flag that is set when a gray node is seen, and the @racket[#:break] function returns the value of this flag, terminating the search as soon as a cycle is found.
             
@racketblock[
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
    #:return (not not-dag)))]

@racket[dfs], @racket[tsort], @racket[dag?], and @racket[scc] all use this form.}
                                                                                         
@defproc[(dag? [g graph?]) boolean?]{
Indicates whether a graph is directed and acyclic.}

@defproc[(tsort [g graph?]) list?]{Returns the vertices in the given graph, topologically sorted. Note that the returned order may not be the only valid ordering.}

@defproc[(scc [g graph?]) (listof list?)]{Calculates the strongly connected components of a graph using Tarjan's algorithm @cite{tarjan-scc}. Returns a list of list of vertices, where each sublist is a strongly connected subgraph.}
                                       
@; min span trees -------------------------------------------------------------
@section{Minimum Spanning Tree}

@defproc[(mst-kruskal [g graph?]) (listof (list/c any/c any/c))]{Computes the minimum spanning tree using Kruskal's algorithm and the @racket[data/union-find] data structure. Returns a list of edges.}

@defproc[(mst-prim [g graph?]) (listof (list/c any/c any/c))]{Computes the minimum spanning tree of a graph using Prim's algorithm, which is based on breadth-first search. Returns a list of edges.}

@; single source shortest paths -----------------------------------------------
@section{Single-source Shortest Paths}

@defproc[(bellman-ford [g graph?] [source any/c]) 
         (values (hash/c any/c number? #:immutable #f) (hash/c any/c any/c #:immutable #f))]{Computes the shortest paths from the given source to every other vertex using the Bellman-Ford algorithm. The function errors when the graph has a negative weight cycle, but otherwise, negative weights are allowed.
                                                                                             
Returns two hashes, one that maps a vertex to its distance (in total edge weights) from the source and one that maps a vertex to its predecessor in the shortest path from the source.}
                                          
@defproc[(dijkstra [g graph?] [source any/c])
         (values (hash/c any/c number? #:immutable #f) (hash/c any/c any/c #:immutable #f))]{
Similarly computes shortest paths from the source to every other vertex. Faster than Bellman-Ford but no negative weight edges are allowed. Based on breadth-first search.}

                                                                                            @defproc[(dag-shortest-paths [g graph?] [source any/c]) 
         (values (hash/c any/c number? #:immutable #f) (hash/c any/c any/c #:immutable #f))]{
The fastest shortest paths algorithm but only works on dags.}
                                                                                            
                                                                                            
@; all-pairs shortest paths ---------------------------------------------------
@section{All-pairs Shortest Paths}
@defproc[(floyd-warshall [g graph?]) (hash/c (list/c any/c any/c) number? #:immutable #f)]{
Computes the length of the shortest path for all pairs of vertices using the Floyd-Warshall algorithm. Returns a hash mapping a pair of vertices to the length of the shortest path between them.}

@defproc[(transitive-closure [g graph?]) (hash/c (list/c any/c any/c) boolean? #:immutable #f)]{
Returns a hash mapping a pair of vertices to a boolean. If true, then there exists a path from the first vertex to the second.}
                                                                 
@defproc[(johnson [g graph?]) (hash/c (list/c any/c any/c) number? #:immutable #f)]{
Computes all-pairs shortest paths using Johnson's algorithm. Should be faster than Floyd Warshall for sparse graphs. Theoretical running time is O(VElogV).

Handles negative weights by first running @racket[bellman-ford]. The uses @racket[dijkstra] for each vertex in the graph.

Note, the running time could be theoretically faster with a version of Dijkstra that uses a Fibonacci heap instead of a standard heap.}


@; graph coloring -------------------------------------------------------------

Graph coloring functions are only valid for undirected graphs.

@section{Graph Coloring}

@defproc[(coloring [g graph?] [num-colors natural-number/c] [#:order order (-> list? list?) (λ (x) x)])
         (or/c (hash/c any/c number? #:immutable #f) #f)]{
Returns a coloring for the given graph using at most the specified number of colors, or @racket[#f] if no coloring is possible. Uses backtracking algorithm so takes exponential time. Optional @racket[#:order] parameter determines the order in which verticies are colored.}
                                                                                        
@defproc[(coloring/greedy 
          [g graph?] 
          [#:order order (or/c 'smallest-last (-> list? list?)) 'smallest-last])
         (values number? (hash/c any/c number? #:immutable #f))]{
Returns a "greedy" coloring of the given graph, where the color for a vertex is the "smallest" color not used by one of its neighbors (or the number of colors is increased).

The function always returns a valid coloring but the optimality (ie number of colors used) depends on the ordering in which the vertices are considered. The default is to use "smallest-last" ordering (see @racket[order-smallest-last]) but if @racket[#:order] is a procedure, then it is applied to sort the list of vertices before the coloring begins.

Only works on undirected graphs.

The returned "colors" are numbers starting from 0. The function also returns the total number of colors used.}

@defproc[(coloring/brelaz [g graph?]) (hash/c any/c number? #:immutable #f)]{
Returns a "greedy" coloring of the given graph, using the Brelaz vertex ordering heuristic. Note that this function is separate from @racket[coloring/greedy] because the ordering is determined in an online manner.}
          
@defproc[(order-smallest-last [g graph?]) list?]{
Consumes a graph and returns the vertices of the graph in "smallest-last" order. The ordering proceeds as follows. The vertex with the smallest degree is last. Then that vertex is removed and the degrees of the remaining vertices are updated. Then the second to last vertex has the lowest remaining degree and so on.

Only works for undirected graphs.}
                                                                
@defproc[(valid-coloring? [g graph?] [coloring (hash/c any/c number?)]) boolean?]{
Indicates whether the given coloring (a hash that maps a vertex to a color) is valid, meaning that no edge has two vertices of the same color.

This function assumes that a "color" is a number and uses @racket[=] to compare colors.}

@; max flow -------------------------------------------------------------------
@section{Maximum Flow}
@defproc[(maxflow [g graph?] [source any/c] [sink any/c])
         (hash/c (list/c any/c any/c) number?)]{
Computes the maximum flow in graph @racket[g] from @racket[source] to @racket[sink] using the Edmonds-Karp algorithm, which runs in time O(VE^2). Edmonds-Karp is an improvement on the Ford-Fulkerson algorithm in that it uses @racket[fewest-vertices-path] to find an augmenting path in each iteration.
                                   
The function returns a hash mapping an edge to a non-negative number representing the flow along that edge. The returned returned flow is maximal and obeys a few properties:
@itemize[@item{The flow out of the source equals the source into the sink.}
         @item{The flow out of each non-source/sink vertex equals the flow into that vertex.}
         @item{The flow along each edge is <= the edge's capacity (ie, weight).}]
                                   
This function should only be used on directed graphs, otherwise things get double counted.}
                                               
@defproc[(bipartite? [g graph?]) (or/c #f (list/c list? list?))]{
  Determines whether a graph is bipartite using a @racket[dfs]-based 2-@racket[coloring], thus runs in linear time. Returns @racket[#f] if the graph is not bipartite (ie, not 2-colorable), and other wise returns a list of two lists, where each sublist is the "left" and "right" sets of vertices, respectively, in the bipartite graph.}

@defproc[(maximum-bipartite-matching [g graph?]) (listof (list/c any/c any/c))]{
  Returns a list of edges representing the maximum matching of the given bipartite graph. An error is raised if the given graph is not bipartite. Uses @racket[maxflow] to compute the maximum matching.

Note: this is not the Hopcroft-Karp (ie fastest) bipartite matching algorithm.}

@; util fns -------------------------------------------------------------------
@section{Other Useful Graph Functions}
@defproc[(graphviz [g graph?] [#:colors colors boolean? #f]) string?]{
Returns the dotfile representation of the given graph (as a string).}

@(bibliography 
  (bib-entry #:key "GGCL"
             #:author "Lie-Quan Lee, Jeremy G. Siek, and Andrew Lumsdaine"
             #:title "The Generic Graph Component Library"
             #:location "OOPSLA"
             #:date "1999")
  (bib-entry #:key "tarjan-scc"
             #:author "Robert E. Tarjan"
             #:title "Depth first search and linear graph algorithms."
             #:location "SIAM Journal on Computing, 1(2):146-160"
             #:date "1972"))