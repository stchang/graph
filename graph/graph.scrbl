#lang scribble/manual
@(require scribble/eval
          (for-label graph
                     racket/contract/base
                     racket/generic
                     data/queue
                     math/matrix
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

@itemlist[
 
  @item{@defproc[(has-vertex? [g graph?] [v any/c]) boolean?]{Indicates whether a vertex is in the given graph.}}
  @item{@defproc[(has-edge? [g graph?] [u any/c] [v any/c]) boolean?]{Indicates whether an edge is in the given graph.}}
  @item{@defproc[(vertex=? [g graph?] [u any/c] [v any/c]) boolean?]{Indicates whether vertices @racket[u] and @racket[v] are the same vertex, according to graph @racket[g].}}
  @item{@defproc[(add-vertex! [g graph?] [v any/c]) void?]{Imperatively adds a vertex to a graph.}}
  @item{@defproc[(remove-vertex! [g graph?] [v any/c]) void?]{Imperatively removes a vertex from a graph. Any edges containing the vertex are also removed.}}
  @item{@defproc[(rename-vertex! [g graph?] [old any/c] [new any/c]) void?]{Imperatively renames vertex @racket[old] to @racket[new] in a graph. An exception is thrown if @racket[new] already exists in the graph.}}
  @item{@defproc[(add-edge! [g graph?] [u any/c] [v any/c] [weight any/c 'default-value]) void?]{Imperatively adds an undirected edge and optional weight to a graph. Each graph that implements the interface determines its own default weight value.}}
  @item{@defproc[(add-directed-edge! [g graph?] [u any/c] [v any/c] [weight any/c 'default-value]) void?]{Imperatively adds a directed, optionally weighted edge to a graph. Each graph that implements the interface determines its own default weight value.}}
  @item{@defproc[(remove-edge! [g graph?] [u any/c] [v any/c]) void?]{Imperatively removes the undirected edge.}}
  @item{@defproc[(remove-directed-edge! [g graph?] [u any/c] [v any/c]) void?]{Imperatively removes the directed edge.}}
  @item{@defproc[(get-vertices [g graph?]) list?]{Returns a list of vertices in the graph.}}
  @item{@defproc[(in-vertices [g graph?]) sequence?]{Returns a sequence of vertices in the graph.}}
  @item{@defproc[(get-neighbors [g graph?] [v any/c]) list?]{Returns a list of vertex @racket[v]'s neighbors in the graph.}}
  @item{@defproc[(in-neighbors [g graph?] [v any/c]) sequence?]{Returns a sequence of vertex @racket[v]'s neighbors in the graph.}}
  @item{@defproc[(get-edges [g graph?]) list?]{Returns a list of edges in the graph.}}
  @item{@defproc[(in-edges [g graph?]) sequence?]{Returns a sequence of edges in the graph.}}
  @item{@defproc[(edge-weight [g graph?] [u any/c] [v any/c]) number?]{Returns the weight of the edge in the graph (if it has one). Returns +inf.0 for non-existent edges.}}
  @item{@defproc[(transpose [g graph?]) graph?]{Returns a new graph where the edges of the original graph are reversed.}}
  @item{@defproc[(graph-copy [g graph?]) graph?]{Returns a copy of the given graph.}}
  ]


@; graph constructors ---------------------------------------------------------
@section{Graph Constructors}

@; unweighted graphs ----------------------------------------------------------
@subsection{Unweighted Graphs}

See also the @racket[directed-graph] and @racket[undirected-graph] constructors, 
which create either a weighted or unweighted graph.

@defproc[(unweighted-graph? [g any/c]) boolean?]{Indicates whether a graph is an unweighted graph.}

@defproc[(unweighted-graph/undirected [edges (listof (list/c any/c any/c))])
         (and/c graph? unweighted-graph?)]{
  Creates an unweighted graph that implements @racket[gen:graph] from a list of undirected edges. Each edge is represented by a list of two vertices. Vertices can be any Racket values comparable with @racket[equal?].
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
Creates an unweighted graph that implements @racket[gen:graph] from a list of directed edges. Each edge is represented by a list of two vertices, where the first vertex in the list is the source and the second is the destination. Vertices can be any Racket values comparable with @racket[equal?].
@examples[#:eval the-eval
  (define g (unweighted-graph/directed '((a b) (c d))))
  (graph? g)
  (unweighted-graph? g)
  (has-edge? g 'a 'b)
  (has-edge? g 'b 'a)
  ]}

@defproc[(unweighted-graph/adj [edges (listof list?)])
         (and/c graph? unweighted-graph?)]{
Creates an unweighted graph that implements @racket[gen:graph] from an adjacency list. Each element of the list is a list of vertices where the first vertex is the source and the rest are destinations. Vertices can be any Racket values comparable with @racket[equal?].
@examples[#:eval the-eval
  (define g (unweighted-graph/adj '((a b c) (b c d))))
  (graph? g)
  (unweighted-graph? g)
  (has-edge? g 'a 'b)
  (has-edge? g 'b 'a)
  ]}

             
@; weighted graphs ------------------------------------------------------------
@subsection{Weighted Graphs}

See also the @racket[directed-graph] and @racket[undirected-graph] constructors, 
which create either a weighted or unweighted graph.

@defproc[(weighted-graph? [g any/c]) boolean?]{Indicates whether a graph is a weighted graph.}

@defproc[(weighted-graph/undirected [edges (listof (list/c number? any/c any/c))])
         (and/c graph? weighted-graph?)]{
  Creates a weighted graph that implements @racket[gen:graph] from a list of weighted, undirected edges. Each edge is represented by a list of one number and two vertices, where the number is the weight. Vertices can be any Racket values comparable with @racket[equal?].
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
Creates a weighted graph that implements @racket[gen:graph] from a list of weighted, directed edges. Each edge is represented by a list of one number and two vertices, where the number is the weight, and the first vertex in the list is the source and the second vertex is the destination. Vertices can be any Racket values comparable with @racket[equal?]. Non-existent edges return an infinite weight, even for non-existent vertices.
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

@defproc[(undirected-graph [edges (listof (list/c number? any/c any/c))]
                           [wgts (listof number?) #f])
         (and/c graph? (or/c unweighted-graph? weighted-graph?))]{
  Creates either a weighted or unweighted graph that implements @racket[gen:graph] from a list of undirected edges and possibly a list of weights. Each edge is represented by a list of two vertices. Vertices can be any Racket values comparable with @racket[equal?]. If a list of weights is provided, then the result is a weighted graph. Otherwise, the result is an unweighted graph. The number of weights must equal the number of edges.
@examples[#:eval the-eval
  (define g (undirected-graph '((a b) (b c)) '(10 20)))
  (graph? g)
  (weighted-graph? g)
  (has-edge? g 'a 'b)
  (has-edge? g 'b 'a)
  (edge-weight g 'a 'b)
  (edge-weight g 'b 'a)
  (define g2 (undirected-graph '((a b) (b c))))
  (weighted-graph? g2)
  (unweighted-graph? g2)
  ]
}
                                          
@defproc[(directed-graph [edges (listof (list/c number? any/c any/c))]
                         [wgts (listof number?) #f])
         (and/c graph? (or/c unweighted-graph? weighted-graph?))]{
Creates either a weighted or unweighted graph that implements @racket[gen:graph] from a list of directed edges and possibly a list of weights. Each edge is represented by a list of two vertices where the first vertex in the list is the source and the second vertex is the destination. Vertices can be any Racket values comparable with @racket[equal?]. If a list of weights is provided, then the result is a weighted graph. Otherwise, the result is an unweighted graph. The number of weights must equal the number of edges. Non-existent edges return an infinite weight, even for non-existent vertices.
@examples[#:eval the-eval
  (define g (directed-graph '((a b) (b c)) '(10 20)))
  (graph? g)
  (weighted-graph? g)
  (has-edge? g 'a 'b)
  (has-edge? g 'b 'a)
  (edge-weight g 'a 'b)
  (edge-weight g 'b 'a)
  (edge-weight g 'b 'd)
  (define g2 (directed-graph '((a b) (b c))))
  (weighted-graph? g2)
  (unweighted-graph? g2)
  ]}


@; matrix graphs ------------------------------------------------------------
@subsection{Matrix Graphs}

@defproc[(matrix-graph? [g any/c]) boolean?]{Indicates whether a graph is a matrix graph.}

@defform[(matrix-graph [[expr ...+] ...+])]{
  Creates a matrix graph that implements @racket[gen:graph] from nested rows of 
expressions, exactly like @racket[matrix] form. Vertices are the (0-based) 
row/column numbers and the weights are the number at each row-column. Use 
@racket[#f] to indicate no edge.
                                         
NOTE: @racket[matrix-graph] is implemented with @racket[matrix], so the same typed-untyped performance caveats probably apply.

@examples[#:eval the-eval
  (define g (matrix-graph [[0 3 8 #f -4]
                           [#f 0 #f 1 7]
                           [#f 4 0 #f #f]
                           [2 #f -5 0 #f]
                           [#f #f #f 6 0]]))
  (graph? g)
  (matrix-graph? g)
  (has-edge? g 0 1)
  (has-edge? g 1 0)
  (edge-weight g 0 1)
  (edge-weight g 1 0)
  (floyd-warshall g)
  ]
}
                                          

@; graph properties -----------------------------------------------------------
@section{Graph properties}

The following forms associate properties with a graph. The graph itself is unchanged. These are inspired by and resemble the Boost Graph Library's @hyperlink["http://www.boost.org/doc/libs/1_42_0/libs/graph/doc/quick_tour.html"]{externally-stored properties}

@defform/subs[(define-vertex-property graph prop-name maybe-init maybe-vs)
              ([graph graph?]
               [prop-name identifier?]
               [maybe-init (code:line) (code:line #:init init-expr)]
               [init-expr expr]
               [maybe-vs (code:line) (code:line #:vs vs)]
               [vs list?])]{
  Creates a @deftech{vertex property} for vertices of the graph where each 
vertex maps to a value. This is a loose association, as keys of the mapping 
don't necessarily have to be vertices in the graph. 

Essentially, a vertex property is just a hash table with vertex keys that has
some convenient accessors. The accessors do not enforce
that vertices must be in the graph to support algorithms that make use of 
imaginary vertices.
  
  Defines the following names:
  
  @itemlist[
            
    @item{@racket[prop-name]: 
         A procedure that returns the value associated with the given vertices.
           @itemlist[
             @item{@racket[(prop-name v)]: Returns the value associated with the vertex @racket[v].}
             @item{@racket[(prop-name v #:default val)]: Returns the value associated with the given vertex. Returns @racket[val] if @racket[v] has no value associated with it.}
             ]}
    @item{@racket[prop-name]@racketidfont{->hash}: A no-argument function that returns a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{hash table} representation of the vertex-value mappings.}
    @item{@racket[prop-name]@racketidfont{-set!}: When given a vertex and a value, associates the value with the given vertex.}
    @item{@racket[prop-name]@racketidfont{-defined?}: Indicates whether the given vertex has an associated value.}
  ]
  
  If an @racket[#:init] argument is supplied, each vertex in the graph is 
  associated with the result of computing @racket[init-expr]. In the 
  @racket[init-expr], the identifier @racket[$v] may be used to refer to the 
  vertex whose value is being set. The @racket[init-expr] is evaluated only
  once. If vertices are added to the graph after the mapping is created, 
  those vertices will not have a value in the mapping until the setter is called.
  
  A @racket[#:vs] argument maybe supplied to specify the order in which the vertices are initialized. The @racket[vs] list is only used if there is also an @racket[#:init] expression.
  
  @examples[#:eval the-eval
   (define g (unweighted-graph/directed '((a b) (b c) (c d))))
   (define-vertex-property g dist #:init 0)
   (dist 'a)
   (dist 'non-exist)
   (dist 'non-exist #:default #f)
   (dist-set! 'a 100)
   (dist 'a)
   (dist->hash)
   (define-vertex-property g itself #:init $v)
   (itself 'a)
  ]
}

@defform/subs[(define-edge-property graph prop-name maybe-init maybe-for-each)
              ([graph graph?]
               [prop-name identifier?]
               [maybe-init (code:line) (code:line #:init init-expr)]
               [init-expr expr]
               [maybe-vs (code:line) (code:line #:for-each for-each-e ...)]
               [for-each-e expr])]{
  Creates an @deftech{edge property} for @emph{all possible} edges in the graph
 (ie, every pair of vertices) where each edge maps to a value.
  
 Essentially, an edge property is just a hash table with edges as keys that has
some convenient accessors. The accessors do not enforce
that edges must be in the graph to support algorithms that make use of 
imaginary edges.

  Defines the following names:
  
  @itemlist[
            
    @item{@racket[prop-name]: 
           A procedure that returns the value associated with the given vertices.
           @itemlist[
             @item{@racket[(prop-name u v)]: Returns the value associated with the pair of vertices.}
             @item{@racket[(prop-name u v #:default val)]: Returns the value associated with the given pair of vertices. Returns @racket[val] if edge @racket[u]-@racket[v] has no value associated with it.}
             ]}
    @item{@racket[prop-name]@racketidfont{->hash}: A no-argument function that returns a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{hash table} representation of the edge-value mappings.}
    @item{@racket[prop-name]@racketidfont{-set!}: When given an edge (ie two vertices) and a value, associates the value with the given edge.}
  ]
  
  If an @racket[#:init] argument is supplied, each pair of vertices in the graph is associated with the result of computing @racket[init-expr]. In the @racket[init-expr], the identifiers @racket[$from] and @racket[$to] may be used to refer to the vertices whose value is being set.
  
  The @racket[#:for-each] keyword may be used instead of @racket[#:init] when more control of the initialization is required. The @racket[for-each-e] expressions are evaluated once per pair of vertices. The @racket[$from] and @racket[$to] identifiers are again available to refer to the vertices in the "current" edge.
  
  @examples[#:eval the-eval
   (define g (unweighted-graph/directed '((a b) (b c) (c d) (d a))))
   (define-edge-property g A #:init 0)
   (A 'a 'b)
   ]
  
  Non-edges also get a value.
  @examples[#:eval the-eval
   (get-edges g)
   (A 'a 'd)]
  But non-existent vertices still fails.
  @examples[#:eval the-eval
   (A 'a 'e)
   (A 'a 'e #:default #f)]
  @examples[#:eval the-eval
   (A-set! 'a 'b 100)
   (A 'a 'b)
   (define-edge-property g C #:init 100)
   (C->hash)
   (define-edge-property g B #:for-each (B-set! $from $to (format "~a-~a" $from $to)))
   (B 'c 'd)
   (B->hash)]
}


@; basic graph fns ------------------------------------------------------------

@section{Basic Graph Functions}

@; bfs ------------------------------------------------------------------------
@subsection{Breadth-first Search}

@defproc[(bfs [g graph?] [source any/c])
         (values (hash/c any/c number? #:immutable #f) (hash/c any/c any/c #:immutable #f))]{
 Standard textbook breadth-first search, ie as @cite["CLRS"]. Takes two arguments, a graph and a source vertex. Returns two values, a hash table mapping a vertex to the distance (in terms of number of vertices) from the source, and a hash table mapping a vertex to its predecessor in the search.}
                                                                                            
@defproc[(bfs/generalized 
          [g graph?] [source any/c]
          [#:init-queue Q queue? (mk-empty-fifo)]
          [#:break break? (-> graph? [source any/c] [from any/c] [to any/c] boolean?) (λ _ #f)]
          [#:init init (-> graph? [source any/c] void?) void]
          [#:visit? custom-visit?-fn (-> graph? [source any/c] [from any/c] [to any/c] boolean?) #f]
          [#:discover discover (-> graph? [source any/c] [from any/c] [to any/c] [acc any/c] any/c)
                      (λ (G s u v acc) acc)]
          [#:visit visit (-> graph? [source any/c] [v any/c] [acc any/c] any/c)
                   (λ (G s v acc) acc)]
          [#:return finish (-> graph? [source any/c] [acc any/c] any) (λ (G s acc) acc)]) any]{
Generalized breadth-first search. Partially inspired by the C++ Boost Graph 
Library. See Lee et al. OOPSLA 1999 @cite["GGCL"]. Here is the rough implementation:
@racketblock[  
  (enqueue! Q s)
  (mark-discovered! s)
  (define new-acc
    (for/fold ([acc (init G s)]) ([u (in-queue Q)])
      (for ([inner-acc (visit G s u acc)])
        ([v (in-neighbors G u)] #:when (visit? G s u v) #:break (break? G s u v))
        (mark-discovered! v)
        (begin0 (discover G s u v inner-acc)
                (enqueue! Q v)))))
  (finish G s new-acc)]
Utilizes a queue that implements @racket[gen:queue]. A vertex is 
@deftech{discovered} when it gets added to the queue. If no 
@racket[custom-visit?-fn] is provided, then the default behavior is to not 
enqueue/visit vertices that have already been discovered (ie seen). The function
checks the @racket[#:break] condition when a vertex is discovered (ie being 
considered for enqueueing). When a vertex is dequeued,
then @racket[visit] is called. The result of @racket[bfs/generalized] is the result 
of @racket[finish].

An accumulator is threaded through all the functions and returned at the end.
The initial accumulator value is the result of applying @racket[init]. By 
default the accumulator is threaded through @racket[discover], @racket[visit], 
and is returned via @racket[finish].

Accumulator history: @history[#:added "0.2"]}

@defform/subs[(do-bfs graph source maybe-init-queue maybe-break
                      maybe-init maybe-visit? maybe-discover maybe-visit maybe-return)
              ([graph graph?]
               [maybe-init-queue (code:line) 
                                 (code:line #:init-queue queue)
                                 (code:line #:init-queue: queue)]
               [queue queue?]
               [maybe-break (code:line) 
                            (code:line #:break (from to) break-exp ...)
                            (code:line #:break: break-exp ...)]
               [break-exp expr]
               [maybe-init (code:line) 
                           (code:line #:init init-exp ...)
                           (code:line #:init: init-exp ...)]
               [init-exp expr]
               [maybe-visit? (code:line) 
                             (code:line #:visit? (from to) visit?-exp ...)
                             (code:line #:visit?: visit?-exp ...)
                             (code:line #:enqueue? (from to) enq?-exp ...)
                             (code:line #:enqueue?: enq?-exp ...)]
               [visit?-exp expr]
               [enq?-exp expr]
               [maybe-discover (code:line)
                               (code:line #:discover (from to) discover-exp ...)
                               (code:line #:discover (from to acc) discover-exp ...)
                               (code:line #:discover: discover-exp ...)
                               (code:line #:on-enqueue (from to) enq-exp ...)
                               (code:line #:on-enqueue (from to acc) enq-exp ...)
                               (code:line #:on-enqueue: enq-exp ...)]
               [discover-exp expr]
               [enq-exp expr]
               [maybe-visit (code:line) 
                            (code:line #:visit (v) visit-exp ...)
                            (code:line #:visit (v acc) visit-exp ...)
                            (code:line #:visit: visit-exp ...)
                            (code:line #:on-dequeue (v) deq-exp ...)
                            (code:line #:on-dequeue (v acc) deq-exp ...)
                            (code:line #:on-dequeue: deq-exp ...)]
               [visit-exp expr]
               [deq-exp expr]
               [maybe-return (code:line) 
                             (code:line #:return (acc) return-exp ...)
                             (code:line #:return: return-exp ...)]
               [return-exp expr]
               [from identifier?] [to identifier?] [v identifier?] [acc identifier?])]{
Cleaner syntax for @racket[bfs/generalized]. Essentially, this form eliminates 
the need to define separate functions and then pass them as
@racket[bfs/generalized]'s keyword arguments. Instead, the bodies of those 
functions are inserted right after the corresponding keywords.

Each possible keyword has a colon-suffixed and non-colon-suffixed version. The
non-colon-suffixed versions bind user-supplied identifiers. For example, the
keywords @racket[#:break], @racket[#:visit?], @racket[#:discover], and 
@racket[#:visit] bind identifiers that represent the vertices under
consideration. For some keywords, the accumulator may also be named. 
If the accumulator is unnamed, then it is implicitly passed through unchanged.
If the accumulator is named, then the result of evaluating the body becomes
the new accumulator value.

In the body of colon-suffixed keywords, implicit special identifiers refer 
to the vertices under consideration. Specifically, @racket[$v] is bound to the 
current vertex and @racket[$from] is its parent (when appropriate). A 
@racket[$to] identifier has the same value as @racket[$v], in case that name 
makes more sense in the context of the program. 
The special @racket[$acc] identifier represents the accumulator value.

Colon-suffixed keyword history: @history[#:added "0.3"]}

The keywords that don't bind any names (@racket[#:init-queue] and @racket[#:init])
have both colon and non-colon versions, to have a consistent naming scheme.

The keywords @racket[#:visit?], @racket[#:discover], and @racket[#:visit] also
have alternate names, @racket[#:enqueue?], @racket[#:on-enqueue], and 
@racket[#:on-dequeue], respectively, which can be used when these names are more
appropriate, for program readability.

In any of the keyword arguments, the special identifiers @racket[$discovered?],
@racket[$seen?], and @racket[$visited?] are also available, where 
@racket[($seen? v)] indicates whether the vertex has been seen, ie whether the 
vertex has ever been added to the queue (@racket[($discovered? v)] is the same 
as @racket[$seen?]), and @racket[($visited? v)] indicates whether the vertex has
been visited, ie pulled off the queue.

In the @racket[#:return] expressions, the @racket[$broke?] special identifier
indicates whether the search was terminated early according to the 
@racket[#:break] condition.

For example, below is Dijkstra's algorithm, implemented with @racket[do-bfs]. 
The code defines two @tech{vertex property}s: @racket[d] maps a vertex to the 
currently known shortest distance from the source and @racket[π] maps a vertex 
to its predecessor in the shortest path from the source. The algorithm utilizes 
a priority queue (ie, heap) that is sorted according to intermediate shortest 
paths. A node is added to the heap when it improves one of the paths. The 
algorithm makes use of the special identifiers @racket[$v] and @racket[$from].

@#reader scribble/comment-reader (racketblock
(define (dijkstra G src) 
  (define-vertex-property G d #:init +inf.0) ; length of currently known shortest path
  (define-vertex-property G π #:init #f) ; (π v) is predecessor of v on shortest path
  (define (wgt u v) (edge-weight G u v))

  (do-bfs G src #:init-queue (mk-empty-priority (λ (u v) (< (d u) (d v))))
    #:init (d-set! src 0)
    #:enqueue?: (> (d $v) (+ (d $from) (wgt $from $v)))
    #:on-enqueue: 
      (d-set! $v (+ (d $from) (wgt $from $v)))
      (π-set! $v $from)
    #:return: (values (d->hash) (π->hash))))
    )

@racket[bfs], @racket[fewest-vertices-path], @racket[cc/bfs], @racket[mst-prim], 
and @racket[dijkstra] all use the @racket[do-bfs] form.}
               
@defproc[(fewest-vertices-path [G graph?] [source any/c] [target any/c]) (or/c list? #f)]{
Consumes a graph and two vertices, and returns the shortest path (in terms of number of vertices) between the two vertices. The result is a list of vertices, or @racket[#f] if there is no path.}

@; dfs ------------------------------------------------------------------------
@subsection{Depth-first Search}

@defproc[(dfs [g graph?])
         (values (hash/c any/c number? #:immutable #f) (hash/c any/c any/c #:immutable #f)
                 (hash/c any/c number? #:immutable #f))]{
Standard textbook depth-first search algorith, ie like in @cite["CLRS"]. 
Consumes a graph and returns three hashes: one that maps a vertex to its 
"discovery time", another that maps a vertex to its predecessor in the search, 
and a third that maps a vertex to its "finishing time".}
                                                        
                                                        
@defproc[(dfs/generalized 
          [g graph?]
          [#:order order (-> list? list?) (λ (x) x)]
          [#:break break? (-> graph? [from any/c] [to any/c] boolean?) (λ _ #f)]
          [#:init init (-> graph? void?) void]
          [#:inner-init inner-init (-> any/c any/c) (λ (acc) acc)]
          [#:visit? custom-visit?-fn (-> graph? [from any/c] [to any/c] boolean?) #f]
          [#:prologue prologue (-> graph? [from any/c] [v any/c] [acc any/c] any/c) (λ (G u v acc) acc)]
          [#:epilogue epilogue (-> graph? [from any/c] [v any/c] [acc any/c] any/c) (λ (G u v acc) acc)]
          [#:process-unvisited? process-unvisited? 
                                (-> graph? [from any/c] [to any/c] boolean?) (λ _ #f)]
          [#:process-unvisited process-unvisited
                               (-> graph? [from any/c] [to any/c] [acc any/c] any/c) (λ (G u v acc) acc)]
          [#:combine combine (-> [x any/c] [acc any/c] any/c) (λ (x acc) x)]
          [#:return finish (-> graph? [acc any/c] any/c) (λ (G acc) acc)]) any]{
Note: You probably want to use the nicer @racket[do-dfs] form instead of this function.
                            
Generalized depth-first search. Partially inspired by the C++ Boost Graph 
Library. See Lee et al. OOPSLA 1999 @cite["GGCL"]. Here is the rough 
implementation:

@#reader scribble/comment-reader (racketblock
  ;; inner loop: keep following (unvisited) links
  (define (do-visit parent u acc)
    (mark-visited! u)
    (define new-acc
      (for/fold ([acc (prologue G parent u acc)])
                ([v (in-neighbors G u)] #:break (break? G u v))
        (cond [(visit? G u v) (do-visit u v acc)]
              [(process-unvisited? G u v) (process-unvisited G u v acc)]
              [else acc])))
    (epilogue G parent u new-acc))
  
  ;; outer loop: picks a new vertex to continue with when search reaches dead end
  (define new-acc 
    (for/fold ([acc (init G)]) 
              ([u (order (get-vertices G))] #:break (break? G #f u))
      (cond [(visit? G #f u) (combine (do-visit #f u (inner-init acc)) acc)]
            [(process-unvisited? G #f u) (process-unvisited G #f u acc)]
            [else acc])))
  
  (finish G new-acc))

The @racket[do-visit] function is the inner loop that keeps following edges, 
depth-first, until the search gets stuck. The "visit" part of the code is 
separated into two functions, the @racket[prologue], which represents the 
descending part of the visit, and @racket[epilogue], which gets called on the 
way back up. The outer @racket[for/fold] loop picks a new vertex to restart the 
search when it gets stuck. Vertices that are already visited are marked and are 
not visited again by default, but the @racket[custom-visit?-fn] can override this. 
The algorithm terminates when all vertices are visited, or 
the @racket[#:break] condition is triggered. The 
result of @racket[dfs/generalized] is the result of @racket[finish]. 

An accumulator is threaded through all the functions and returned at the end.
The initial accumulator value is the result of applying @racket[init]. There is
another @racket[inner-init] value that starts a new sub-accumulator each time the
outer @racket[for/fold] loop iterates. The optional @racket[combine] function
combines the result of each outer loop iteration with the overall accumulator.
See the implementation of @racket[cc] for an example of where this is useful.
If a different @racket[inner-init] is unneeded, it defaults to the same value as
the overall accumulator.

Accumulator history: @history[#:added "0.2"]

If an @racket[order] function is supplied, the outer loop uses it to sort the
vertices before determining which vertex to visit next. The @racket[break?] 
predicate aborts the search and returns when it is true. 
@racket[process-unvisited?] and @racket[process-unvisited] specify code to run 
when a vertex is not visited.

The functions that require both a "from" and a "to" vertex argument are given 
@racket[#f] for the "from" (ie the parent) when there is none.
}

@defform/subs[(do-dfs graph maybe-order maybe-break maybe-init maybe-inner-init 
                      maybe-visit? maybe-prologue maybe-epilogue 
                      maybe-process-unvisited? maybe-process-unvisited maybe-combine maybe-return)
              ([graph graph?]
               [maybe-order (code:line) 
                            (code:line #:order order)
                            (code:line #:order: order)]
               [order (-> list? list?)]
               [maybe-break (code:line) 
                            (code:line #:break (from to) break-exp ...)
                            (code:line #:break (from to acc) break-exp ...)
                            (code:line #:break: break-exp ...)]
               [break-exp expr]
               [maybe-init (code:line) 
                           (code:line #:init init-exp ...)
                           (code:line #:init: init-exp ...)]
               [init-exp expr]
               [maybe-inner-init (code:line) 
                                 (code:line #:inner-init iinit-exp ...)
                                 (code:line #:inner-init: iinit-exp ...)]
               [iinit-exp expr]
               [maybe-visit? (code:line)
                             (code:line #:visit? (from to) visit?-exp ...)
                             (code:line #:visit?: visit?-exp ...)]
               [visit?-exp expr]
               [maybe-prologue (code:line) 
                               (code:line #:prologue (from to) prologue-exp ...)
                               (code:line #:prologue (from to acc) prologue-exp ...)
                               (code:line #:prologue: prologue-exp ...)]
               [prologue-exp expr]
               [maybe-epilogue (code:line) 
                               (code:line #:epilogue (from to) epilogue-exp ...)
                               (code:line #:epilogue (from to acc) epilogue-exp ...)
                               (code:line #:epilogue: epilogue-exp ...)]
               [epilogue-exp expr]
               [maybe-process-unvisited? 
                (code:line) 
                (code:line #:process-unvisited? (from to) process-unvisited?-exp ...)
                (code:line #:process-unvisited?: process-unvisited?-exp ...)]
               [process-unvisited?-exp expr]
               [maybe-process-unvisited
                (code:line) 
                (code:line #:process-unvisited (from to) process-unvisited-exp ...)
                (code:line #:process-unvisited (from to acc) process-unvisited-exp ...)
                (code:line #:process-unvisited: process-unvisited-exp ...)]
               [process-unvisited-exp expr]
               [maybe-combine (code:line) 
                              (code:line #:combine combine-fn)
                              (code:line #:combine: combine-fn)]
               [maybe-return (code:line) 
                             (code:line #:return () return-exp ...)
                             (code:line #:return (acc) return-exp ...)
                             (code:line #:return: return-exp ...)]
               [return-exp expr]
               [from identifier?] [to identifier?] [v identifier?] [acc identifier?])]{
Analogous to @racket[do-bfs]. Nicer syntax for @racket[dfs/generalized]. 
Essentially, this form eliminates the need to define separate functions and 
then pass them as @racket[dfs/generalized]'s keyword arguments. Instead, the 
bodies of those functions are inserted right after the corresponding keywords.

Each possible keyword has a colon-suffixed and non-colon-suffixed version. The
non-colon-suffixed versions bind user-supplied identifiers. For example, the
keywords @racket[#:break], @racket[#:visit?], @racket[#:prologue],
@racket[#:epilogue], @racket[#:process-unvisited?], and @racket[#:process-unvisited]
bind identifiers that represent the vertices under consideration. For some keywords, 
the accumulator may also be named. 
If the accumulator is unnamed, then it is implicitly passed through unchanged.
If the accumulator is named, then the result of evaluating the body becomes
the new accumulator value.

In the body of colon-suffixed keywords, implicit special identifiers refer
to the vertices under consideration. Specifically, @racket[$v] is bound to the 
current vertex and @racket[$from] is its parent (when appropriate). A 
@racket[$to] identifier has the same value as @racket[$v], in case that name 
makes more sense in the context of the program. 
The special @racket[$acc] identifier represents the accumulator value.

Colon-suffixed keyword history: @history[#:added "0.3"]}

The keywords that don't bind any names (@racket[#:order], @racket[#:init], 
@racket[#:iinit], and @racket[#:combine])
have both colon and non-colon versions, to have a consistent naming scheme.

In the @racket[#:return] expressions, the @racket[$broke?] special identifier
indicates whether the search was terminated early according to the 
@racket[#:break] condition.

As an example, here is a possible definition for  @racket[dag?], 
a predicate that indicates whether a graph is directed and 
acyclic. The function uses the classic three vertex coloring, where white 
indicates unseen, gray indicates discovered, and black indicates done. 
Encountering a gray node while searching indicates a cycle so this is 
checked as the @racket[#:break] condition. The implementation of @racket[dag?]
uses the special @racket[$v] and @racket[$broke?] identifiers.
             
@#reader scribble/comment-reader (racketblock
(define (dag? G)
  (define-vertex-property G color #:init WHITE)
  (do-dfs G 
   #:break: (gray? (color $v)) ; seeing a gray vertex means a loop
   #:visit?: (white? (color $v))
   #:prologue: (color-set! $v GRAY)
   #:epilogue: (color-set! $v BLACK)
   #:return: (not $broke?))) ; didnt break means no loop = acyclic
)

Here is a possible implementation of @racket[tsort], ie topological sort.
It maintains the sorted vertices as an accmulator and ads vertices to the result
on the way back up the depth-first search.

@#reader scribble/comment-reader (racketblock
(define (tsort G)
  (do-dfs G #:init null
            #:epilogue: (cons $v $acc)))
)

@racket[dfs], @racket[tsort], @racket[dag?], @racket[cc], and @racket[scc] all 
use the @racket[do-dfs] form.}
                                                                                         
@defproc[(dag? [g graph?]) boolean?]{
Indicates whether a graph is directed and acyclic.}

@defproc[(tsort [g graph?]) list?]{Returns the vertices in the given graph, topologically sorted. Note that the returned order may not be the only valid ordering.}

@defproc[(cc [g graph?]) (listof list?)]{Calculates the connected components of graph @racket[g]. @racket[g] should be an undirected graph. Returns a list of lists of vertices, where each sublist is a connected component.

@history[#:added "0.2"]}

@defproc[(cc/bfs [g graph?]) (listof list?)]{Calculates the connected components of graph @racket[g] using @racket[do-bfs]. @racket[g] should be an undirected graph. Returns a list of lists of vertices, where each sublist is a connected component.

@history[#:added "0.2"]}

@defproc[(scc [g graph?]) (listof list?)]{Calculates the strongly connected components of a graph using Tarjan's algorithm @cite{tarjan-scc}. @racket[g] should be a directed graph. Returns a list of lists of vertices, where each sublist is a strongly connected subgraph.}
                                       
@; min span trees -------------------------------------------------------------
@section{Minimum Spanning Tree}

@defproc[(mst-kruskal [g graph?]) (listof (list/c any/c any/c))]{Computes the minimum spanning tree using Kruskal's algorithm and the @racket[data/union-find] data structure. Returns a list of edges.}

@defproc[(mst-prim [g graph?] [source any/c]) (listof (list/c any/c any/c))]{Computes the minimum spanning tree of a graph using Prim's algorithm, which is based on breadth-first search. Returns a list of edges.}

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
@itemlist[
  @item{The flow out of the source equals the source into the sink.}
  @item{The flow out of each non-source/sink vertex equals the flow into that vertex.}
  @item{The flow along each edge is <= the edge's capacity (ie, weight).}]
                                   
This function should only be used on directed graphs, otherwise things get double counted.}
                                               
@defproc[(bipartite? [g graph?]) (or/c #f (list/c list? list?))]{
  Determines whether a graph is bipartite using a @racket[dfs]-based 2-@racket[coloring], thus runs in linear time. Returns @racket[#f] if the graph is not bipartite (ie, not 2-colorable), and other wise returns a list of two lists, where each sublist is the "left" and "right" sets of vertices, respectively, in the bipartite graph.}

@defproc[(maximum-bipartite-matching [g graph?]) (listof (list/c any/c any/c))]{
  Returns a list of edges representing the maximum matching of the given bipartite graph. An error is raised if the given graph is not bipartite. Uses @racket[maxflow] to compute the maximum matching.

Note: this is not the Hopcroft-Karp (ie fastest) bipartite matching algorithm.}

@; util fns -------------------------------------------------------------------
@section{Graphviz}
@defproc[(graphviz [g graph?] [#:colors colors boolean? #f]) string?]{
Returns the dotfile representation of the given graph (as a string).}

@; other
@section{Other}
@defthing[$v identifier?]{An identifier that's bound to the "current" vertex in certain contexts. 
              
              See @racket[define-vertex-property], @racket[do-bfs], and @racket[do-dfs].}

@defthing[$from identifier?]{An identifier that's bound to the "from" vertex in certain contexts. 
              
              See @racket[define-edge-property], @racket[do-bfs], and @racket[do-dfs].}
@defthing[$to identifier?]{An identifier that's bound to the "to" vertex certain contexts. 
              
              See @racket[define-edge-property], @racket[do-bfs], and @racket[do-dfs].}
@defthing[$discovered? identifier?]{A function that queries the "seen" vertices in certain contexts. 
              
              See @racket[do-bfs].}
@defthing[$seen? identifier?]{A function that queries the "seen" vertices in certain contexts. 
                              Same as @racket[$discovered?].
              
              See @racket[do-bfs].}
@defthing[$visited? identifier?]{A function that queries the "visited" vertices in certain contexts. 
              
              See @racket[do-bfs].}
@defthing[$broke? identifier?]{Indicates whether the @racket[#:break] condition was triggered in some contexts.
              
              See @racket[do-bfs] and @racket[do-dfs].}
@defthing[$acc identifier?]{Represents an accumulator value in some contexts.
              
              See @racket[do-dfs].
              @history[#:added "0.2"]}



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
             #:date "1972")
  (bib-entry #:key "CLRS"
             #:author "Charles E. Leiserson, Clifford Stein, Thomas H. Cormen, and Ronald Rivest"
             #:title "Introduction to Algorithms, 2nd ed."
             #:is-book? #t
             #:date "2001"))