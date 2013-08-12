#lang scribble/manual
@(require scribble/eval
          (for-label "main.rkt" 
                     racket/contract/base
                     racket))

@title{Racket Generic Graph Library}

@defmodule[graph]

@(define the-eval (make-base-eval))
@(the-eval '(require "main.rkt"))

@author[@author+email["Stephen Chang" "stchang@racket-lang.org"]]

@; generic interface ----------------------------------------------------------
@section{Generic Graph Interface}

@defthing[gen:graph any/c]{
  A @tech{generic interface} (see @secref["struct-generics"]) that defines a @deftech{graph}. To supply method implementations, a struct should use the @racket[#:methods] form. A @tech{graph} has the following methods:

@itemize[
 
  @item{@racket[has-vertex?]: Accepts two arguments, a graph and a vertex. Indicates whether the vertex is in the graph.}
  @item{@racket[has-edge?]: Accepts threes arguments, a graph and two vertices. Indicates whether the edge is in the graph.}
  @item{@racket[add-vertex!]: Accepts two arguments, a graph and a vertex. Imperatively adds the vertex to the graph.}
  @item{@racket[add-edge!]: Accepts three or four arguments, a graph, two vertices, and an optional weight value. Imperatively adds the undirected edge comprised of the two vertices and the optional weight to the graph.}
  @item{@racket[add-directed-edge!]: Accepts three or four arguments, a graph, source and destination vertices, and an optional weight value. Imperatively adds to the graph the directed, optionally weighted edge going from the source to the destination.}
  @item{@racket[in-vertices]: Accepts one argument, a graph. Returns a list whose elements are the vertices of the graph.}
  @item{@racket[in-neighbors]: Accepts two arguments, a graph and a vertex. Returns a sequence whose elements are the vertex's neighbors in the graph.}
  @item{@racket[in-edges]: Accepts one argument, a graph. Returns a sequence whose elements are the edges of the graph.}
  @item{@racket[edge-weight]: Accepts three arguments, a graph and two vertices. Returns the weight of the edge in the graph (if it has one).}
  ]
}

@defproc[(graph? [g any/c]) boolean?]{
  Returns @racket[#t] if @racket[g] is a @tech{graph} (ie, implements @racket[gen:graph]) and @racket[#f] otherwise.
}

@; graph constructors ---------------------------------------------------------
@section{Graph Constructors}

@; unweighted graphs ----------------------------------------------------------
@subsection{Unweighted Graphs}

@defproc[(unweighted-graph? [g any/c]) boolean?]{Indicates whether a graph is an unweighted graph.}

@defproc[(unweighted-graph/undirected [edges (list/c (list/c any/c any/c) ...)])
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
                                          
@defproc[(unweighted-graph/directed [edges (list/c (list/c any/c any/c) ...)])
         (and/c graph? unweighted-graph?)]{
Creates an unweighted graph that implements @racket[gen:graph] from a list of directed edges. Each edge is represented by a list of two vertices, where the first vertex in the list is the source and the second is the destination. Vertices can be any racket values comparable with @racket[equal?].
@examples[#:eval the-eval
  (define g (unweighted-graph/directed '((a b) (c d))))
  (graph? g)
  (unweighted-graph? g)
  (has-edge? g 'a 'b)
  (has-edge? g 'b 'a)
  ]}

@defproc[(unweighted-graph/adj [edges (list/c (list/c any/c ...) ...)])
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

@defproc[(weighted-graph/undirected [edges (list/c (list/c number? any/c any/c) ...)])
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
                                          
@defproc[(weighted-graph/directed [edges (list/c (list/c number? any/c any/c) ...)])
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


@section{Basic Graph Functions}

@defproc[(bfs [g graph?] [source any/c])
         (values (hash/c any/c number? #:immutable #f) (hash/c any/c any/c #:immutable #f))]{
 Standard textbook breadth-first search, ie as in CLRS. Takes two arguments, a graph and a source vertex. Returns two values, a hash table mapping a vertex to the distance (in terms of number of vertices) from the source, and a hash table mapping a vertex to its predecessor in the search.}