/*
    wgraphs are the weighted equivalent of ugraphs.

*/
:- module(wgraphs, [
    ugraph_breadth_first_search/4,
    ugraph_graphviz//1,
    ugraph_to_wgraph/2,
    vertices_edges_to_wgraph/3,
    wgraph_add_edges/3,
    wgraph_add_vertices/3,
    wgraph_breadth_first_search/4,
    % wgraph_complement/2,
    % wgraph_compose/3,
    wgraph_cycles/3,
    % wgraph_del_edges/3,
    wgraph_del_vertices/3,
    wgraph_gen/4,
    wgraph_graphviz//1,
    wgraph_edges/2,
    wgraph_neighbors/3,
    wgraph_neighbours/3,
    % wgraph_reachable/3,
    % wgraph_transitive_closure/2,
    % wgraph_transpose/2,
    wgraph_to_ugraph/2,
    % wgraph_union/3,
    wgraph_vertices/2,
    wgraph_weight/4
]).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

%!  ugraph_breadth_first_search(+Graph:ugraph, +Starts:list, -End, -Path:nonneg) is nondet.
%!  ugraph_breadth_first_search(+Graph:ugraph, +Starts:list, +End, -Path:nonneg) is semidet.
ugraph_breadth_first_search(Graph, Starts, End, Path) :-
    must_be(list, Graph),
    must_be(list, Starts),
    sort(Starts, Sorted),
    vertices(Graph, Vertices),
    ord_subset(Sorted, Vertices),
    Path in 0..sup,
    (   ground(End)
    ->  once(ugraph_breadth_first_search_(Graph, Sorted, End, 0, Path))
    ;   ugraph_breadth_first_search_(Graph, Sorted, End, 0, Path)
    ).

ugraph_breadth_first_search_(_,      Heres, Here, Path, Path) :- member(Here, Heres).
ugraph_breadth_first_search_(Graph0, Heres, End, Path0, Path) :-
    Path0 #< Path,
    Heres \== [],
    Path1 #= Path0 + 1,
    maplist({Graph0}/[V0, Vs] >> neighbours(V0, Graph0, Vs), Heres, Theres0),
    ord_union(Theres0, Theres1),
    ord_subtract(Theres1, Heres, Theres),
    del_vertices(Graph0, Heres, Graph),
    ugraph_breadth_first_search_(Graph, Theres, End, Path1, Path).

ugraph_graphviz(Graph) -->
    "digraph {", eol, sequence(ugraph_graphviz_edges, Graph), "}", eol.

ugraph_graphviz_edges(A - Bs) -->
    graphviz_node(A), " -> {", sequence(graphviz_node, " ", Bs), "};", eol.

graphviz_node(Node) --> { atom_codes(Node, Codes) }, Codes.

%!  ugraph_to_wgraph(+Graph, ?WGraph) is det.
%   Converts an unweighted graph to weighted graph with every edge weight equal to 1.
ugraph_to_wgraph --> maplist([A - Bs0, A - Bs] >> maplist([B, B - 1] >> true, Bs0, Bs)).

%!  vertices_edges_to_wgraph(+Vertices:list, +Edges:list, -WGraph) is det.
%   Edges is a list of Source - (Destination - Weight).
vertices_edges_to_wgraph(Vertices, Edges, WGraph) :-
    wgraph_add_vertices([], Vertices, WGraph0),
    wgraph_add_edges(WGraph0, Edges, WGraph).

wgraph_add_edge(A - (B - Weight), Graph0, Graph) :-
    (   select(A - BWeights0, Graph0, Graph1)
    ->  once((
            select(B - _, BWeights0, BWeights1)
        ;   BWeights0 = BWeights1
        )),
        ord_add_element(BWeights1, B - Weight, BWeights),
        ord_add_element(Graph1, A - BWeights, Graph2)
    ;   ord_add_element(Graph0, A - [B - Weight], Graph2)
    ),
    (   memberchk(B - _, Graph2)
    ->  Graph2 = Graph
    ;   ord_add_element(Graph2, B - [], Graph)
    ).

%!  wgraph_add_edges(+WGraph0, +Edges:list, ?WGraph) is det.
%   Edges is a list of Source - (Destination - Weight).
wgraph_add_edges(WGraph0, Edges, WGraph) :-
    sort(0, @<, Edges, Sorted),
    wgraph_add_sorted_edges(WGraph0, Sorted, WGraph).

wgraph_add_sorted_edges(WGraph, [], WGraph) :- !.

wgraph_add_sorted_edges([], [V - W | Edges], WGraph) :-
    wgraph_add_sorted_edges([V - [W]], Edges, WGraph).

wgraph_add_sorted_edges([V0 - Ws | WG0], [V1 - W | Edges], WGraph) :-
    compare(Order, V0, V1),
    wgraph_add_sorted_edges_(Order, [V0 - Ws | WG0], [V1 - W | Edges], WGraph).

wgraph_add_sorted_edges_(<, [VWs0 | WG0], Edges, [VWs0 | WG]) :-
    wgraph_add_sorted_edges(WG0, Edges, WG).

wgraph_add_sorted_edges_(=, [V - Ws0 | WG0], [V - W | Edges], WG) :-
    ord_add_element(Ws0, W, Ws),
    wgraph_add_sorted_edges([V - Ws | WG0], Edges, WG).

wgraph_add_sorted_edges_(>, WG0, [V - W | Edges], [V - [W] | WG]) :-
    wgraph_add_sorted_edges(WG0, Edges, WG).

%!  wgraph_add_vertices(+WGraph0, +Vertices:list, ?WGraph) is det.
wgraph_add_vertices(WGraph0, Vertices, WGraph) :-
    sort(Vertices, Sorted),
    wgraph_add_sorted_vertices(WGraph0, Sorted, WGraph).

wgraph_add_sorted_vertices(Graph, [], Graph) :- !.

wgraph_add_sorted_vertices([], Vertices, WGraph) :-
    pairs_keys_values(WGraph, Vertices, Values),
    maplist(=([]), Values).

wgraph_add_sorted_vertices([V0 - DWs | WG0], [V1 | Vs], Graph) :-
    compare(Order, V0, V1),
    wgraph_add_sorted_vertices(Order, [V0 - DWs | WG0], [V1 | Vs], Graph).

wgraph_add_sorted_vertices(<, [Es | WG0], Vs, [Es | WG]) :-
    wgraph_add_sorted_vertices(WG0, Vs, WG).

wgraph_add_sorted_vertices(=, [Es | WG0], [_ | Vs], [Es | WG]) :-
    wgraph_add_sorted_vertices(WG0, Vs, WG).

wgraph_add_sorted_vertices(>, WG0, [V | Vs], [V - [] | WG]) :-
    wgraph_add_sorted_vertices(WG0, Vs, WG).

%!  wgraph_breadth_first_search(+WGraph, +Starts:list, -End, -Path:nonneg) is multi.
%!  wgraph_breadth_first_search(+WGraph, +Starts:list, +End, -Path:nonneg) is semidet.
wgraph_breadth_first_search(WGraph, Starts, End, Path) :-
    must_be(list, WGraph),
    must_be(list, Starts),
    pairs_keys_values(Pairs, Keys, Starts),
    maplist(=(0), Keys),
    list_to_heap(Pairs, Heap),
    (   ground(End)
    ->  once(wgraph_breadth_first_search_(WGraph, Heap, End, Path))
    ;   wgraph_breadth_first_search_(WGraph, Heap, End, Path)
    ).

wgraph_breadth_first_search_(WGraph0, Heap0, End, Path) :-
    get_from_heap(Heap0, Path0, Here, Heap1),
    (   wgraph_neighbours(Here, WGraph0, Neighbours)
    ->  (   End = Here, Path = Path0
        ;   wgraph_del_vertices(WGraph0, [Here], WGraph),
            foldl({Path0}/[There - Path1, H0, H] >> (
                Path2 is Path0 + Path1,
                add_to_heap(H0, Path2, There, H)
            ), Neighbours, Heap1, Heap),
            wgraph_breadth_first_search_(WGraph, Heap, End, Path)
        )
    ;   wgraph_breadth_first_search_(WGraph0, Heap1, End, Path)
    ).

%!  wgraph_cycles(+WGraph, -Cycle:list, -Length:number) is nondet.
wgraph_cycles(WGraph, Cycle, Length) :-
    must_be(list, WGraph),
    must_be(list ; var, Cycle),
    wgraph_vertices(WGraph, Vertices),
    member(Start, Vertices),
    wgraph_cycles(WGraph, Start, Cycle, Length).

%!  wgraph_cycles(+WGraph, +Start, -Cycle:list, -Length:number) is nondet.
wgraph_cycles(WGraph, Start, [Start | Cycle], Length) :-
    wgraph_neighbours(Start, WGraph, Neighbours),
    member(Vertex - Length0, Neighbours),
    wgraph_cycles_(WGraph, Vertex, Start, Cycle, Length0, Length).

wgraph_cycles_(WGraph0, Here, Start, Cycle, Length0, Length) :-
    (   Here = Start
    ->  Length0 = Length,
        Cycle = [Here]
    ;   Cycle = [Here | Cycle0],
        wgraph_neighbours(Here, WGraph0, Neighbours),
        wgraph_del_vertices(WGraph0, [Here], WGraph),
        member(There - Length1, Neighbours),
        Length2 is Length0 + Length1,
        wgraph_cycles_(WGraph, There, Start, Cycle0, Length2, Length)
    ).

%!  wgraph_del_vertices(+WGraph0, +Vertices:list, ?WGraph) is det.
wgraph_del_vertices(WGraph0, Vertices, WGraph) :-
    sort(Vertices, Sorted),
    wgraph_del_sorted_vertices(WGraph0, Sorted, WGraph).

wgraph_del_sorted_vertices(WGraph0, Vertices, WGraph) :-
    convlist({Vertices}/[A - Bs0, A - Bs] >> (
        \+ ord_memberchk(A, Vertices),
        exclude({Vertices}/[B - _] >> ord_memberchk(B, Vertices), Bs0, Bs)
    ), WGraph0, WGraph).

%!  wgraph_edges(+WGraph, ?Edges) is det.
%   Edges is a list of Source - (Destination - Weight).
wgraph_edges(WGraph, Edges) :-
    findall(Source - DestinationWeight, (
        member(Source - DestinationWeights, WGraph),
        member(DestinationWeight, DestinationWeights)
    ), Edges).

%!  wgraph_gen(+WGraph, ?Src, ?Dest, ?Weight) is nondet.
wgraph_gen(WGraph, Src, Dest, Weight) :-
    member(Src - DWs, WGraph),
    member(Dest - Weight, DWs).

%!  wgraph_graphviz(+WGraph, ?Codes0, ?Codes) is nondet.
wgraph_graphviz(WGraph) -->
    "digraph {", eol, sequence(wgraph_graphviz_edges, WGraph), "}", eol.

wgraph_graphviz_edges(Source - DestWeights) -->
    sequence(wgraph_graphviz_edge(Source), DestWeights).

wgraph_graphviz_edge(Source, Destination - Weight) -->
    "\t", graphviz_node(Source), " -> ", graphviz_node(Destination),
    " [label=", integer(Weight), "]", eol.

%!  wgraph_neighbors(+Vertex, +Graph, ?NeighborWeights) is det.
wgraph_neighbors(Vertex, Graph, NeighborWeights) :-
    memberchk(Vertex - NeighborWeights, Graph).

%!  wgraph_neighbours(+Vertex, +Graph, ?NeighbourWeights) is det.
wgraph_neighbours(Vertex, Graph, NeighbourWeights) :-
    memberchk(Vertex - NeighbourWeights, Graph).

%!  wgraph_vertices(+WGraph, ?Vertices) is det.
wgraph_vertices(WGraph, Vertices) :- pairs_keys(WGraph, Vertices).

%!  wgraph_weight(+WGraph, +Source, ?Destination, ?Weight) is semidet.
wgraph_weight(WGraph, Source, Destination, Weight) :-
    memberchk(Source - DestinationWeights, WGraph),
    memberchk(Destination - Weight, DestinationWeights).

%!  wgraph_to_ugraph(+WGraph, ?Graph) is det.
%   Strip the weights from a weighted graph, returning an unweighted graph.
wgraph_to_ugraph --> maplist([A - Bs0, A - Bs] >> maplist(arg(1), Bs0, Bs)).
