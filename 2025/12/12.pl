#!/usr/bin/env swipl
% https://www.codyssi.com/view_problem_16

:- module(codyssi2025day12, []).
:- module(codyssi2025day12).
:- initialization(main, main).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).

load(whirlpool(rows(Rows), Instructions, Actions)) -->
    sequence(number_row, Rows), eol,
    sequence(instructions, Instructions), eol,
    sequence(flow_control, Actions), eol, eos.

number_row(Row) --> sequence(integer, " ", Row), eol, { Row \== [] }.

rowcol(row(Y)) --> "ROW ", integer(Y).
rowcol(col(X)) --> "COL ", integer(X).

instructions(shift(RowCol, Amount)) -->
    "SHIFT ", rowcol(RowCol), " BY ", integer(Amount), eol.

instructions(change(Target, Cmd)) -->
    command(Cmd), " ", ( "ALL", { Target = all } | rowcol(Target) ), eol.

command(     add(N)) -->      "ADD ", integer(N).
command(     sub(N)) -->      "SUB ", integer(N).
command(multiply(N)) --> "MULTIPLY ", integer(N).

add(     A, B, C) :- C #= (A + B) mod 1073741824.
sub(     A, B, C) :- C #= (B - A) mod 1073741824.
multiply(A, B, C) :- C #= (A * B) mod 1073741824.

flow_control(  act) --> "TAKE", eol,   "ACT", eol.
flow_control(cycle) --> "TAKE", eol, "CYCLE", eol.

change(all, Cmd, Grid0, Grid) :-
    Grid0 =.. [Name, Vals0],
    Grid  =.. [Name, Vals ],
    maplist(maplist(Cmd), Vals0, Vals).

change(col(X), Cmd, rows(Rows), cols(Cols)) :- !,
    transpose(Rows, Cols0),
    change_(X, Cmd, Cols0, Cols).

change(col(X), Cmd, cols(Cols0), cols(Cols)) :- !,
    change_(X, Cmd, Cols0, Cols).

change(row(Y), Cmd, cols(Cols), rows(Rows)) :- !,
    transpose(Cols, Rows0),
    change_(Y, Cmd, Rows0, Rows).

change(row(Y), Cmd, rows(Rows0), rows(Rows)) :- !,
    change_(Y, Cmd, Rows0, Rows).

change_(N, Cmd, Lists0, Lists) :-
    nth1(N, Lists0, List0, Lists1),
    maplist(Cmd, List0, List),
    nth1(N, Lists, List, Lists1).

shift(row(Y), Amount, cols(Cols), rows(Rows)) :- !,
    transpose(Cols, Rows0),
    shift_(Y, Amount, Rows0, Rows).

shift(row(Y), Amount, rows(Rows0), rows(Rows)) :- !,
    shift_(Y, Amount, Rows0, Rows).

shift(col(X), Amount, rows(Rows), cols(Cols)) :- !,
    transpose(Rows, Cols0),
    shift_(X, Amount, Cols0, Cols).

shift(col(X), Amount, cols(Cols0), cols(Cols)) :- !,
    shift_(X, Amount, Cols0, Cols).

shift_(N, Amount, Lists0, Lists) :-
    length(Suffix, Amount),
    nth1(N, Lists0, List0, Lists1),
    once(append(Prefix, Suffix, List0)),
    once(append(Suffix, Prefix, List)),
    nth1(N, Lists, List, Lists1).

load(Filename, Result) :- once(phrase_from_file(load(Result), Filename)).

max_row_or_column_sum(Grid, MaxSum) :-
    arg(1, Grid, Rows),
    transpose(Rows, Cols),
    aggregate_all(max(Sum), (
        ( member(List, Rows) ; member(List, Cols) ),
        sum_list(List, Sum)
    ), MaxSum).

part_one(whirlpool(Grid0, Instructions, _), Result) :-
    foldl(call, Instructions, Grid0, Grid),
    max_row_or_column_sum(Grid, Result).

cycle([I | Is0], Is, G, G) :- append(Is0, [I], Is).
act([I | Is], Is, G0, G) :- call(I, G0, G).

process(As0, As, Is0, Is, G0, G) :-
    (   As0 = As, Is0 = Is, G0 = G
    ;   As0 = [A | As1],
        call(A, Is0, Is1, G0, G1),
        process(As1, As, Is1, Is, G1, G)
    ).

part_two(whirlpool(Grid0, Instructions, Actions), Result) :-
    once(process(Actions, [], Instructions, _, Grid0, Grid)),
    max_row_or_column_sum(Grid, Result).

part_three(whirlpool(Grid0, Instructions, Actions0), Result) :-
    append(Actions0, Actions, Actions),
    once(process(Actions, _, Instructions, [], Grid0, Grid)),
    max_row_or_column_sum(Grid, Result).

main([Filename]) :-
    load(Filename, Input),
    part_one(Input, Result1),
    writeln(Result1),
	part_two(Input, Result2),
	writeln(Result2),
	part_three(Input, Result3),
	writeln(Result3).
