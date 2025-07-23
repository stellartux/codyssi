/** <module> Binary Trees

*/
:- module(binary_trees, [
    bt_equal/2,
    bt_empty/1,
    bt_insert/3,
    bt_in_order/2,
    bt_post_order/2,
    bt_pre_order/2,
    bt_level_order/3,
    bt_path/3,
    bt_size/2,
    bt_to_ord_list/2,
    list_to_binary_tree/2
]).

%!  bt_equal(+BT1, +BT2) is semidet.
bt_equal(bt, bt).
bt_equal(bt(L0, X0, R0), bt(L1, X1, R1)) :- X0 = X1, bt_equal(L0, L1), bt_equal(R0, R1).

bt_empty(bt).

%!  bt_insert(+BT0, +X, -BT) is det.
bt_insert(bt, X, bt(bt, X, bt)).
bt_insert(bt(L0, X0, R0), X, bt(L1, X0, R1)) :-
    (   X @> X0
    ->  L1 = L0, bt_insert(R0, X, R1)
    ;   R1 = R0, bt_insert(L0, X, L1)
    ).

%!  bt_in_order(+BT, -X) is nondet.
bt_in_order(bt(L, X0, R), X) :- ( X0 = X ; bt_in_order(L, X) ; bt_in_order(R, X) ).

%!  bt_pre_order(+BT, -X) is nondet.
bt_pre_order(bt(L, X0, R), X) :- ( bt_pre_order(L, X) ; X = X0 ; bt_pre_order(R, X) ).

%!  bt_post_order(+BT, -X) is nondet.
bt_post_order(bt(L, X0, R), X) :- ( bt_post_order(L, X) ; bt_post_order(R, X) ; X = X0 ).

%!  bt_level_order(+BT, -X, -Level) is nondet.
bt_level_order(BT, X, Level) :- bt_level_order_([BT], X, 1, Level).

bt_level_order_(BTs, X, Level, Level) :- member(bt(_, X, _), BTs).
bt_level_order_(BTs0, X, Level0, Level) :-
    BTs0 \== [],
    findall(BT, (
        member(bt(L, _, R), BTs0),
        ( BT = L ; BT = R )
    ), BTs),
    succ(Level0, Level1),
    bt_level_order_(BTs, X, Level1, Level).

%!  bt_path(+BT, +Value, -Path) is nondet.
bt_path(bt(L, X0, R), X, Path) :-
    (   X = X0
    ->  Path = []
    ;   Path = [X0 | Path0],
        (   X @> X0
        ->  bt_path(R, X, Path0)
        ;   bt_path(L, X, Path0)
        )
    ).

%!  bt_size(+BT, -Size) is det.
bt_size(BT, Size) :- bt_size(BT, 0, Size).

bt_size(bt)          --> [].
bt_size(bt(L, _, R)) --> succ, bt_size(L), bt_size(R).

%!  bt_to_ord_list(+BT, -List) is det.
bt_to_ord_list(BT, List) :- findall(X, bt_pre_order(BT, X), List).

%!  list_to_binary_tree(+List, -BT) is det.
list_to_binary_tree(List, BT) :- foldl([X, B0, B] >> bt_insert(B0, X, B), List, bt, BT).
