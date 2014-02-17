%% Given two binary space partitions (BSP), where a partition can be labelled
%% zero or one, compute the binary AND of the two BSPs.
%%
%% NOTE: tree(one, one) can be simplified to one,
%% and tree(zero, zero) can be simplified to zero.
%%
%% The code below is in Prolog.


simplify(one, one).
simplify(zero, zero).
simplify(tree(L, R), one) :- simplify(L, one), simplify(R, one).
simplify(tree(L, R), zero) :- simplify(L, zero), simplify(R, zero).
simplify(tree(L1, R1), tree(L2, R2)) :- simplify(L1, L2), simplify(R1, R2).

and(Tree1, Tree2, Res) :- and_(Tree1, Tree2, Tmp), simplify(Tmp, Res).

and_(one, R, R).
and_(L, one, L).
and_(zero, _,  zero).
and_(_, zero, zero).
and_(tree(L1, R1), tree(L2, R2), tree(L3, R3)) :-
    and_(L1, L2, L3), and_(R1, R2, R3).

%%%% The method below simplifies as it performs the AND operation
%%%% and so only traverses the tree once.
%% and(one, R1, R2) :- simplify(R1, R2).
%% and(L1, one, L2) :- simplify(L1, L2).
%% and(zero, _,  zero).
%% and(_, zero, zero).
%% and(tree(L1, R1), tree(L2, R2), one) :-          and(L1, L2, one),
%%                                                  and(R1, R2, one).
%% and(tree(L1, R1), tree(L2, R2), zero) :-         and(L1, L2, zero),
%%                                                  and(R1, R2, zero).
%% and(tree(L1, R1), tree(L2, R2), tree(L3, R3)) :- and(L1, L2, L3),
%%                                                  and(R1, R2, R3).


