%% This program is written in Prolog.
%%
%% A plateau is an sublist of 2 or more of the same elements,
%% where the sublist is not contained within a larger sublist
%% of the same elements.
%%
%% ?- plateaus([1,2,2,3,4,4,4,5,6,6,7], Results).
%% Results = [[2, 2], [4, 4, 4], [6, 6]].

plateaus(Xs, Results) :- findall(Res, plateau_(Xs, [], Res), Results).

plateau_([],     Acc,       Acc) :- Acc = [_,_|_].
plateau_([X|Xs], [],        Res) :- plateau_(Xs, [X], Res).
plateau_([X|Xs], [X|Acc],   Res) :- plateau_(Xs, [X,X|Acc], Res).
plateau_([X|_],  [Y,Y|Acc], Res) :- X \= Y, Res = [Y,Y|Acc].
plateau_([X|Xs], [Y|_],     Res) :- X \= Y, plateau_(Xs, [X], Res).

