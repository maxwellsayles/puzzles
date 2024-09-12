%% Solver for the logic game "Cat Crimes".

take(0, _, []).
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :- M is N-1, take(M, Xs, Ys).

places(Places) :-
    Places = [birdcage, coffee_cup, shoes, fish_bowl, yarn, plant].

cat_perms(SleepingCats, Cats) :-
    InitCats = [duchess, ginger, mr_mittens, pip_squeak, sassy, tom_cat],
    subtract(InitCats, SleepingCats, PresentCats),
    append(PresentCats, [none_cat, none_cat, none_cat, none_cat, none_cat, none_cat], NoneCats),
    take(6, NoneCats, SixCats),
    permutation(SixCats, Cats).

minutia(catnip, 2).
minutia(catnip, 5).
minutia(sock, 3).
minutia(sock, 5).

across_from(0, 3).
across_from(1, 5).
across_from(2, 4).
across_from(3, 0).
across_from(4, 2).
across_from(5, 1).

left_of(0, 5).
left_of(X, Y) :- Y is X - 1, Y >= 0, Y =< 5.

right_of(5, 0).
right_of(X, Y) :- Y is X + 1, Y >= 0, Y =< 5.

next_to(X, Y) :- left_of(X, Y).
next_to(X, Y) :- right_of(X, Y).

pretty_print(Cat, Place, Res) :-
    swritef(Res, '%w => %w', [Cat, Place]).

solution1(Cats) :-
    cat_perms([mr_mittens, pip_squeak], Cats),
    places(Places),

    nth0(TomCat, Cats, tom_cat),
    minutia(catnip, TomCat),
    minutia(sock, TomCat),

    nth0(Sassy, Cats, sassy),
    across_from(Sassy, TomCat),

    nth0(Ginger, Cats, ginger),
    nth0(FishBowl, Places, fish_bowl),
    next_to(Ginger, FishBowl),

    nth0(Duchess, Cats, duchess),
    left_of(Duchess, Sassy),

    !.

pretty(Res) :-
    solution1(Cats),
    places(Places),
    maplist(pretty_print, Cats, Places, Res).
