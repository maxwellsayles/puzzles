%% Solver for the logic game "Cat Crimes".

replicate(0, _, Ys) :- Ys = [], !.
replicate(N, X, Ys) :-
    M is N - 1,
    replicate(M, X, Zs),
    Ys = [X | Zs],
    !.

places(Places) :-
    Places = [birdcage, coffeecup, shoes, fishbowl, yarn, plant].

cat_perms(SleepingCats, Cats) :-
    InitCats = [duchess, ginger, mrmittens, pipsqueak, sassy, tomcat],
    subtract(InitCats, SleepingCats, PresentCats),
    length(SleepingCats, Cnt),
    replicate(Cnt, none_cat, NoneCats),
    append(PresentCats, NoneCats, SixCats),
    permutation(SixCats, Cats).

minutia(2, catnip).
minutia(5, catnip).
minutia(0, mouse).
minutia(4, mouse).
minutia(3, sock).
minutia(5, sock).

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
    cat_perms([mrmittens, pipsqueak], Cats),
    places(Places),

    nth0(TomCat, Cats, tomcat),
    minutia(TomCat, catnip),
    minutia(TomCat, sock),

    nth0(Sassy, Cats, sassy),
    across_from(Sassy, TomCat),

    nth0(Ginger, Cats, ginger),
    nth0(FishBowl, Places, fishbowl),
    next_to(Ginger, FishBowl),

    nth0(Duchess, Cats, duchess),
    left_of(Duchess, Sassy),

    !.

solution2(Cats) :-
    cat_perms([], Cats),
    places(Places),

    nth0(MrMittens, Cats, mrmittens),
    nth0(MrMittens, Places, birdcage),

    nth0(TomCat, Cats, tomcat),
    across_from(MrMittens, TomCat),

    nth0(PipSqueak, Cats, pipsqueak),
    minutia(PipSqueak, mouse),

    nth0(Duchess, Cats, duchess),
    next_to(Duchess, PipSqueak),

    nth0(Sassy, Cats, sassy),
    nth0(Ginger, Cats, ginger),
    next_to(Sassy, Ginger),

    \+ across_from(Ginger, Duchess),

    !.

pretty(Res) :-
    solution1(Cats),
    places(Places),
    maplist(pretty_print, Cats, Places, Res).
