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

minutia(1, bellball).
minutia(3, bellball).
minutia(2, catnip).
minutia(5, catnip).
minutia(0, mouse).
minutia(4, mouse).
minutia(1, pawprint).
minutia(4, pawprint).
minutia(3, sock).
minutia(5, sock).

across_from(0, 3).
across_from(1, 5).
across_from(2, 4).
across_from(3, 0).
across_from(4, 2).
across_from(5, 1).

left_of(0, 5).
left_of(X, Y) :- between(0, 5, X), between(0, 5, Y), Y is X - 1.

right_of(5, 0).
right_of(X, Y) :- between(0, 5, X), between(0, 5, Y), Y is X + 1.

next_to(X, Y) :- left_of(X, Y).
next_to(X, Y) :- right_of(X, Y).

some_cat(X, Cats) :-
    nth0(X, Cats, Cat),
    Cat \= none_cat.

cat_across_from_cat(A, B, Cats) :-
    nth0(X, Cats, A),
    nth0(Y, Cats, B),
    across_from(X, Y).

cat_next_to_cat(A, B, Cats) :-
    nth0(X, Cats, A),
    nth0(Y, Cats, B),
    next_to(X, Y).

cat_next_to_place(A, P, Cats) :-
    places(Places),
    nth0(X, Cats, A),
    nth0(Y, Places, P),
    next_to(X, Y).

cat_right_of_cat(A, B, Cats) :-
    nth0(X, Cats, A),
    nth0(Y, Cats, B),
    right_of(X, Y).

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

solution3(Cats) :-
    cat_perms([duchess, sassy, tomcat], Cats),
    places(Places),

    nth0(Ginger, Cats, ginger),
    next_to(N, Ginger),
    some_cat(N, Cats),
    nth0(FishBowl, Places, fishbowl),
    \+ next_to(Ginger, FishBowl),

    nth0(MrMittens, Cats, mrmittens),
    MrMittens = FishBowl,

    nth0(PipSqueak, Cats, pipsqueak),
    minutia(PipSqueak, bellball),
    minutia(PipSqueak, pawprint),

    !.

solution4(Cats) :-
    cat_perms([duchess, pipsqueak, tomcat], Cats),
    cat_across_from_cat(ginger, sassy, Cats),
    cat_next_to_cat(sassy, mrmittens, Cats),
    cat_next_to_place(ginger, birdcage, Cats),
    cat_right_of_cat(ginger, mrmittens, Cats),
    !.

pretty(Res) :-
    solution1(Cats),
    places(Places),
    maplist(pretty_print, Cats, Places, Res).
