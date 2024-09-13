%% Solver for the logic game "Cat Crimes".

replicate(0, _, Ys) :- Ys = [], !.
replicate(N, X, Ys) :-
    M is N - 1,
    replicate(M, X, Zs),
    Ys = [X | Zs],
    !.

places(Places) :-
    Places = [birdcage, coffeecup, shoes, fishbowl, yarn, plant].

all_cats(Cats) :-
    Cats = [duchess, ginger, mrmittens, pipsqueak, sassy, tomcat].

cat_perms(Cats) :-
    all_cats(T),
    permutation(T, Cats).

excluded_cat_perms(SleepingCats, Cats) :-
    all_cats(AllCats),
    subtract(AllCats, SleepingCats, PresentCats),
    length(SleepingCats, Cnt),
    replicate(Cnt, no_cat, NoCats),
    append(PresentCats, NoCats, SixCats),
    permutation(SixCats, Cats).

included_cat_perms(IncludedCats, Cats) :-
    length(IncludedCats, Cnt),
    replicate(6 - Cnt, no_cat, NoCats),
    append(IncludedCats, NoCats, SixCats),
    permutation(SixCats, Cats).

minutia(1, bellball).
minutia(3, bellball).
minutia(2, catnip).
minutia(5, catnip).
minutia(0, clawmarks).
minutia(2, clawmarks).
minutia(0, mouse).
minutia(4, mouse).
minutia(1, pawprint).
minutia(4, pawprint).
minutia(3, sock).
minutia(5, sock).

trait(whitepaws, sassy).
trait(whitepaws, mrmittens).

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

left2_of(X, Y) :-
    left_of(X, Z),
    left_of(Z, Y).

right2_of(X, Y) :-
    right_of(X, Z),
    right_of(Z, Y).

next_to(X, Y) :- left_of(X, Y).
next_to(X, Y) :- right_of(X, Y).

next2_to(X, Y) :- left2_of(X, Y).
next2_to(X, Y) :- right2_of(X, Y).

some_cat(X, Cats) :-
    nth0(X, Cats, Cat),
    Cat \= no_cat.

cat_by_minutia(C, M, Cats) :-
    nth0(X, Cats, C),
    minutia(X, M).

cat_by_minutia2(C, M1, M2, Cats) :-
    nth0(X, Cats, C),
    minutia(X, M1),
    minutia(X, M2).

cat_by_place(C, P, Cats) :-
    places(Places),
    nth0(X, Cats, C),
    nth0(X, Places, P).

cat_across_from_cat(A, B, Cats) :-
    nth0(X, Cats, A),
    nth0(Y, Cats, B),
    across_from(X, Y).

cat_left_of_cat(A, B, Cats) :-
    nth0(X, Cats, A),
    nth0(Y, Cats, B),
    left_of(X, Y).

cat_right_of_cat(A, B, Cats) :-
    nth0(X, Cats, A),
    nth0(Y, Cats, B),
    right_of(X, Y).

cat_next_to_cat(A, B, Cats) :-
    nth0(X, Cats, A),
    nth0(Y, Cats, B),
    next_to(X, Y).

cat_2_from_cat(A, B, Cats) :-
    nth0(X, Cats, A),
    nth0(Y, Cats, B),
    next2_to(X, Y).

cat_next_to_place(A, P, Cats) :-
    places(Places),
    nth0(X, Cats, A),
    nth0(Y, Places, P),
    next_to(X, Y).

cat_next_to_some_cat(C, Cats) :-
    nth0(X, Cats, C),
    next_to(Y, X),
    some_cat(Y, Cats).

trait_by_minutia(T, M, Cats) :-
    trait(T, C),
    nth0(X, Cats, C),
    minutia(X, M).

pretty_print(Cat, Place, Res) :-
    swritef(Res, '%w => %w', [Cat, Place]).

solution1(Cats) :-
    excluded_cat_perms([mrmittens, pipsqueak], Cats),
    cat_by_minutia(tomcat, catnip, Cats),
    cat_by_minutia(tomcat, sock, Cats),
    cat_across_from_cat(sassy, tomcat, Cats),
    cat_next_to_place(ginger, fishbowl, Cats),
    cat_left_of_cat(duchess, sassy, Cats),
    !.

solution2(Cats) :-
    cat_perms(Cats),
    cat_by_place(mrmittens, birdcage, Cats),
    cat_across_from_cat(tomcat, mrmittens, Cats),
    cat_by_minutia(pipsqueak, mouse, Cats),
    cat_next_to_cat(duchess, pipsqueak, Cats),
    cat_next_to_cat(sassy, ginger, Cats),
    \+ cat_across_from_cat(ginger, duchess, Cats),
    !.

solution3(Cats) :-
    included_cat_perms([ginger, mrmittens, pipsqueak], Cats),
    cat_next_to_some_cat(ginger, Cats),
    \+ cat_next_to_place(ginger, fishbowl, Cats),
    cat_by_place(mrmittens, fishbowl, Cats),
    cat_by_minutia(pipsqueak, bellball, Cats),
    cat_by_minutia(pipsqueak, pawprint, Cats),
    !.

solution4(Cats) :-
    included_cat_perms([ginger, sassy, mrmittens], Cats),
    cat_across_from_cat(ginger, sassy, Cats),
    cat_next_to_cat(sassy, mrmittens, Cats),
    cat_next_to_place(ginger, birdcage, Cats),
    cat_right_of_cat(ginger, mrmittens, Cats),
    !.

solution5(Cats) :-
    excluded_cat_perms([tomcat, sassy, duchess], Cats),
    % no cat is sitting next to another
    ([no_cat, _, no_cat, _, no_cat, _] = Cats;
     [_, no_cat, _, no_cat, _, no_cat] = Cats),

    trait_by_minutia(whitepaws, pawprint, Cats),
    cat_by_minutia(pipsqueak, clawmarks, Cats),
    cat_across_from_cat(ginger, no_cat, Cats),
    !.

solution6(Cats) :-
    included_cat_perms([tomcat, duchess, ginger, sassy], Cats),
    cat_by_minutia2(no_cat, bellball, pawprint, Cats),
    cat_by_minutia2(no_cat, catnip, clawmarks, Cats),
    cat_by_minutia(tomcat, mouse, Cats),
    cat_by_minutia(duchess, sock, Cats),
    cat_next_to_cat(ginger, sassy, Cats),
    cat_next_to_place(sassy, fishbowl, Cats),
    \+ cat_across_from_cat(ginger, tomcat, Cats),
    !.

solution7(Cats) :-
    cat_perms(Cats),
    cat_by_place(mrmittens, fishbowl, Cats),
    cat_left_of_cat(sassy, mrmittens, Cats),
    cat_by_minutia(tomcat, clawmarks, Cats),
    cat_2_from_cat(ginger, sassy, Cats),
    cat_right_of_cat(ginger, duchess, Cats),
    cat_across_from_cat(pipsqueak, duchess, Cats),
    !.

pretty(Res) :-
    solution1(Cats),
    places(Places),
    maplist(pretty_print, Cats, Places, Res).
