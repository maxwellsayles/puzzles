%% Solver for the logic game "Cat Crimes".

replicate(0, _, Ys) :- Ys = [], !.
replicate(N, X, Ys) :-
    M is N - 1,
    replicate(M, X, Zs),
    Ys = [X | Zs],
    !.

choose(0, _, Ys) :- Ys = [], !.
choose(_, [], _) :- false.
choose(N, [X | Xs], [X | Ys]) :-
    M is N - 1,
    choose(M, Xs, Ys).
choose(N, [_ | Xs], Ys) :-
    choose(N, Xs, Ys).

all_cats(Cats) :-
    Cats = [duchess, ginger, mrmittens, pipsqueak, sassy, tomcat].

choose_perms(N, Cats) :-
    all_cats(Cs),
    choose(N, Cs, CatSet),
    replicate(6 - N, no_cat, NoCats),
    append(CatSet, NoCats, SixCats),
    permutation(SixCats, Cats).

set_of_cats(MentionedCats, MentionedTraits, SleepingCats, SetSize, Cats) :-
    maplist([T, C]>>trait_to_cat(T, C), MentionedTraits, TraitCats),
    append(MentionedCats, TraitCats, Cats1),
    list_to_set(Cats1, CatSet1),
    subtract(CatSet1, SleepingCats, Cats),
    length(Cats, SetSize).

fancy_perms(MentionedCats, MentionedTraits, SleepingCats, SetSize, Cats) :-
    set_of_cats(MentionedCats, MentionedTraits, SleepingCats, SetSize, Cats1),
    replicate(6 - SetSize, no_cat, NoCats),
    append(Cats1, NoCats, Cats2),
    permutation(Cats2, Cats).

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

cat(duchess).
cat(ginger).
cat(mrmittens).
cat(pipsqueak).
cat(sassy).
cat(tomcat).

place(birdcage).
place(coffeecup).
place(fishbowl).
place(shoes).
place(plant).
place(yarn).

minutia(bellball).
minutia(catnip).
minutia(clawmarks).
minutia(mouse).
minutia(pawprint).
minutia(sock).

minutia2(M1, M2) :-
    minutia(M1),
    minutia(M2),
    M1 \= M2.

trait(bell).
trait(bow).
trait(blueeyes).
trait(longhair).
trait(stripes).
trait(whitepaws).

place_to_idx(birdcage, 0).
place_to_idx(coffeecup, 1).
place_to_idx(shoes, 2).
place_to_idx(fishbowl, 3).
place_to_idx(yarn, 4).
place_to_idx(plant, 5).

minutia_to_idx(bellball, 1).
minutia_to_idx(bellball, 3).
minutia_to_idx(catnip, 2).
minutia_to_idx(catnip, 5).
minutia_to_idx(clawmarks, 0).
minutia_to_idx(clawmarks, 2).
minutia_to_idx(mouse, 0).
minutia_to_idx(mouse, 4).
minutia_to_idx(pawprint, 1).
minutia_to_idx(pawprint, 4).
minutia_to_idx(sock, 3).
minutia_to_idx(sock, 5).

trait_to_cat(bell, mrmittens).
trait_to_cat(bell, pipsqueak).
trait_to_cat(bow, duchess).
trait_to_cat(bow, tomcat).
trait_to_cat(blueeyes, ginger).
trait_to_cat(blueeyes, tomcat).
trait_to_cat(longhair, duchess).
trait_to_cat(longhair, sassy).
trait_to_cat(stripes, ginger).
trait_to_cat(stripes, pipsqueak).
trait_to_cat(whitepaws, sassy).
trait_to_cat(whitepaws, mrmittens).

idx(C, Idx, Cats) :-
    cat(C),
    nth0(Idx, Cats, C),
    !.
idx(no_cat, Idx, Cats) :-
    nth0(Idx, Cats, no_cat).
idx(some_cat, Idx, Cats) :-
    nth0(Idx, Cats, C),
    C \= no_cat.
idx(P, Idx, _) :-
    place(P),
    place_to_idx(P, Idx),
    !.
idx(T, Idx, Cats) :-
    trait(T),
    trait_to_cat(T, C),
    nth0(Idx, Cats, C).
idx(M, Idx, _) :-
    minutia(M),
    minutia_to_idx(M, Idx).
idx(minutia2(M1, M2), Idx, Cats) :-
    idx(M1, Idx, Cats),
    idx(M2, Idx, Cats).
idx((I1; I2), Idx, Cats) :-
    (idx(I1, Idx, Cats);
     idx(I2, Idx, Cats)).

near(X, X).

across_from(0, 3).
across_from(1, 5).
across_from(2, 4).
across_from(3, 0).
across_from(4, 2).
across_from(5, 1).

left_of(0, 5).
left_of(X, Y) :-
    between(0, 5, X),
    between(0, 5, Y),
    Y is X - 1.

left2_of(X, Y) :-
    left_of(X, Z),
    left_of(Z, Y).

right_of(5, 0).
right_of(X, Y) :-
    between(0, 5, X),
    between(0, 5, Y),
    Y is X + 1.

right2_of(X, Y) :-
    right_of(X, Z),
    right_of(Z, Y).

next_to(X, Y) :- left_of(X, Y).
next_to(X, Y) :- right_of(X, Y).

next2_to(X, Y) :- left2_of(X, Y).
next2_to(X, Y) :- right2_of(X, Y).

next3_to(X, Y) :-
    between(0, 5, X),
    between(0, 5, Y),
    Y is ((X + 3) mod 6).

is_between(X, Y1, Y2) :-
    next_to(X, Y1),
    next_to(X, Y2),
    Y1 \= Y2.

rel(A, R, B, Cats) :-
    idx(A, X, Cats),
    idx(B, Y, Cats),
    call(R, X, Y).

rel(A, R, B1, B2, Cats) :-
    idx(A, X, Cats),
    idx(B1, Y1, Cats),
    idx(B2, Y2, Cats),
    call(R, X, Y1, Y2).

solution1(Cats) :-
    excluded_cat_perms([mrmittens, pipsqueak], Cats),
    rel(tomcat, near, minutia2(catnip, sock), Cats),
    rel(sassy, across_from, tomcat, Cats),
    rel(ginger, next_to, fishbowl, Cats),
    rel(duchess, left_of, sassy, Cats),
    !.

solution2(Cats) :-
    cat_perms(Cats),
    rel(mrmittens, near, birdcage, Cats),
    rel(tomcat, across_from, mrmittens, Cats),
    rel(pipsqueak, near, mouse, Cats),
    rel(duchess, next_to, pipsqueak, Cats),
    rel(sassy, next_to, ginger, Cats),
    \+ rel(ginger, across_from, duchess, Cats),
    !.

solution3(Cats) :-
    included_cat_perms([ginger, mrmittens, pipsqueak], Cats),
    rel(ginger, next_to, some_cat, Cats),
    \+ rel(ginger, next_to, fishbowl, Cats),
    rel(mrmittens, near, fishbowl, Cats),
    rel(pipsqueak, near, minutia2(bellball, pawprint), Cats),
    !.

solution4(Cats) :-
    included_cat_perms([ginger, sassy, mrmittens], Cats),
    rel(ginger, across_from, sassy, Cats),
    rel(sassy, next_to, mrmittens, Cats),
    rel(ginger, next_to, birdcage, Cats),
    rel(ginger, right_of, mrmittens, Cats),
    !.

solution5(Cats) :-
    excluded_cat_perms([tomcat, sassy, duchess], Cats),
    ([no_cat, _, no_cat, _, no_cat, _] = Cats;
     [_, no_cat, _, no_cat, _, no_cat] = Cats),
    rel(whitepaws, near, pawprint, Cats),
    rel(pipsqueak, near, clawmarks, Cats),
    rel(ginger, across_from, no_cat, Cats),
    !.

solution6(Cats) :-
    included_cat_perms([tomcat, duchess, ginger, sassy], Cats),
    rel(no_cat, near, minutia2(bellball, pawprint), Cats),
    rel(no_cat, near, minutia2(catnip, clawmarks), Cats),
    rel(tomcat, near, mouse, Cats),
    rel(duchess, near, sock, Cats),
    rel(ginger, next_to, sassy, Cats),
    rel(sassy, next_to, fishbowl, Cats),
    \+ rel(ginger, across_from, tomcat, Cats),
    !.

solution7(Cats) :-
    cat_perms(Cats),
    rel(mrmittens, near, fishbowl, Cats),
    rel(sassy, left_of, mrmittens, Cats),
    rel(tomcat, near, clawmarks, Cats),
    rel(ginger, next2_to, sassy, Cats),
    rel(pipsqueak, across_from, duchess, Cats),
    !.

solution8(Cats) :-
    cat_perms(Cats),
    rel(sassy, near, birdcage, Cats),
    rel(mrmittens, right_of, ginger, Cats),
    \+ rel(ginger, next_to, sassy, Cats),
    rel(ginger, right_of, tomcat, Cats),
    rel(duchess, is_between, tomcat, pipsqueak, Cats),
    !.

solution9(Cats) :-
    excluded_cat_perms([duchess], Cats),
    rel(tomcat, near, birdcage, Cats),
    rel(sassy, next2_to, tomcat, Cats),
    rel(ginger, next_to, (mrmittens; pipsqueak), Cats),
    rel(no_cat, right_of, ginger, Cats),
    rel(mrmittens, near, (minutia2(bellball, pawprint);
			  minutia2(catnip, clawmarks)), Cats),
    !.

solution10(Cats) :-
    cat_perms(Cats),
    rel(tomcat, is_between, pipsqueak, mrmittens, Cats),
    rel(sassy, across_from, tomcat, Cats),
    rel(mrmittens, next3_to, duchess, Cats),
    rel(ginger, across_from, pipsqueak, Cats),
    \+ rel(mrmittens, near, fishbowl, Cats),
    rel(ginger, right_of, duchess, Cats),
    !.

solution11(Cats) :-
    excluded_cat_perms([tomcat, mrmittens], Cats),
    rel(no_cat, near, fishbowl, Cats),
    rel(ginger, is_between, no_cat, no_cat, Cats),
    rel(sassy, is_between, some_cat, some_cat, Cats),
    rel(duchess, near, clawmarks, Cats),
    rel(pipsqueak, near, (minutia2(bellball, pawprint);
			  minutia2(catnip, clawmarks)), Cats),
    !.

solution12(Cats) :-
    cat_perms(Cats),
    rel(duchess, near, fishbowl, Cats),
    rel(mrmittens, next_to, ginger, Cats),
    rel(sassy, next2_to, duchess, Cats),
    rel(ginger, next_to, blueeyes, Cats),
    rel(pipsqueak, near, pawprint, Cats),
    rel(tomcat, next3_to, sassy, Cats),
    !.

solution13(Cats) :-
    cat_perms(Cats),
    rel(mrmittens, across_from, duchess, Cats),
    rel(pipsqueak, across_from, sassy, Cats),
    rel(tomcat, near, bellball, Cats),
    rel(sassy, near, bellball, Cats),
    rel(ginger, near, catnip, Cats),
    rel(tomcat, next3_to, mrmittens, Cats),
    !.

solution14(Cats) :-
    cat_perms(Cats),
    rel(sassy, near, minutia2(catnip, sock), Cats),
    \+ rel(ginger, next_to, bell, Cats),
    \+ rel(ginger, next_to, whitepaws, Cats),
    rel(pipsqueak, next3_to, tomcat, Cats),
    \+ rel(ginger, across_from, sassy, Cats),
    \+ rel(pipsqueak, near, mouse, Cats),
    rel(duchess, next_to, pipsqueak, Cats),
    !.

solution15(Cats) :-
    fancy_perms([tomcat, ginger], [bell, whitepaws], [], 3, Cats),
    ([no_cat, _, no_cat, _, no_cat, _] = Cats;
     [_, no_cat, _, no_cat, _, no_cat] = Cats),
    rel(bell, near, pawprint, Cats),
    rel(tomcat, near, bellball, Cats),
    rel(ginger, across_from, whitepaws, Cats),
    !.

solution16(Cats) :-
    cat_perms(Cats),
    rel(tomcat, left_of, sassy, Cats),
    \+ rel(ginger, across_from, mrmittens, Cats),
    rel(duchess, near, birdcage, Cats),
    \+ rel(tomcat, next_to, pipsqueak, Cats),
    rel(pipsqueak, next3_to, mrmittens, Cats),
    !.

solution17(Cats) :-
    excluded_cat_perms([mrmittens], Cats),
    rel(no_cat, across_from, pipsqueak, Cats),
    rel(ginger, across_from, tomcat, Cats),
    rel(duchess, near, clawmarks, Cats),
    rel(sassy, near, pawprint, Cats),
    rel(no_cat, near, pawprint, Cats),
    rel(tomcat, near, sock, Cats),
    !.

solution18(Cats) :-
    fancy_perms([sassy, ginger, pipsqueak, mrmittens], [bow, blueeyes, whitepaws], [], 5, Cats),
    rel(no_cat, near, minutia2(mouse, pawprint), Cats),
    rel(sassy, is_between, some_cat, no_cat, Cats),
    rel(ginger, next2_to, bow, Cats),
    rel(pipsqueak, left_of, blueeyes, Cats),
    rel(mrmittens, left_of, sassy, Cats),
    \+ rel(ginger, next_to, whitepaws, Cats),
    !.

solution19(Cats) :-
    fancy_perms([ginger, mrmittens, tomcat], [longhair, bow], [], 4, Cats),
    [no_cat, _, _, no_cat, _, _] = Cats,
    rel(ginger, near, pawprint, Cats),
    rel(mrmittens, near, catnip, Cats),
    rel(ginger, across_from, longhair, Cats),
    rel(bow, next_to, fishbowl, Cats),
    \+ rel(tomcat, next_to, fishbowl, Cats),
    !.

solution20(Cats) :-
    cat_perms(Cats),
    rel(ginger, across_from, pipsqueak, Cats),
    rel(duchess, near, (minutia2(mouse, clawmarks);
			minutia2(catnip, sock)), Cats),
    rel(sassy, near, fishbowl, Cats),
    rel(tomcat, next_to, bell, Cats),
    \+ rel(pipsqueak, next_to, fishbowl, Cats),
    rel(mrmittens, near, (minutia2(bellball, pawprint);
			  minutia2(catnip, clawmarks)), Cats),
    !.

solution21(Cats) :-
    cat_perms(Cats),
    rel(sassy, is_between, bell, bow, Cats),
    rel(tomcat, near, catnip, Cats),
    rel(ginger, right_of, tomcat, Cats),
    \+ rel(mrmittens, near, clawmarks, Cats),
    rel(duchess, across_from, mrmittens, Cats),
    \+ rel(ginger, across_from, sock, Cats),
    !.

solution22(Cats) :-
    fancy_perms(
	[duchess, pipsqueak, sassy, ginger, tomcat],
	[longhair, whitepaws],
	[],
	5,
	Cats),
    rel(duchess, near, mouse, Cats),
    rel(no_cat, right_of, duchess, Cats),
    rel(pipsqueak, next_to, sassy, Cats),
    rel(ginger, right_of, longhair, Cats),
    rel(whitepaws, across_from, duchess, Cats),
    rel(ginger, left_of, tomcat, Cats),
    !.

solution23(Cats) :-
    cat_perms(Cats),
    rel(ginger, next_to, longhair, Cats),
    rel(tomcat, across_from, sassy, Cats),
    rel(ginger, across_from, pipsqueak, Cats),
    \+ rel(pipsqueak, next_to, fishbowl, Cats),
    rel(ginger, right_of, tomcat, Cats),
    rel(sassy, is_between, mrmittens, duchess, Cats),
    !.

solution24(Cats) :-
    cat_perms(Cats),
    rel(tomcat, across_from, sassy, Cats),
    rel(ginger, across_from, pipsqueak, Cats),
    rel(tomcat, near, bellball, Cats),
    rel(ginger, next2_to, duchess, Cats),
    rel(duchess, near, (catnip; bellball), Cats),
    \+ rel(ginger, next_to, mrmittens, Cats),
    !.

solution25(Cats) :-
    cat_perms(Cats),
    rel(ginger, left_of, tomcat, Cats),
    rel(duchess, next3_to, pipsqueak, Cats),
    rel(ginger, next2_to, pipsqueak, Cats),
    rel(sassy, right_of, mrmittens, Cats),
    rel(mrmittens, across_from, duchess, Cats),
    \+ rel(ginger, next_to, birdcage, Cats),
    !.

solution26(Cats) :-
    choose_perms(4, Cats),
    [no_cat, _, _, no_cat, _, _] = Cats,
    rel(whitepaws, near, clawmarks, Cats),
    rel(duchess, right_of, stripes, Cats),
    rel(ginger, near, pawprint, Cats),
    \+ rel(ginger, next_to, longhair, Cats),
    rel(bell, across_from, ginger, Cats),
    !.

solution27(Cats) :-
    cat_perms(Cats),
    rel(tomcat, right_of, sassy, Cats),
    \+ rel(ginger, across_from, mrmittens, Cats),
    rel(duchess, near, fishbowl, Cats),
    \+ rel(tomcat, next_to, pipsqueak, Cats),
    rel(pipsqueak, next3_to, mrmittens, Cats),
    !.


solution31(Cats) :-
    cat_perms(Cats),
    \+ rel(tomcat, next_to, mrmittens, Cats),
    forall((cat(X), rel(X, near, mouse, Cats)),
	   trait_to_cat(whitepaws, X)),
    rel(ginger, across_from, bow, Cats),
    rel(pipsqueak, across_from, whitepaws, Cats),
    rel(mrmittens, next3_to, duchess, Cats),
    !.

solution40(Cats) :-
    cat_perms(Cats),
    \+ rel(ginger, next_to, bow, Cats),
    rel(blueeyes, near, bellball, Cats),
    rel(longhair, left_of, bow, Cats),
    \+ rel(longhair, across_from, blueeyes, Cats),
    (trait_to_cat(bow, A),
     rel(A, across_from, bell, Cats),
     rel(A, right_of, stripes, Cats)),
    (trait_to_cat(bow, B),
     rel(B, next3_to, stripes, Cats),
     rel(B, left_of, whitepaws, Cats)),
    (trait_to_cat(stripes, X),
     trait_to_cat(whitepaws, Y),
     trait_to_cat(bell, Z),
     X \= Y,
     Y \= Z,
     rel(X, next2_to, Y, Cats),
     rel(X, next2_to, Z, Cats)),
    !.

pretty_solution(Solution, X) :-
    call(Solution, Cats),
    atomics_to_string(Cats, ', ', Res),
    writef('%d: %w\n', [X, Res]).

main :-
    pretty_solution(solution1, 1),
    pretty_solution(solution2, 2),
    pretty_solution(solution3, 3),
    pretty_solution(solution4, 4),
    pretty_solution(solution5, 5),
    pretty_solution(solution6, 6),
    pretty_solution(solution7, 7),
    pretty_solution(solution8, 8),
    pretty_solution(solution9, 9),
    pretty_solution(solution10, 10),
    pretty_solution(solution11, 11),
    pretty_solution(solution12, 12),
    pretty_solution(solution13, 13),
    pretty_solution(solution14, 14),
    pretty_solution(solution15, 15),
    pretty_solution(solution16, 16),
    pretty_solution(solution17, 17),
    pretty_solution(solution18, 18),
    pretty_solution(solution19, 19),
    pretty_solution(solution20, 20),
    pretty_solution(solution21, 21),
    pretty_solution(solution22, 22),
    pretty_solution(solution23, 23),
    pretty_solution(solution24, 24),
    pretty_solution(solution25, 25),
    pretty_solution(solution26, 26),
    pretty_solution(solution27, 27),
    pretty_solution(solution31, 31),
    pretty_solution(solution40, 40).
