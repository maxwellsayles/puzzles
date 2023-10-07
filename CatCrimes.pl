%% Solver for the logic game "Cat Crimes".

:- discontiguous across_from/2.
:- discontiguous left_of/2.
:- discontiguous in_front_of/2.
:- discontiguous minutiae/2.
:- discontiguous next_to/2.

cat(duchess).
cat(ginger).
cat(mr_mittens).
cat(pip_squeak).
cat(sassy).
cat(tom_cat).

place(birdcage).
place(coffee_cup).
place(fish_bowl).
place(plant).
place(shoes).
place(yarn).

minutiae(birdcage, [claw_marks, mouse]).
minutiae(coffee_cup, [bell_ball, paw_print]).
minutiae(shoes, [catnip, claw_marks]).
minutiae(fish_bowl, [bell_ball, sock]).
minutiae(yarn, [mouse, paw_print]).
minutiae(plant, [catnip, sock]).

across_from(birdcage, fish_bowl).
across_from(coffee_cup, plant).
across_from(shoes, yarn).
across_from(fish_bowl, birdcage).
across_from(yarn, shoes).
across_from(plant, coffee_cup).

left_of(coffee_cup, birdcage).
left_of(shoes, coffee_cup).
left_of(fish_bowl, shoes).
left_of(yarn, fish_bowl).
left_of(plant, yarn).
left_of(birdcage, plant).

right_of(A, B) :-
    place(A),
    place(B),
    left_of(B, A).

next_to(A, B) :-
    place(A),
    place(B),
    (left_of(A, B); right_of(A, B)).

%% "In front of" when "across from"
in_front_of(X, A) :-
    cat(X),
    cat(Y),
    place(A),
    place(B),
    across_from(X, Y),
    in_front_of(Y, B),
    across_from(A, B).

%% "In front of" when "left of"
in_front_of(X, A) :-
    cat(X),
    cat(Y),
    place(A),
    place(B),
    left_of(X, Y),
    in_front_of(Y, B),
    left_of(A, B).

%% "In front of" when "next to"
in_front_of(X, A) :-
    cat(X),
    place(A),
    place(B),
    next_to(X, B),
    next_to(A, B).

%% "In front of" given a minutiae.
in_front_of(X, A) :-
    cat(X),
    place(A),
    minutiae(A, MinA),
    minutiae(X, MinX),
    subset(MinX, MinA).

pretty_print(Cat, Place, Res) :-
    swritef(Res, '%w => %w', [Cat, Place]).

% List of all participating cats.
solution(Res) :-
    findall(X, cat(X), Xs),
    exclude(sleeping, Xs, Cats),
    maplist(in_front_of, Cats, Places),
    sort(Places, SortedPlaces),
    length(Cats, LengthCats),
    length(SortedPlaces, LengthPlaces),
    LengthCats == LengthPlaces,
    maplist(pretty_print, Cats, Places, Res).

%% Example of expressing card #1
sleeping(mr_mittens).
sleeping(pip_squeak).
minutiae(tom_cat, [catnip, sock]).
across_from(sassy, tom_cat).
next_to(ginger, fish_bowl).
left_of(duchess, sassy).


%% bow(duchess).
%% bow(tom_cat).

%% stripes(ginger).
%% stripes(pip_squeak).

%% long_haired(...)

%% eyes(blue, ginger).
%% eyes(blue, tom_cat).
%% eyes(green, sassy).
%% eyes(green, pip_squeak).
%% eyes(yellow, duchess).
%% eyes(yellow, mr_mittens).
