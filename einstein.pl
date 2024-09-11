%% EINSTEIN'S RIDDLE
%% WHO OWNS THE FISH?

%% CLUES:
%% 1. THE BRIT LIVES IN THE HOUSE WITH RED WALLS.
%% 2. THE SWEDE HAS A DOG.
%% 3. THE DANE DRINKS TEA.
%% 4. THE HOUSE WITH GREEN WALLS IS DIRECTLY TO THE LEFT OF THE HOUSE WITH WHITE WALLS.
%% 5. THE OWNER OF THE HOUSE WITH GREEN WALLS DRINKS COFFEE.
%% 6. THE PERSON WHO SMOKES PALL MALL CIGARS OWNS A BIRD.
%% 7. THE OWNER OF THE HOUSE WITH YELLOW WALLS SMOKES DUNHILL CIGARS.
%% 8. THE MAN LIVING IN THE CENTER HOUSE DRINKS MILK.
%% 9. THE NORWEGIAN LIVES IN THE FIRST HOUSE.
%% 10. THE MAN WHO SMOKES BLENDS LIVES NEXT TO THE CAT OWNER.
%% 11. THE HORSE'S OWNER LIVES NEXT TO THE MAN WHO SMOKES DUNHILL.
%% 12. THE MAN WHO SMOKES BLUE MASTER DRINKS ROOT BEER.
%% 13. THE GERMAN SMOKES PRINCE.
%% 14. THE NORWEGIAN LIVES NEXT TO THE HOUSE WITH BLUE WALLS.
%% 15. THE MAN WHO SMOKES BLENDS HAS A NEXT-DOOR NEIGHBOR WHO DRINKS WATER.

%% `solution_` is a fairly literal translation of the clues into statements and
%% likely considers (5!)^5 = 24883200000 combinations.

%% `solution` is a restructuring of these statements in order to reduce the
%% search space. For example, if the permutation of wall colors cannot lead to
%% a valid solution, there is no need to consider the permutations of any other
%% sets.

walls(red).
walls(green).
walls(blue).
walls(yellow).
walls(white).

nation(brit).
nation(swede).
nation(dane).
nation(norwegian).
nation(german).

pet(dog).
pet(bird).
pet(fish).
pet(cat).
pet(horse).

drinks(tea).
drinks(coffee).
drinks(milk).
drinks(water).
drinks(root_beer).

smokes(pall_mall).
smokes(dunhill).
smokes(blends).
smokes(blue_master).
smokes(prince).

wall_perms(Ws) :-
    findall(W, walls(W), T),
    permutation(T, Ws).

nation_perms(Ns) :-
    findall(N, nation(N), T),
    permutation(T, Ns).

pet_perms(Ps) :-
    findall(P, pet(P), T),
    permutation(T, Ps).

drink_perms(Ds) :-
    findall(D, drinks(D), T),
    permutation(T, Ds).

smoke_perms(Ss) :-
    findall(S, smokes(S), T),
    permutation(T, Ss).

left_of(X, Y) :- Y is X + 1.
right_of(X, Y) :- Y is X - 1.
next_to(X, Y) :- left_of(X, Y);
		 right_of(X, Y).

solution(Walls, Nations, Pets, Drinks, Smokes) :-
    wall_perms(Walls),
    R14b is 1, % blue is second
    nth0(R14b, Walls, blue),
    nth0(R4a, Walls, green),
    nth0(R4b, Walls, white),
    left_of(R4a, R4b),

    pet_perms(Pets),
    nth0(R11a, Pets, horse),
    nth0(R7, Walls, yellow),
    R11b = R7,
    next_to(R11a, R11b),

    smoke_perms(Smokes),
    nth0(R10a, Smokes, blends),
    nth0(R10b, Pets, cat),
    next_to(R10a, R10b),
    nth0(R6, Pets, bird),
    nth0(R6, Smokes, pall_mall),
    nth0(R7, Smokes, dunhill),

    drink_perms(Drinks),
    R8 is 2, % milk is third (center)
    nth0(R8, Drinks, milk),
    R15a = R10a,
    nth0(R15b, Drinks, water),
    next_to(R15a, R15b),
    R5 = R4a,
    nth0(R5, Drinks, coffee),
    nth0(R12, Smokes, blue_master),
    nth0(R12, Drinks, root_beer),

    nation_perms(Nations),
    R9 is 0, % norwegian is first
    nth0(R9, Nations, norwegian),
    nth0(R1, Walls, red),
    nth0(R1, Nations, brit),
    nth0(R2, Pets, dog),
    nth0(R2, Nations, swede),
    nth0(R3, Drinks, tea),
    nth0(R3, Nations, dane),
    nth0(R13, Smokes, prince),
    nth0(R13, Nations, german),

    !.

solution_(Walls, Nations, Pets, Drinks, Smokes) :-
    wall_perms(Walls),
    nation_perms(Nations),
    pet_perms(Pets),
    drink_perms(Drinks),
    smoke_perms(Smokes),

    %% 1. THE BRIT LIVES IN THE HOUSE WITH RED WALLS.
    nth0(R1, Nations, brit),
    nth0(R1, Walls, red),

    %% 2. THE SWEDE HAS A DOG.
    nth0(R2, Nations, swede),
    nth0(R2, Pets, dog),

    %% 3. THE DANE DRINKS TEA.
    nth0(R3, Nations, dane),
    nth0(R3, Drinks, tea),

    %% 4. THE HOUSE WITH GREEN WALLS IS DIRECTLY TO THE LEFT OF THE HOUSE WITH WHITE WALLS.
    nth0(R4a, Walls, green),
    nth0(R4b, Walls, white),
    left_of(R4a, R4b),

    %% 5. THE OWNER OF THE HOUSE WITH GREEN WALLS DRINKS COFFEE.
    R5 = R4a,
    nth0(R5, Drinks, coffee),

    %% 6. THE PERSON WHO SMOKES PALL MALL CIGARS OWNS A BIRD.
    nth0(R6, Smokes, pall_mall),
    nth0(R6, Pets, bird),

    %% 7. THE OWNER OF THE HOUSE WITH YELLOW WALLS SMOKES DUNHILL CIGARS.
    nth0(R7, Walls, yellow),
    nth0(R7, Smokes, dunhill),

    %% 8. THE MAN LIVING IN THE CENTER HOUSE DRINKS MILK.
    nth0(R8, Drinks, milk),
    R8 is 2,

    %% 9. THE NORWEGIAN LIVES IN THE FIRST HOUSE.
    nth0(R9, Nations, norwegian),
    R9 is 0,

    %% 10. THE MAN WHO SMOKES BLENDS LIVES NEXT TO THE CAT OWNER.
    nth0(R10a, Smokes, blends),
    nth0(R10b, Pets, cat),
    next_to(R10a, R10b),

    %% 11. THE HORSE'S OWNER LIVES NEXT TO THE MAN WHO SMOKES DUNHILL.
    nth0(R11a, Pets, horse),
    R11b = R7,
    next_to(R11a, R11b),

    %% 12. THE MAN WHO SMOKES BLUE MASTER DRINKS ROOT BEER.
    nth0(R12, Smokes, blue_master),
    nth0(R12, Drinks, root_beer),

    %% 13. THE GERMAN SMOKES PRINCE.
    nth0(R13, Nations, german),
    nth0(R13, Smokes, prince),

    %% 14. THE NORWEGIAN LIVES NEXT TO THE HOUSE WITH BLUE WALLS.
    R14a = R9,
    nth0(R14b, Walls, blue),
    next_to(R14a, R14b),

    %% 15. THE MAN WHO SMOKES BLENDS HAS A NEXT-DOOR NEIGHBOR WHO DRINKS WATER.
    R15a = R10a,
    nth0(R15b, Drinks, water),
    next_to(R15a, R15b).

main :-
    solution(Walls, _, Pets, _, _),
    nth0(Idx, Pets, fish),
    nth0(Idx, Walls, HouseColor),
    format('The fish is in the ~s house~n', HouseColor).
