% This is a solution to the longest substring palindrome in Prolog.
% It is at least \Omega(n^3) in runtime, and possibly O(n^2) on a good day.
% I have an O(n) solution in LongestSubstringPalindrome.hs, but
% this was more to learn some prolog and see how easy it would be to write
% the brute force solution.

% Given a list of lists, the Result is the longest list (or the first if
% there are many such longest lists).
longest([H|T], Result) :- length(H, L), longestAcc(T, L, H, Result).
longestAcc([], _, AccRes, AccRes).
longestAcc([H|T], AccLength, _, Result) :-
    length(H, L),
    L > AccLength,
    longestAcc(T, L, H, Result).
longestAcc([H|T], AccLength, AccResult, Result) :-
    length(H, L),
    L =< AccLength,
    longestAcc(T, AccLength, AccResult, Result).

% X is a palindrome if it is its own reverse.
palindrome(X) :- reverse(X, X).

% True if Res is a suffix/prefix/sublist of X.
suffix(X, Res) :- append(Res, _, X).
prefix(X, Res) :- append(_, Res, X).
sublist(X, Res) :- prefix(X, Tmp), suffix(Tmp, Res).

% True if Output is a substring of Input and it is a palindrome.
substringPalindrome(Input, Output) :-
    sublist(Input, Output),
    palindrome(Output).

% Find all substring palindromes and then find the longest.
lsp(Input, Result) :-
    findall(Output, substringPalindrome(Input, Output), Tmp),
    longest(Tmp, Result).

% Convert an input string into a list, find the longest substring palindrome,
% and convert the list output back into a string.
longestSubstringPalindrome(Input, Result) :-
    string_to_list(Input, InputList),
    lsp(InputList, ResultList),
    string_to_list(Result, ResultList).
