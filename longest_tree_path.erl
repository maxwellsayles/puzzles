% Solution for longest path in a binary tree.
% Either the height of the left + height of the right + 2,
% or the longest path on the left, or the longest path on the right.
%
% Invoke this using:
% spawn(longest_tree_path, p_longest_path,
%       [self(), root, 16, longest_tree_path:random_tree(1,10000000)]).	
% for the parallel version.
% You can get the result with: flush().
%
% The serial version can be invoked with:
% longest_tree_path:longest_path(longest_tree_path:random_tree(1,10000000)).
%
% $ erl
% Erlang R16B03 (erts-5.10.4) [source] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]
% 
% Eshell V5.10.4  (abort with ^G)
% 1> c('longest_tree_path.erl').
% {ok,longest_tree_path}
% 2> longest_tree_path:longest_path(longest_tree_path:random_tree(1,10000000)).
% {56,110}
% 3>
   
-module(longest_tree_path).
-export([random_tree/2,
	 p_random_tree/4,
	 longest_path/1,
	 p_longest_path/4]).


random_tree(Lo, Hi) when Lo > Hi ->
    empty;
random_tree(Lo, Hi) ->
    Mid = Lo + random:uniform(Hi-Lo+1) - 1,
    {Mid, random_tree(Lo, Mid-1), random_tree(Mid+1, Hi)}.

p_random_tree(Pid, _, Lo, Hi) when Lo > Hi ->
    Pid ! {self(), empty};
p_random_tree(Pid, 0, Lo, Hi) ->
    Pid ! {self(), random_tree(Lo, Hi)};
p_random_tree(Pid, ParLevels, Lo, Hi) ->
    Mid = Lo + random:uniform(Hi-Lo+1) - 1,
    LPid = spawn(longest_tree_path, p_random_tree,
		 [self(), ParLevels-1, Lo, Mid-1]),
    RPid = spawn(longest_tree_path, p_random_tree,
		 [self(), ParLevels-1, Mid+1, Hi]),
    receive
	{LPid, L} ->
	    receive {RPid2, R} when RPid =:= RPid2 ->
		    Pid ! {self(), {Mid, L, R}}
	    end
    end.
    

longest_path(empty) ->
    {0, 0};
longest_path({_, Left, Right}) ->
    {LHeight, LPath} = longest_path(Left),
    {RHeight, RPath} = longest_path(Right),
    Height = 1 + erlang:max(LHeight, RHeight),
    Path = lists:max([LPath, RPath, LHeight + RHeight + 2]),
    {Height, Path}.

p_longest_path(Pid, Token, _, empty) ->
    Pid ! {Token, 0, 0};
p_longest_path(Pid, Token, 0, Node) ->
    {Height, Path} = longest_path(Node),
    Pid ! {Token, Height, Path};
p_longest_path(Pid, Token, ParLevels, {_, Left, Right}) ->
    spawn(longest_tree_path, p_longest_path,
	  [self(), left, ParLevels-1, Left]),
    spawn(longest_tree_path, p_longest_path,
	  [self(), right, ParLevels-1, Right]),
    One = receive X -> X end,
    Two = receive Y -> Y end,
    case {One, Two} of
	{{left, LHeight1, LPath1}, {right, RHeight1, RPath1}} ->
	    Height1 = 1 + erlang:max(LHeight1, RHeight1),
	    Path1 = lists:max([LPath1, RPath1, LHeight1 + RHeight1 + 2]),
	    Pid ! {Token, Height1, Path1};
	{{right, RHeight2, RPath2}, {left, LHeight2, LPath2}} ->
	    Height2 = 1 + erlang:max(LHeight2, RHeight2),
	    Path2 = lists:max([LPath2, RPath2, LHeight2 + RHeight2 + 2]),
	    Pid ! {Token, Height2, Path2}
    end.

