%%%-----------------------------------------------------------------------------
%%% @doc
%%% Interface for all goodies functions.
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(goodies).
-author("nickolaswood").

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% List Functions
-export([
    fori/2,
    foldli/2,
    acc/2,
    find_repeating/2,
    foldlx/3,
    forx/2
]).

%% File Functions
-export([
    readlines/1
]).

%% Regex Functions
-export([
    regex_capture/3
]).

%% Math Functions
-export([
    intersects_fully/2,
    intersects/2,
    sign/1,
    mod/2,
    mul/2,
    add/2,
    sub/2,
    dir/1
]).

%%%=============================================================================
%%% List Functions
%%%=============================================================================

fori(F, List) when is_function(F, 2), is_list(List) ->
    list_functions:fori(F, List).

foldli(F, List) when is_function(F, 2), is_list(List) ->
    list_functions:foldli(F, List).

acc(F, List) when is_function(F, 1), is_list(List) ->
    list_functions:acc(F, List).

find_repeating(StartingList, RestOfLists)
        when is_list(StartingList), is_list(RestOfLists) ->
    list_functions:find_repeating(StartingList, RestOfLists).

foldlx(F, Accu, List)
        when is_function(F, 2), is_list(List) ->
    list_functions:foldlx(F, Accu, List).

forx(F, List)
        when is_function(F, 1), is_list(List) ->
    list_functions:forx(F, List).

%%%=============================================================================
%%% File Functions
%%%=============================================================================

readlines(FileName) ->
    file_functions:readlines(FileName).

%%%=============================================================================
%%% Regex Functions
%%%=============================================================================

regex_capture(MatchPattern, RegexPattern, Line)
        when is_binary(MatchPattern), is_binary(RegexPattern), is_binary(Line) ->
    regex_functions:regex_capture(MatchPattern, RegexPattern, Line).

%%%=============================================================================
%%% Math Functions
%%%=============================================================================

intersects_fully(A, B) ->
    math_functions:intersects_fully(A, B).

intersects(A, B) ->
    math_functions:intersects(A, B).

sign(Val) ->
    math_functions:sign(Val).

mod(X, Y) ->
    math_functions:mod(X, Y).

mul(X, Y) ->
    math_functions:mul(X, Y).

add(X, Y) ->
    math_functions:add(X, Y).

sub(X, Y) ->
    math_functions:sub(X, Y).

dir(X) ->
    math_functions:dir(X).