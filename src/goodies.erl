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
    forx/2,
    group_list/2
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

%% Map Functions
-export([
    maps_get_lazy/2,
    maps_get_lazy/3,
    maps_fallthrough_get/3,
    maps_fallthrough_get/2,
    maps_get_seq/2,
    maps_get_seq/3
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

group_list(List, F)
        when is_function(F, 1), is_list(List) ->
    list_functions:group_list(List, F).

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

%%%=============================================================================
%%% Map Functions
%%%=============================================================================

maps_get_lazy(KeySequence, Map) ->
    map_functions:maps_get_lazy(KeySequence, Map).

maps_get_lazy(KeySequence, Map, DefaultFun) ->
    map_functions:maps_get_lazy(KeySequence, Map, DefaultFun).

maps_fallthrough_get(Key, Maps) when is_list(Maps) ->
    map_functions:maps_fallthrough_get(Key, Maps).

maps_fallthrough_get(Key, Maps, Default) when is_list(Maps) ->
    map_functions:maps_fallthrough_get(Key, Maps, Default).

maps_get_seq(Key, Maps) when is_list(Maps) ->
    map_functions:maps_get_seq(Key, Maps).

maps_get_seq(Key, Maps, Default) when is_list(Maps) ->
    map_functions:maps_get_seq(Key, Maps, Default).