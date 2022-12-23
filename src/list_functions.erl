%%%-----------------------------------------------------------------------------
%%% @doc
%%% Helpful list functions.
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(list_functions).
-author("nickolaswood").

%% External API
-export([
    fori/2,
    foldli/2,
    acc/2,
    find_repeating/2,
    foldx/3,
    forx/2
]).

fori(F, List) when is_function(F, 2), is_list(List) ->
    case List of
        [Hd | Tail] ->
            F(0, Hd),
            fori(F, Tail, 1);
        [] -> []
    end.

fori(F, [Hd | Tail], Index) ->
    F(Index, Hd),
    fori(F, Tail, Index + 1);
fori(_F, [], _Index) ->
    ok.

foldli(F, List) when is_function(F, 2), is_list(List) ->
    case List of
        [Hd | Tail] -> foldli(F, [F(0, Hd)], Tail, 1);
        [] -> []
    end.

foldli(F, Acc, [Hd | Tail], Index) when is_list(Acc) ->
    foldli(F, [F(Index, Hd) | Acc], Tail, Index + 1);
foldli(_F, Acc, [], _Index) ->
    Acc.

acc(F, List) when is_function(F, 1), is_list(List) ->
    case List of
        [Hd | Tail] -> acc_1(F, [F(Hd), []], Tail);
        [] -> []
    end.

acc_1(F, Acc, [Hd | Tail]) when is_list(Acc) ->
    acc_1(F, [F(Hd), Acc], Tail);
acc_1(_F, Acc, []) ->
    Acc.

find_repeating([Repeating | _], []) ->
    Repeating;
find_repeating(ListA, [NextList | Rest]) ->
    find_repeating(
        lists:filter(
            fun(Elem) -> lists:member(Elem, NextList) end,
            ListA),
        Rest).

foldx(F, Accu, List) when is_function(F, 2) ->
    case List of
        [Hd | Tail] -> foldx_1(F, F(Hd, Accu), Tail);
        [] -> Accu
    end.

foldx_1(_, {exit, Value}, _) ->
    Value;
foldx_1(F, Accu, [Hd | Tail]) ->
    foldx_1(F, F(Hd, Accu), Tail);
foldx_1(_F, Accu, []) ->
    Accu.

forx(F, [Hd | Tail]) ->
    case F(Hd) of
        exit -> ok;
        _ -> forx(F, Tail)
    end;
forx(_F, []) ->
    ok.

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.