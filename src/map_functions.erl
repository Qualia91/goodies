%%%-----------------------------------------------------------------------------
%%% @doc
%%% Helpful map functions.
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(map_functions).
-author("nickolaswood").

%% External API
-export([
    maps_get_lazy/2,
    maps_get_lazy/3,
    maps_fallthrough_get/3,
    maps_fallthrough_get/2,
    maps_get_seq/2,
    maps_get_seq/3
]).

maps_get_lazy([], Map, DefaultFun) when is_function(DefaultFun) ->
    Map;
maps_get_lazy([NextKey | KeySequence], Map, DefaultFun) when is_map(Map) andalso is_function(DefaultFun) ->
    case maps:get(NextKey, Map, false) of
        false -> DefaultFun();
        Value -> maps_get_lazy(KeySequence, Value, DefaultFun)
    end;
maps_get_lazy(Key, Map, DefaultFun) ->
    case maps:get(Key, Map, false) of
        false -> DefaultFun();
        Value -> Value
    end.

maps_get_lazy([], Map) ->
    Map;
maps_get_lazy([NextKey | KeySequence], Map) when is_map(Map) ->
    case maps:get(NextKey, Map, false) of
        false -> error({badkey, NextKey});
        Value -> maps_get_lazy(KeySequence, Value)
    end;
maps_get_lazy(Key, Map) ->
    case maps:get(Key, Map, false) of
        false -> error({badkey, Key});
        Value -> Value
    end.

maps_fallthrough_get(_Key, [], Default) ->
    Default;
maps_fallthrough_get(Key, [NextMap | MapsSequence], Default) ->
    case maps:get(Key, NextMap, not_found) of
        not_found -> maps_fallthrough_get(Key, MapsSequence, Default);
        Value -> Value
    end.

maps_fallthrough_get(Key, []) ->
    error({badkey, Key});
maps_fallthrough_get(Key, [NextMap | MapsSequence]) ->
    case maps:get(Key, NextMap, not_found) of
        not_found -> maps_fallthrough_get(Key, MapsSequence);
        Value -> Value
    end.

maps_get_seq([Key], Map, Default) ->
    maps:get(Key, Map, Default);
maps_get_seq([Key | Rest], Map, Default) ->
    case maps:get(Key, Map, not_found) of
        not_found -> Default;
        Value -> maps_get_seq(Rest, Value, Default)
    end.

maps_get_seq([Key], Map) ->
    maps:get(Key, Map);
maps_get_seq([Key | Rest], Map) ->
    case maps:get(Key, Map, not_found) of
        not_found -> error({badkey, Key});
        Value -> maps_get_seq(Rest, Value)
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

maps_get_lazy_test() ->
    Map = #{
        a => b,
        c => #{
            d => e,
            f => g
        }
    },

    DefaultFun = fun() -> x end,

    ?assertEqual(b, maps_get_lazy(a, Map, DefaultFun)),
    ?assertEqual(x, maps_get_lazy(y, Map, DefaultFun)),
    ?assertEqual(e, maps_get_lazy([c, d], Map, DefaultFun)),
    ?assertEqual(x, maps_get_lazy([c, y], Map, DefaultFun)),
    ?assertError({badkey,x}, maps_get_lazy(x, Map)),
    ?assertError({badkey,x}, maps_get_lazy([c,x], Map)).

maps_fallthrough_get_test() ->
    Map1 = #{
        a => b
    },
    Map2 = #{
        a => not_b,
        c => d
    },

    ?assertEqual(b, maps_fallthrough_get(a, [Map1, Map2])),
    ?assertEqual(d, maps_fallthrough_get(c, [Map1, Map2])),
    ?assertEqual(x, maps_fallthrough_get(d, [Map1, Map2], x)),
    ?assertError({badkey,x}, maps_fallthrough_get(x, [Map1, Map2])).

maps_get_seq_test() ->
    Map = #{
        a => b,
        c => #{
            d => e,
            f => #{
                g => h
            }
        }
    },

    ?assertEqual(b, maps_get_seq([a], Map, x)),
    ?assertEqual(h, maps_get_seq([c, f, g], Map, x)),
    ?assertEqual(x, maps_get_seq([c, f, a], Map, x)),
    ?assertEqual(b, maps_get_seq([a], Map)),
    ?assertEqual(h, maps_get_seq([c, f, g], Map)),
    ?assertError({badkey,x}, maps_get_seq([x], Map)),
    ?assertError({badkey,a}, maps_get_seq([c, f, a], Map)).

-endif.