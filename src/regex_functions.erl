%%%-----------------------------------------------------------------------------
%%% @doc
%%% Useful regex functions.
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(regex_functions).
-author("nickolaswood").

%% External API
-export([
    regex_capture/3
]).

regex_capture(MatchPattern, RegexPattern, Line) ->
    case re:run(Line, RegexPattern, [{capture, all_names, binary}]) of
        {match, MatchPattern} -> MatchPattern;
        _ -> false
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.