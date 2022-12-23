%%%-----------------------------------------------------------------------------
%%% @doc
%%% Useful file functions.
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(file_functions).
-author("nickolaswood").

%% External API
-export([
    readlines/1
]).

readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.