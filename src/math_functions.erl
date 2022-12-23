%%%-----------------------------------------------------------------------------
%%% @doc
%%% Empty Module built from template.
%%% @author nickolaswood
%%% @end
%%%-----------------------------------------------------------------------------

-module(math_functions).
-author("nickolaswood").

%% External API
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
%%% API
%%%=============================================================================

intersects_fully({R1, L1}, {R2, L2}) ->
    R1 =< R2 andalso L1 >= L2.

intersects({R1, L1}, {R2, L2}) ->
    (R1 =< R2 andalso L1 >= R2) orelse (R1 =< L2 andalso R1 >= R2).

sign(X) when X < 0 -> -1;
sign(X) when X > 0 -> 1;
sign(_) -> 0.

mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_Y) -> 0.

mul({X1, Y1}, {X2, Y2}) ->
    {X1 * X2, Y1 * Y2}.

add({X1, Y1}, {X2, Y2}) ->
    {X1 + X2, Y1 + Y2}.

sub({X1, Y1}, {X2, Y2}) ->
    {X1 - X2, Y1 - Y2}.

dir({X, 0}) when X > 0 ->
    {1, 0};
dir({_, 0}) ->
    {-1, 0};
dir({0, Y}) when Y > 0 ->
    {0, 1};
dir({0, _})->
    {0, -1}.

%%%=============================================================================
%%% Eunit Tests
%%%=============================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    ?assertEqual(true, true).

-endif.