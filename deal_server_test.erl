%%%-------------------------------------------------------------------
%%% @author Ales Guzik <me@aguzik.net>
%%% @copyright (C) 2012, Ales Guzik
%%% @doc
%%% Deal server unit tests
%%% @end
%%% Created : 27 Nov 2012 by Ales Guzik <me@aguzik.net>
%%%-------------------------------------------------------------------
-module(deal_server_test).

-include_lib("eunit/include/eunit.hrl").

add_test_() ->
    {setup, fun() -> deal_server:start_link() end,
     fun(_) ->
             deal_server:shutdown()
     end,
     fun(_) -> generate_add_tests() end}.

generate_add_tests() ->
    [fun() -> ?_assertEqual(1,2) end].

report_test_() ->
    {setup, fun() -> deal_server:start_link() end,
     fun(_) ->
             deal_server:shutdown()
     end,
     fun(_) -> generate_report_tests() end}.

generate_report_tests() ->
    [].
