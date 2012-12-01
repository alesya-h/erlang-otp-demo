%%%-------------------------------------------------------------------
%%% @author Ales Guzik <me@aguzik.net>
%%% @copyright (C) 2012, Ales Guzik
%%% @doc
%%% Deal server unit tests
%%% @end
%%% Created : 27 Nov 2012 by Ales Guzik <me@aguzik.net>
%%%-------------------------------------------------------------------
-module(deal_server_test).

-include("deal_system.hrl").
-include_lib("eunit/include/eunit.hrl").

add_test_() ->
    {setup, fun() ->
                    deal_server:start_link()
            end,
     fun(_) ->
             deal_server:shutdown()
     end,
     fun(_) -> generate_add_tests() end}.

generate_add_tests() ->
    [
     fun() ->
             ?_assertEqual(0, length(deals_server:all_deals()))
     end,
     fun() ->
             deal_server:add_deal(#deal{name="ECHO", timestamp=time_helper:now_to_timestamp(erlang:now()), cost=21.0, quantity=1100}),
             deal_server:add_deal(#deal{name="ECHO", timestamp=time_helper:now_to_timestamp(erlang:now()), cost=21.0, quantity=1100}),
             deal_server:add_deal(#deal{name="GOOG", timestamp=time_helper:now_to_timestamp(erlang:now()), cost=21.0, quantity=1100}),
             ?_assertEqual(3, length(deals_server:all_deals()))
     end
    ].
report_test_() ->
    {setup, fun() -> deal_server:start_link() end,
     fun(_) ->
             deal_server:shutdown()
     end,
     fun(_) -> generate_report_tests() end}.

generate_report_tests() ->
    [].
