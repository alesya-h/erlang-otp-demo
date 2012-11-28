%%%-------------------------------------------------------------------
%%% @author Ales Guzik <>
%%% @copyright (C) 2012, Ales Guzik
%%% @doc
%%% Module with utility functions
%%% @end
%%% Created : 28 Nov 2012 by Ales Guzik <>
%%%-------------------------------------------------------------------
-module(deals_utils).
-export([maybe_int_list_to_float/1, extract_add_deal_args/1, now_to_timestamp/1, date_and_time_to_timestamp/2, deal_to_html/1, deals_to_html/1]).
-include_lib ("yaws/include/yaws_api.hrl").
-include("deal.hrl").

%% Converts strings like "42" or "42.0" to floats (42.0)
maybe_int_list_to_float(List) ->
    try list_to_float(List)
    catch
        error:badarg ->
            float(list_to_integer(List))
    end.

extract_add_deal_args(Arg) ->
    case Arg of
        #arg{req=#http_request{method='POST'}} ->
            ReceivedData = {yaws_api:getvar(Arg, "name"),
                            yaws_api:getvar(Arg, "date"),
                            yaws_api:getvar(Arg, "time"),
                            yaws_api:getvar(Arg, "cost"),
                            yaws_api:getvar(Arg, "quantity")},
            case ReceivedData of
                {{ok, Name}, {ok, DateString},{ok, TimeString}, {ok, Cost}, {ok, Quantity}} ->
                    {ok, #deal{name=Name,
                               timestamp=date_and_time_to_timestamp(DateString, TimeString),
                               cost=maybe_int_list_to_float(Cost),
                               quantity=list_to_integer(Quantity)}};
                _ -> undefined
            end;
        _ -> undefined
    end.

now_to_timestamp({Mega, S, _Micro}) ->
    1000000 * Mega + S.

%% TODO: replace stub with implementation
date_and_time_to_timestamp(DateStr, TimeStr) ->
    now_to_timestamp(erlang:now()).

timestamp_to_date_and_time(Timestamp) ->
    {"2012-11-28","11:00"}.

deal_to_html(#deal{name=Name, timestamp=Timestamp, cost=Cost, quantity=Quantity}) ->
    {DateStr, TimeStr} = timestamp_to_date_and_time(Timestamp),
    {tr, [], [{td, [], Name},
              {td, [], DateStr},
              {td, [], TimeStr},
              {td, [], lists:flatten(io_lib:format("~p",[Cost]))},
              {td, [], integer_to_list(Quantity)}]}.

deals_to_html(Deals) ->
    lists:map(fun deal_to_html/1, Deals).
