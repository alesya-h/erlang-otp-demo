%%%-------------------------------------------------------------------
%%% @author Ales Guzik <me@aguzik.net>
%%% @copyright (C) 2012, Ales Guzik
%%% @doc
%%% Deal processing application.
%%% @end
%%% Created :  1 Dec 2012 by Ales Guzik <me@aguzik.net>
%%%-------------------------------------------------------------------
-module(deal_system).

-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
    application:start(?MODULE).

start(_StartType, StartArgs) ->
    case deal_system_sup:start_link(StartArgs) of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
                end.

stop(_State) ->
    ok.
