%%%-------------------------------------------------------------------
%%% @author Ales Guzik <me@aguzik.net>
%%% @copyright (C) 2012, Ales Guzik
%%% @doc
%%% Deal processing server.
%%% @end
%%% Created : 27 Nov 2012 by Ales Guzik <me@aguzik.net>
%%%-------------------------------------------------------------------
-module(deal_server).

-behaviour(gen_server).

%% API
-export([start_link/0, shutdown/0, add/1, get_all/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("deal_system.hrl").
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

add(Deal) ->
    gen_server:call({global, ?SERVER}, {add, Deal}).

get_all() ->
    gen_server:call({global, ?SERVER}, get_all).

start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

shutdown() ->
    gen_server:cast({global, ?SERVER}, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit,true),
    io:format("~p (~p) is starting...~n", [?MODULE, self()]),
    {ok, ets:new(deals, [duplicate_bag,{keypos, #deal.timestamp}])}.

handle_call({add, #deal{name=Name, timestamp=Timestamp, cost=Cost, quantity=Quantity}},
            _From,
            Deals) when ?deal_spec(Name,Timestamp,Cost,Quantity) ->
    ets:insert(Deals, #deal{name=Name, timestamp=Timestamp, cost=Cost, quantity=Quantity}),
    {reply, ok, Deals};
handle_call(get_all, _From, Deals) ->
    {reply, ets:tab2list(Deals), Deals};
handle_call(_Request, _From, State) ->
    Reply = message_ignored,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
