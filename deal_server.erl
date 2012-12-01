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

-include("deal.hrl").
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

add(Deal) ->
    gen_server:call({global, ?SERVER}, {add, Deal}).

get_all() ->
    gen_server:call({global, ?SERVER}, get_all).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

shutdown() ->
    gen_server:cast({global, ?SERVER}, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit,true),
    io:format("~p (~p) is starting...~n", [?MODULE, self()]),
    {ok, ets:new(deals, [duplicate_bag,{keypos, #deal.timestamp}])}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add, #deal{name=Name, timestamp=Timestamp, cost=Cost, quantity=Quantity}}, _From, Deals) when is_atom(Name), is_float(Cost), is_integer(Quantity) ->
    ets:insert(Deals, #deal{name=Name, timestamp=Timestamp, cost=Cost, quantity=Quantity}),
    {reply, ok, Deals};
handle_call(get_all, _From, Deals) ->
    {reply, ets:tab2list(Deals), Deals};
handle_call(_Request, _From, State) ->
    Reply = message_ignored,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
