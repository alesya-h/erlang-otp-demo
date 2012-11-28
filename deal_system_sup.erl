%%%-------------------------------------------------------------------
%%% @author Ales Guzik <me@aguzik.net>
%%% @copyright (C) 2012, Ales Guzik
%%% @doc
%%% Application supervisor
%%% @end
%%% Created : 28 Nov 2012 by Ales Guzik <me@aguzik.net>
%%%-------------------------------------------------------------------
-module(deal_system_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Port]) ->
    io:format("~p (~p) is starting...~n", [?MODULE, self()]),
    DealServerSup = {deal_server_sup, {deal_server_sup, start_link, []},
               permanent, infinity, supervisor, [deal_server_sup]},
    YawsSup = {deals_yaws_sup, {deals_yaws_sup, start_link, [Port]},
               permanent, infinity, supervisor, [deals_yaws_sup]},
    {ok, {{one_for_one, 5, 60}, [DealServerSup, YawsSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
