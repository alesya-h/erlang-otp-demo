-module(deal_server_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    process_flag(trap_exit, true),
    io:format("~p (~p) is starting...~n", [?MODULE, self()]),
    DealServer = {deal_server, {deal_server, start_link, []},
            permanent, 2000, worker, [deal_server]},
    {ok,{{one_for_all,3,10}, [DealServer]}}.
