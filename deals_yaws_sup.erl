-module(deals_yaws_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

init([Port]) ->
    process_flag(trap_exit, true),
    io:format("~p (~p) is starting...~n", [?MODULE, self()]),
    Yaws = {deals_yaws, {deals_yaws, start, [Port]},
            permanent, 2000, worker, [deals_yaws]},
    {ok,{{one_for_all,0,1}, [Yaws]}}.
