-module(deals_yaws_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Yaws = {deals_yaws, {deals_yaws, start, []},
            permanent, 2000, worker, [deals_yaws]},
    {ok,{{one_for_all,0,1}, [Yaws]}}.
