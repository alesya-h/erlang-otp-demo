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
-export([start_link/0, shutdown/0,
         get_all/0,
         add_deal/1, add_deal/4,
         gen_report/1, gen_report/4
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("deal_system.hrl").
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

add_deal(Name, Timestamp, Cost, Quantity) ->
    add_deal(#deal{name=Name, timestamp=Timestamp, cost=Cost, quantity=Quantity}).
add_deal(Deal) ->
    gen_server:call({global, ?SERVER}, {add_deal, Deal}).

gen_report(DealName, FromTimestamp, ToTimestamp, ByTimestamp) ->
    gen_report(#report_in{deal_name=DealName, from_timestamp=FromTimestamp, to_timestamp=ToTimestamp, by_timestamp=ByTimestamp}).
gen_report(ReportIn) ->
    gen_server:call({global, ?SERVER}, {gen_report, ReportIn}).

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

handle_call({add_deal, #deal{name=Name, timestamp=Timestamp, cost=Cost, quantity=Quantity}},
            _From,
            Deals) when ?deal_spec(Name,Timestamp,Cost,Quantity) ->
    ets:insert(Deals, #deal{name=Name, timestamp=Timestamp, cost=Cost, quantity=Quantity}),
    {reply, ok, Deals};

handle_call({gen_report, #report_in{deal_name=DealName, from_timestamp=FromTimestamp, to_timestamp=ToTimestamp, by_timestamp=ByTimestamp}},
            _From, Deals) when ?report_in_spec(DealName, FromTimestamp, ToTimestamp, ByTimestamp) ->
    NumberOfBuckets = 1 + (ToTimestamp - FromTimestamp) div ByTimestamp,
    Report = ets:foldl(fun(Deal, Buckets) ->
                               #deal{name=Name, timestamp=Timestamp, cost=Cost, quantity=Quantity} = Deal,
                               if
                                   Name == DealName, Timestamp >= FromTimestamp, Timestamp =< ToTimestamp ->
                                       BucketNumber = (Timestamp - FromTimestamp) div ByTimestamp,
                                       case array:get(BucketNumber,Buckets) of
                                           undefined ->
                                               ReportRecord = #report_out{open_timestamp=Timestamp, close_timestamp=Timestamp,
                                                                          open_cost=Cost, close_cost=Cost,
                                                                          min_cost=Cost, max_cost=Cost, quantity=Quantity},
                                               array:set(BucketNumber, ReportRecord, Buckets);
                                           #report_out{open_timestamp=OpenTimestamp, close_timestamp=CloseTimestamp,
                                                       open_cost=OpenCost, close_cost=CloseCost,
                                                       min_cost=MinCost, max_cost=MaxCost, quantity=CurrentQuantity} ->
                                               NewOpenTimestamp = min(OpenTimestamp, Timestamp),
                                               NewOpenCost = ?If(NewOpenTimestamp == Timestamp, Cost, OpenCost),
                                               NewCloseTimestamp = max(CloseTimestamp, Timestamp),
                                               NewCloseCost = ?If(NewCloseTimestamp == Timestamp, Cost, CloseCost),
                                               NewMinCost = min(MinCost, Cost),
                                               NewMaxCost = max(MaxCost, Cost),
                                               ReportRecord = #report_out{open_timestamp=NewOpenTimestamp, close_timestamp=NewCloseTimestamp,
                                                                          open_cost=NewOpenCost, close_cost=NewCloseCost,
                                                                          min_cost=NewMinCost, max_cost=NewMaxCost, quantity=(CurrentQuantity+Quantity)},
                                               array:set(BucketNumber, ReportRecord, Buckets)
                                       end
                                       ;
                                   true -> Buckets
                               end
                       end,
                       array:new(NumberOfBuckets), Deals),
    {reply, array:to_list(Report), Deals};

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
