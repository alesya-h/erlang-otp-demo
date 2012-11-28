%%%-------------------------------------------------------------------
%%% @author Ales Guzik <me@aguzik.net>
%%% @copyright (C) 2012, Ales Guzik
%%% @doc
%%% Deal interface web server.
%%% @end
%%% Created : 27 Nov 2012 by Ales Guzik <me@aguzik.net>
%%%-------------------------------------------------------------------
-module(deals_yaws).
-compile(export_all).

start(Port) ->
    {ok, spawn(?MODULE, run, [Port])}.

run(Port) ->
    io:format("~p (~p) is starting...~n", [?MODULE, self()]),
    Id = "deals_yaws",
    GconfList = [{id, Id}],
    Docroot = "./public",
    SconfList = [{port, Port},
                 {servername, "deals_yaws"},
                 {listen, {0,0,0,0}},
                 {docroot, Docroot}],
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(deals_yaws_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.
