-module(web_helper).

-export([maybe_int_list_to_float/1, extract_add_deal_args/1, deal_to_html/1, deals_to_html/1]).
-include_lib ("yaws/include/yaws_api.hrl").
-include("deal_system.hrl").

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
                    {ok, #deal{name=list_to_atom(Name),
                               timestamp=time_helper:date_and_time_to_timestamp(DateString, TimeString),
                               cost=maybe_int_list_to_float(Cost),
                               quantity=list_to_integer(Quantity)}};
                _ -> undefined
            end;
        _ -> undefined
    end.

deal_to_html(#deal{name=Name, timestamp=Timestamp, cost=Cost, quantity=Quantity}) ->
    {DateStr, TimeStr} = time_helper:timestamp_to_date_and_time(Timestamp),
    {tr, [], [{td, [], Name},
              {td, [], DateStr},
              {td, [], TimeStr},
              {td, [], ?f("~p",[Cost])},
              {td, [], integer_to_list(Quantity)}]}.

deals_to_html(Deals) ->
    lists:map(fun deal_to_html/1, Deals).

%% f(Template, Args) ->
%%     lists:flatten(io_lib:format(Template,Args)).
