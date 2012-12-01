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
    Result = (
      catch
          case Arg of
              #arg{req=#http_request{method='POST'}} ->
                  ReceivedData = {yaws_api:getvar(Arg, "name"),
                                  yaws_api:getvar(Arg, "date"),
                                  yaws_api:getvar(Arg, "time"),
                                  yaws_api:getvar(Arg, "cost"),
                                  yaws_api:getvar(Arg, "quantity")},
                  case ReceivedData of
                      {{ok, NameStr}, {ok, DateStr},{ok, TimeStr}, {ok, CostStr}, {ok, QuantityStr}} ->
                          Name=list_to_atom(NameStr),
                          Timestamp=time_helper:date_and_time_to_timestamp(DateStr, TimeStr),
                          Cost=maybe_int_list_to_float(CostStr),
                          Quantity=list_to_integer(QuantityStr),
                          {ok, #deal{name=Name,
                                     timestamp=Timestamp,
                                     cost=Cost,
                                     quantity=Quantity}};
                      _ -> undefined
                  end;
              _ -> undefined
          end),
    case Result of
        {ok, _Deal} -> Result;
        _ -> undefined
    end.

deal_to_html(#deal{name=Name, timestamp=Timestamp, cost=Cost, quantity=Quantity}) ->
    {DateStr, TimeStr} = time_helper:timestamp_to_date_and_time(Timestamp),
    {tr, [], [{td, [], atom_to_list(Name)},
              {td, [], DateStr},
              {td, [], TimeStr},
              {td, [], ?f("~p",[Cost])},
              {td, [], integer_to_list(Quantity)}]}.

deals_to_html(Deals) ->
    lists:map(fun deal_to_html/1, Deals).
