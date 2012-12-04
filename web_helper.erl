-module(web_helper).

-export([maybe_int_list_to_float/1, extract_add_deal_args/1, extract_get_report_args/1, deals_to_html/1, report_to_html/2]).
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

extract_get_report_args(Arg) ->
    Result = (
      catch
          case Arg of
              #arg{req=#http_request{method='POST'}} ->
                  ReceivedData = {yaws_api:getvar(Arg, "name"),

                                  yaws_api:getvar(Arg, "from_date"),
                                  yaws_api:getvar(Arg, "from_time"),
                                  yaws_api:getvar(Arg, "to_date"),
                                  yaws_api:getvar(Arg, "to_time"),

                                  yaws_api:getvar(Arg, "by_months"),
                                  yaws_api:getvar(Arg, "by_weeks"),
                                  yaws_api:getvar(Arg, "by_days"),
                                  yaws_api:getvar(Arg, "by_hours"),
                                  yaws_api:getvar(Arg, "by_minutes")},
                  case ReceivedData of
                      {{ok, NameStr},
                       {ok, FromDateStr}, {ok, FromTimeStr},
                       {ok, ToDateStr}, {ok, ToTimeStr},
                       {ok, ByMonthsStr}, {ok, ByWeeksStr}, {ok, ByDaysStr}, {ok, ByHoursStr}, {ok, ByMinutesStr}} ->
                          Name = list_to_atom(NameStr),
                          FromTimestamp = time_helper:date_and_time_to_timestamp(FromDateStr, FromTimeStr),
                          ToTimestamp = time_helper:date_and_time_to_timestamp(ToDateStr, ToTimeStr),
                          By = lists:foldl(fun(X,Sum) -> X+Sum end, 0,
                                           lists:zipwith(fun(Str,Multiplier) -> list_to_integer(Str) * Multiplier end,
                                                         [ByMonthsStr, ByWeeksStr, ByDaysStr, ByHoursStr, ByMinutesStr],
                                                         [30*24*3600,  7*24*3600,  24*3600,   3600,       60])),
                          {ok, #report_in{deal_name=Name,
                                          from_timestamp=FromTimestamp,
                                          to_timestamp=ToTimestamp,
                                          by_timestamp=By}};
                      _ -> undefined
                  end;
              _ -> undefined
          end),
    case Result of
        {ok, _ReportIn} -> Result;
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

report_to_html(#report_in{from_timestamp=FromTimestamp, by_timestamp=ByTimestamp}, Entries) ->
    {Rows, _ } = lists:foldl(fun (RowData, {RowsAccumulator, CurrentTimestamp}) ->
                                     {OpenDateStr, OpenTimeStr} = time_helper:timestamp_to_date_and_time(CurrentTimestamp),
                                     OpenDateTimeStr = OpenDateStr ++ " " ++ OpenTimeStr,
                                     {
                                       case RowData of
                                           #report_out{open_cost=OpenCost, close_cost=CloseCost, min_cost=MinCost, max_cost=MaxCost, quantity=Quantity} ->
                                               [{tr, [],
                                                 [{td, [], OpenDateTimeStr},
                                                  {td, [], ?f("~p",[OpenCost])},
                                                  {td, [], ?f("~p",[CloseCost])},
                                                  {td, [], ?f("~p",[MinCost])},
                                                  {td, [], ?f("~p",[MaxCost])},
                                                  {td, [], integer_to_list(Quantity)}]
                                                }| RowsAccumulator ];
                                             _ ->
                                               %% [{tr, [],
                                               %%   [{td, [], OpenDateTimeStr},
                                               %%    {td, [], "-"},
                                               %%    {td, [], "-"},
                                               %%    {td, [], "-"},
                                               %%    {td, [], "-"},
                                               %%    {td, [], "-"}]
                                               %%  } |
                                               RowsAccumulator
                                               %% ]
                                         end , CurrentTimestamp + ByTimestamp}
                             end, {[], FromTimestamp}, Entries),
    Rows.
