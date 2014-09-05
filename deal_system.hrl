-record(deal, {name, timestamp, cost, quantity}).
-define(deal_spec(Name, Timestamp, Cost, Quantity),
        is_atom(Name),
        is_integer(Timestamp),
        Timestamp >=0,
        is_float(Cost),
        is_integer(Quantity),
        Quantity > 0
       ).

-record(report_in,  {deal_name, from_timestamp, to_timestamp, by_timestamp}).
-define(report_in_spec(DealName, FromTimestamp, ToTimestamp, ByTimestamp),
        is_atom(DealName),
        is_integer(FromTimestamp),
        FromTimestamp >= 0,
        is_integer(ToTimestamp),
        ToTimestamp > FromTimestamp,
        is_integer(ByTimestamp),
        ByTimestamp > 0
       ).

-record(report_out, {open_timestamp, close_timestamp, open_cost, close_cost, min_cost, max_cost, quantity}).
-define(report_out_spec(OpenTimestamp, CloseTimestamp, OpenCost, CloseCost, MinCost, MaxCost, Quantity),
        is_integer(OpenTimestamp),
        OpenTimestamp >= 0,
        is_integer(CloseTimestamp),
        CloseTimestamp >= 0,
        CloseTimestamp >= OpenTimestamp,
        is_integer(OpenCost),
        is_integer(CloseCost),
        is_integer(MinCost),
        is_integer(MaxCost),
        MaxCost >= MinCost,
        is_integer(Quantity)
       ).

-undef(f).
-define(f(Template, Args),
        (lists:flatten(io_lib:format(Template,Args)))).

-undef(If).
-define(If(Statement, PosResult, NegResult),
        if
            (Statement) -> (PosResult);
            true -> (NegResult)
        end).
