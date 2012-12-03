-record(deal, {name, timestamp, cost, quantity}).
-define(deal_spec(Name, Timestamp, Cost, Quantity), is_atom(Name), is_integer(Timestamp), is_float(Cost), is_integer(Quantity), Timestamp >=0).

-record(report_in,  {name, from_timestamp, to_timestamp, by_timestamp}).
-define(report_in_spec(Name, FromTimestamp, ToTimestamp, ByTimestamp),
        is_atom(Name),
        is_integer(FromTimestamp),
        is_integer(ToTimestamp),
        is_integer(ByTimestamp),
        FromTimestamp >= 0,
        ToTimestamp > FromTimestamp,
        ByTimestamp > 0).

-record(report_out, {name, open_cost, close_cost, min_cost, max_cost, total_quantity}).
-define(report_out_spec(Name, OpenCost, CloseCost, MinCost, MaxCost),
        is_atom(Name),
        is_integer(OpenCost),
        is_integer(CloseCost),
        is_integer(MinCost),
        is_integer(MaxCost),
        MaxCost >= MinCost).

-undef(f).
-define(f(Template, Args),
        (.lists:flatten(io_lib:format(Template,Args)))).
