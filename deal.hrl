-record(deal, {name, timestamp, cost, quantity}).
-define(f(Template, Args),
        (.lists:flatten(io_lib:format(Template,Args)))).
