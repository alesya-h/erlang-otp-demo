-record(deal, {name, timestamp, cost, quantity}).
-undef(f).
-define(f(Template, Args),
        (.lists:flatten(io_lib:format(Template,Args)))).
