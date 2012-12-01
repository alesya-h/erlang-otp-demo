-module(time_helper).
-export([now_to_timestamp/1, date_and_time_to_timestamp/2, timestamp_to_date_and_time/1]).
-include("deal_system.hrl").

now_to_timestamp({Mega, S, _Micro}) ->
    1000000 * Mega + S.

date_and_time_to_timestamp(DateStr, TimeStr) ->
    [YYYY, MM, DD] = string:tokens(DateStr, "-"),
    [Hh,Mm|Tail] = string:tokens(TimeStr, ":"),
    Ss = case Tail of
        [] ->
            "00";
        [First|_] ->
            First
    end,
    [Year,Month,Day,Hour,Minute,Second] = lists:map(fun list_to_integer/1, [YYYY,MM,DD,Hh,Mm,Ss]),
    DateTime = {{Year,Month,Day},{Hour, Minute, Second}},
    calendar:datetime_to_gregorian_seconds(DateTime) - calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time({0,0,0})).

timestamp_to_date_and_time(Timestamp) ->
    Now = {Timestamp div 1000000, Timestamp rem 1000000, 0},
    {{Year, Month, Day},{Hour, Minute, Second}} = calendar:now_to_universal_time(Now),
    {?f("~w-~s~w-~s~w",[Year,
                        prepend_with_zero(Month), Month,
                        prepend_with_zero(Day), Day]),
     ?f("~s~w:~s~w:~s~w",[prepend_with_zero(Hour), Hour,
                         prepend_with_zero(Minute), Minute,
                         prepend_with_zero(Second), Second ])}.

prepend_with_zero(Num) ->
    if
        Num >= 10 ->
            "" ;
        true -> "0"
    end.
