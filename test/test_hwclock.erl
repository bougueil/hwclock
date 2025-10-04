-module(test_hwclock).
-include_lib("eunit/include/eunit.hrl").

-define(BPM, 60).
-define(NTICKS, 4).
-define(HWCLOCK_interval_s, 60 / ?BPM * ?NTICKS / 128).
-define(PPQ, 128).

open2_test() ->
    Port = hwclock:open(?BPM, ?NTICKS),

    ?assert(is_port(Port)).

open3_test() ->
    Port = hwclock:open(?BPM, ?NTICKS, ?PPQ),

    ?assert(is_port(Port)).

get_tick_test() ->
    Port = hwclock:open(?BPM, ?NTICKS, ?PPQ),
    receive
        {Port, {data, Opaque}} ->
            Tick = hwclock:get_tick(Opaque),

            ?assert(Tick =:= ?NTICKS)
    end.

get_tick_s_test() ->
    Port = hwclock:open(?BPM, ?NTICKS, ?PPQ),
    receive
        {Port, {data, Opaque}} ->
            Res = hwclock:get_tick_s(Opaque, ?BPM, ?PPQ),

            ?assert(Res =:= {?NTICKS, ?HWCLOCK_interval_s})
    end.

stop_test() ->
    Port = hwclock:open(?BPM, ?NTICKS, ?PPQ),
    Result = hwclock:stop(Port),

    ?assert(Result =:= true).
