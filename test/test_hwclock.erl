-module(test_hwclock).

-include_lib("eunit/include/eunit.hrl").

-define(BPM, 60).
-define(NTICKS, 4).
-define(HWCLOCK_interval_s, 60 / ?BPM * ?NTICKS / 128).
-define(PPQ, 512).
-define(HWCLOCK_interval_with_ppq_s, 60 / ?BPM * ?NTICKS / 128 * 128 / ?PPQ).

open_test() ->
    Port = hwclock:open(?BPM, ?NTICKS),

    ?assert(is_port(Port)).

open_with_ppq_test() ->
    Port = hwclock:open(?BPM, ?NTICKS, ?PPQ),

    ?assert(is_port(Port)).

event_duration_s_test() ->
    Duration_s = hwclock:event_duration_s(?BPM, ?NTICKS),

    ?assert(Duration_s =:= 60 / ?BPM * ?NTICKS / 128).

event_duration_s_with_ppq_test() ->
    Duration_s = hwclock:event_duration_s(?BPM, ?NTICKS, ?PPQ),

    ?assert(Duration_s =:= 60 / ?BPM * ?NTICKS / 128 * 128 / ?PPQ).

get_tick_test() ->
    Port = hwclock:open(?BPM, ?NTICKS),
    receive
        {Port, {data, Opaque}} ->
            Tick = hwclock:get_tick(Opaque),

            ?assert(Tick =:= ?NTICKS)
    end.

get_tick_s_test() ->
    Port = hwclock:open(?BPM, ?NTICKS),
    receive
        {Port, {data, Opaque}} ->
            Res = hwclock:get_tick_s(Opaque, ?BPM),

            ?assert(Res =:= {?NTICKS, ?HWCLOCK_interval_s})
    end.

get_tick_s_with_ppq_test() ->
    Port = hwclock:open(?BPM, ?NTICKS, ?PPQ),
    receive
        {Port, {data, Opaque}} ->
            Res = hwclock:get_tick_s(Opaque, ?BPM, ?PPQ),

            ?assert(Res =:= {?NTICKS, ?HWCLOCK_interval_with_ppq_s})
    end.

stop_test() ->
    Port = hwclock:open(?BPM, ?NTICKS),
    Result = hwclock:stop(Port),

    ?assert(Result =:= true).

stop_with_ppq_test() ->
    Port = hwclock:open(?BPM, ?NTICKS, ?PPQ),
    Result = hwclock:stop(Port),

    ?assert(Result =:= true).
