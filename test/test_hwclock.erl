-module(test_hwclock).

-include_lib("eunit/include/eunit.hrl").

-define(BPM, 255).
-define(NTICKS, 4).
-define(HWCLOCK_interval_s, 60 / ?BPM * ?NTICKS / 128).
-define(PPQ, 1024).
-define(HWCLOCK_interval_with_ppq_s, 60 / ?BPM * ?NTICKS / 128 * 128 / ?PPQ).

read_Tick(Hdl) ->
    ok = hwclock:select(Hdl, self()),
    receive
        {select, Hdl, T, ready_input} ->
            T
    end.

open_test() ->
    H1 = hwclock:open(?BPM, ?NTICKS),
    ?assert(is_reference(H1)),
    H2 = hwclock:open(?BPM, ?NTICKS, ?PPQ),
    ?assert(is_reference(H2)).

select_test() ->
    Iterators = lists:seq(0, 2),
    H1 = hwclock:open(?BPM, ?NTICKS),
    Ticks_result = [I * ?NTICKS || I <- Iterators],
    Res = [read_Tick(H1) || _ <- Iterators],
    ?assert(Res =:= Ticks_result).

spawned_select_test() ->
    Iterators = lists:seq(0, 2),
    {H1, H2} = {hwclock:open(?BPM, ?NTICKS), hwclock:open(?BPM, ?NTICKS)},
    Ticks_result = [I * ?NTICKS || I <- Iterators],
    Parent = self(),

    spawn(fun() ->
             Res = [read_Tick(H1) || _ <- Iterators],
             ?assert(Res =:= Ticks_result),
             Parent ! done
          end),
    spawn(fun() ->
             Res = [read_Tick(H2) || _ <- Iterators],
             ?assert(Res =:= Ticks_result),
             Parent ! done
          end),

    receive
        _ ->
            receive
                _ ->
                    ok
            end
    end,
    {ok, ok} = {hwclock:close(H1), hwclock:close(H2)}.

close_test() ->
    _ = hwclock:open(?BPM, ?NTICKS),
    H2 = hwclock:open(?BPM, ?NTICKS, ?PPQ),
    % Res1 = hwclock:close(H1),
    ?assert(H2 =/= nil),
    garbage_collect(),
    Res2 = hwclock:close(H2),
    ?assert(Res2 =:= ok).

close_span_test() ->
    Parent = self(),
    spawn(fun() ->
             _ = hwclock:open(?BPM, ?NTICKS),
             H2 = hwclock:open(?BPM, ?NTICKS, ?PPQ),
             Res2 = hwclock:close(H2),
             ?assert(Res2 =:= ok),
             Parent ! done
          end),
    % FIXME H1 should be gc ??
    garbage_collect(),
    receive
        _ ->
            ok
    end,
    timer:sleep(500).

open_close_close_test() ->
    Hdl = hwclock:open(?BPM, ?NTICKS),
    read_Tick(Hdl),
    Res = hwclock:close(Hdl),
    ?assert(Res =:= ok),

    Res1 = hwclock:close(Hdl),
    ?assert(Res1 =:= ok),

    Hdl2 = hwclock:open(?BPM, ?NTICKS),
    read_Tick(Hdl2),
    Res2 = hwclock:close(Hdl2),
    ?assert(Res2 =:= ok).

open_select_close_test() ->
    Hdl = hwclock:open(?BPM, ?NTICKS),
    ok = hwclock:select(Hdl, self()),
    _ = read_Tick(Hdl),
    Result = hwclock:close(Hdl),
    ?assert(Result =:= ok),

    {error, Result1} = hwclock:select(Hdl, self()),
    ?assert(Result1 =:= -1),

    Hdl2 = hwclock:open(?BPM, ?NTICKS),
    _ = read_Tick(Hdl2),
    Result2 = hwclock:close(Hdl2),
    ?assert(Result2 =:= ok).

open_notify_test() ->
    Pid = hwclock:open_notify(?BPM, ?NTICKS, ?PPQ, self()),
    ?assert(is_pid(Pid) =:= true),
    Result =
        [receive
             {hwclock, Ev} ->
                 Ev
         end
         || _ <- lists:seq(0, 2)],
    ?assert(Result =:= [0, ?NTICKS, ?NTICKS * 2]),
    Result2 = exit(Pid, kill),
    ?assert(Result2 =:= true).

event_duration_s_test() ->
    Duration_s = hwclock:event_duration_s(?BPM, ?NTICKS),
    ?assert(Duration_s =:= 60 / ?BPM * ?NTICKS / 128).

event_duration_s_with_ppq_test() ->
    Duration_s = hwclock:event_duration_s(?BPM, ?NTICKS, ?PPQ),
    ?assert(Duration_s =:= 60 / ?BPM * ?NTICKS / 128 * 128 / ?PPQ).

get_ticks_test() ->
    Hdl = hwclock:open(?BPM, ?NTICKS),
    Result = [read_Tick(Hdl) || _ <- lists:seq(0, 2)],
    ?assert(Result =:= [0, ?NTICKS, 2 * ?NTICKS]).

get_tick_s_test() ->
    Hdl = hwclock:open(?BPM, ?NTICKS),
    _ = read_Tick(Hdl),
    Tick = read_Tick(Hdl),
    Timestamp_s = hwclock:get_tick_s(Tick, ?BPM),
    ?assert(Timestamp_s =:= ?HWCLOCK_interval_s).

get_tick_s_with_ppq_test() ->
    Hdl = hwclock:open(?BPM, ?NTICKS, ?PPQ),
    _ = read_Tick(Hdl),
    Tick = read_Tick(Hdl),
    Res = hwclock:get_tick_s(Tick, ?BPM, ?PPQ),
    ?assert(Res =:= ?HWCLOCK_interval_with_ppq_s).

open_bad_arg1_test() ->
    Result =
        try
            hwclock:open(256, ?NTICKS)
        catch
            _Failure:Error ->
                Error
        end,
    ?assert(Result =:= badarg).

open_bad_arg2_test() ->
    Result =
        try
            hwclock:open(?BPM, 256)
        catch
            _Failure:Error ->
                Error
        end,
    ?assert(Result =:= badarg).

open_bad_arg3_test() ->
    Result =
        try
            hwclock:open(?BPM, ?NTICKS, 1025)
        catch
            _Failure:Error ->
                Error
        end,
    ?assert(Result =:= badarg).

open_notify_bad_arg1_test() ->
    Result =
        try
            hwclock:open_notify(256, ?NTICKS, self())
        catch
            error:Error ->
                Error
        end,
    ?assert(Result =:= function_clause).

open_notify_bad_arg2_test() ->
    Result =
        try
            hwclock:open_notify(?BPM, 256, self())
        catch
            error:Error ->
                Error
        end,
    ?assert(Result =:= function_clause).

open_notify_bad_arg3_test() ->
    Result =
        try
            hwclock:open_notify(?BPM, ?NTICKS, 1025, self())
        catch
            error:Error ->
                Error
        end,
    ?assert(Result =:= function_clause).
