-module(hwclock).

-ignore_xref([event_duration_s/2,
              event_duration_s/3,
              open_notify/3,
              get_tick_s/2,
              get_tick_s/3,
              t/0]).

-export([open/2, open/3, select/2, close/1, open_notify/3, open_notify/4, t/0]).
-export([event_duration_s/2, event_duration_s/3, get_tick_s/2, get_tick_s/3]).

-nifs([{open, 3}, {select, 2}, {close, 1}]).

-on_load on_load/0.

-type hwclock() :: reference().

-define(PPQ_DEFAULT, 128).
-define(DURATION_COEFF_PPQ, 60).
% DURATION_COEFF = 60/128 = 0.46875
-define(DURATION_COEFF, 0.46875).

% Basic usage
t() ->
    H = hwclock:open(60, 128),
    ok = hwclock:select(H, self()),

    [receive
         V ->
             io:format("\rrcv from select ~p ~n", [V]),
             _ = hwclock:select(H, self())
     end
     || _ <- lists:seq(0, 3)],
    _ = hwclock:close(H).

on_load() ->
    ok = erlang:load_nif(code:priv_dir(hwclock) ++ "/hwclock", 0).

-doc("Start an `hwclock` process with BPM and NTICKS, where :\n - BPM is an integer between 1 and 255 defining the tempo\n - NTICKS is an integer between 1 and 255 defining the number of ticks in a tempo\n\n Equivalent to open(BPM, NTICKS, 128).").

-spec open(BPM :: integer(), NTICKS :: integer(), PPQ :: integer()) -> hwclock().
open(_BPM, _NTICKS, _PPQ) ->
    erlang:nif_error(undef).

-doc("Start an `hwclock` process with BPM, NTICKS and PPQ, where :\n - BPM is an integer between 1 and 255 defining the tempo\n - NTICKS is an integer between 1 and 255 defining the number of ticks in a tempo\n - PPQ is an integer between 1 and 1024 increasing further the clock.\n").

-spec open(BPM :: integer(), NTICKS :: integer()) -> hwclock().
open(BPM, NTICKS) ->
    open(BPM, NTICKS, ?PPQ_DEFAULT).

-doc("fetch an hwclock event from the hardware and send it to Pid.\n\nEvent is of the form : {select, Hdl, T, ready_input}\n\n where T is a multiple of NTICKS as given in open/2, open/3").

-spec select(Handle :: hwclock(), Pid :: pid()) -> ok | {error, integer()}.
select(_Handle, _Pid) ->
    erlang:nif_error(undef).

-spec close(Handle :: hwclock()) -> ok.
close(_Handle) ->
    erlang:nif_error(undef).

-doc("Returns in seconds the elapsed_time between two consecutive events\n\nEquivalent to event_duration_s(BPM, NTICKS, 128).").

-spec event_duration_s(BPM :: byte(), NTICKS :: byte()) -> Duration :: float().
event_duration_s(BPM, NTICKS) when NTICKS > 0, NTICKS < 256, BPM > 0, BPM < 256 ->
    % formula: 60 / BPM * NTICKS / 128.
    ?DURATION_COEFF / BPM * NTICKS.

-doc("Returns a float in seconds the elapsed_time between two consecutive events").

-spec event_duration_s(BPM :: byte(), NTICKS :: byte(), PPQ :: integer()) ->
                          Duration :: float().
event_duration_s(BPM, NTICKS, PPQ)
    when NTICKS > 0, NTICKS < 256, BPM > 0, BPM < 256, PPQ > 0, PPQ =< 1024 ->
    % formula: 60 / BPM * NTICKS / 128 * 128 / PPQ.
    ?DURATION_COEFF_PPQ / BPM * NTICKS / PPQ.

-doc("Same as get_tick_s(Tick, BPM, 128)").

-spec get_tick_s(Tick :: non_neg_integer(), BPM :: non_neg_integer()) ->
                    Timestamp :: float().
get_tick_s(Tick, BPM) ->
    ?DURATION_COEFF * Tick / BPM.

-doc("From the Tick value and a given BPM and PPQ,\nreturns the tick timestamp in second as a float.\n\n").

-spec get_tick_s(Tick :: non_neg_integer(),
                 BPM :: non_neg_integer(),
                 PPQ :: non_neg_integer()) ->
                    Timestamp :: float().
get_tick_s(Tick, BPM, PPQ) ->
    ?DURATION_COEFF_PPQ * Tick / BPM / PPQ.

-doc("Start an `hwclock` spawn process with BPM and NTICKS, where :\n - BPM is an integer between 1 and 255 defining the tempo\n - NTICKS is an integer between 1 and 255 defining the number of ticks in a tempo\n\nEquivalent to open_notify(BPM, NTick, 128).").

-spec open_notify(BPM :: integer(), NTICKS :: integer(), Pid :: pid()) -> pid().
open_notify(BPM, NTICKS, Pid) when is_pid(Pid) ->
    open_notify(BPM, NTICKS, ?PPQ_DEFAULT, Pid).

-spec open_notify(BPM :: integer(),
                  NTICKS :: integer(),
                  PPQ :: integer(),
                  Pid :: pid()) ->
                     pid().
-doc("Start an `hwclock` spawn process with BPM and NTICKS, where :\n - BPM is an integer between 1 and 255 defining the tempo\n - NTICKS is an integer between 1 and 255 defining the number of ticks in a tempo\n - PPQ is an integer between 1 and 1024 increasing further the clock.\n\nThis process does the select/2 and sends events of the form : {hwclock, T} to Pid\n\n where T is a multiple of NTICKS as given in open_notify/3, open_notify/4.").

open_notify(BPM, NTICKS, PPQ, Pid)
    when NTICKS > 0, NTICKS < 256, BPM > 0, BPM < 256, PPQ > 0, PPQ =< 1024, is_pid(Pid) ->
    Hdl = hwclock:open(BPM, NTICKS, PPQ),
    spawn(fun() -> open_notify_do_select(Hdl, Pid) end).

open_notify_do_select(Hdl, Pid) ->
    ok = hwclock:select(Hdl, self()),
    receive
        {select, Hdl, Tick, ready_input} ->
            % io:format("rcv spawn ~p\n", [Tick]),
            Pid ! {hwclock, Tick}
    end,
    open_notify_do_select(Hdl, Pid).
