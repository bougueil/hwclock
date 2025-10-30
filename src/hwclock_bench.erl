-module(hwclock_bench).

-export([start/0]).

-moduledoc """
Example of use without gen_server.

```bash
rebar3 shell --eval "hwclock_bench:start()."
```
""".

-ignore_xref([start/0]).

-define(REFRESH_SCREEN_ms, timer:seconds(10)).
-define(GAME_OVER_ms, 61 * ?REFRESH_SCREEN_ms).
-define(E_TIMER_ms, 10).

-record(state, {
    e_clock_cumul :: integer(),
    hw_clock_cumul :: integer(),
    pid :: pid(),
    bpm :: byte(),
    iter :: integer(),
    start_us :: term()
}).

-spec start() -> no_return().
start() ->
    %% receive an hwclock event every 1/10 s., same E_TIMER_ms.
    % formula: 60 / BPM * NTICKS / 128 * 128 / PPQ = 60/BPM*NTICKS/PPQ.
    BPM = 60,
    NTICKS = 1,
    PPQ = 100,
    Channel = hwclock_bench,
    HWCLOCK_interval_ms = 1000 * hwclock:event_duration_s(BPM, NTICKS, PPQ),

    io:format(
        "~nhwclock:open_channel(_BPM=~p, _NTICKS=~p, _CHANNEL=~p)~nevents scheduled every: [erlang:~p ms, hwclock:~p ms]~nstats are reported every ~p ms.~n",
        [BPM, NTICKS, Channel, ?E_TIMER_ms, HWCLOCK_interval_ms, ?REFRESH_SCREEN_ms]
    ),
    Now = os:timestamp(),
    erlang:start_timer(?E_TIMER_ms, self(), erlang_clock),
    erlang:start_timer(?REFRESH_SCREEN_ms, self(), stat_collect),
    erlang:start_timer(?GAME_OVER_ms, self(), game_over),

    hwclock:subscribe_pubsub(Channel),
    Pid = hwclock:open_with_pubsub(BPM, NTICKS, PPQ, Channel),
    loop_bench(#state{
        start_us = Now,
        pid = Pid,
        bpm = BPM,
        iter = 0,
        e_clock_cumul = 0,
        hw_clock_cumul = 0
    }).

loop_bench(S) ->
    receive
        {hwclock, _Tick} ->
            loop_bench(S#state{hw_clock_cumul = S#state.hw_clock_cumul + 1});
        {timeout, _, erlang_clock} ->
            erlang:start_timer(?E_TIMER_ms, self(), erlang_clock),
            loop_bench(S#state{e_clock_cumul = S#state.e_clock_cumul + 1});
        {timeout, _, stat_collect} ->
            erlang:start_timer(?REFRESH_SCREEN_ms, self(), stat_collect),
            Elapsed_us =
                timer:now_diff(
                    os:timestamp(), S#state.start_us
                ),
            io:format(
                "# ~p t: ~p ms. acc. events, erlang: ~p, hwclock: ~p~n",
                [
                    S#state.iter + 1,
                    Elapsed_us div 100 / 10,
                    S#state.e_clock_cumul,
                    S#state.hw_clock_cumul
                ]
            ),
            loop_bench(S#state{iter = S#state.iter + 1});
        {timeout, _, gameover} ->
            exit(S#state.pid, kill);
        Tellme ->
            io:format("abort on event ~w  ~n", [Tellme]),
            exit(S#state.pid, kill)
    end.
