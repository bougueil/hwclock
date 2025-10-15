-module(hwclock_bench2).

-ignore_xref([start/0]).

-export([start/0]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start() ->
    gen_server:start_link({local, hwclock_bench2}, hwclock_bench2, [], []).

%%--------------------------------------------------------
%% Example of using hwclock:open with a gen_server.
%%
%% erl -noshell -pa _build/default/lib/hwclock/ebin -s hwclock_bench2 | tee output_eee.dat
-define(REFRESH_SCREEN_ms, timer:seconds(10)).
-define(GAME_OVER_ms, 61 * ?REFRESH_SCREEN_ms).
%% ms.
-define(E_TIMER_ms, 10).

-record(state,
        {e_clock_cumul :: integer(),
         hw_clock_cumul :: integer(),
         handle :: reference(),
         bpm :: byte(),
         iter :: integer(),
         start_us :: term()}).

init(_Args) ->
    %% receive an hwclock event every 1/10 s., same E_TIMER_ms.
    % formula: 60 / BPM * NTICKS / 128 * 128 / PPQ = 60/BPM*NTICKS/PPQ.
    %% 10 ms.
    BPM = 60,
    NTICKS = 1,
    PPQ = 100,

    HWCLOCK_interval_ms = 1000 * hwclock:event_duration_s(BPM, NTICKS, PPQ),

    io:format("~nhwclock:open(_BPM=~p, _NTICKS=~p)~nevents scheduled every: [erlang:~p ms, hwclock:~p ms]~nstats are reported every ~p ms.~n",
              [BPM, NTICKS, ?E_TIMER_ms, HWCLOCK_interval_ms, ?REFRESH_SCREEN_ms]),
    Now = os:timestamp(),
    erlang:start_timer(?E_TIMER_ms, self(), erlang_clock),
    erlang:start_timer(?REFRESH_SCREEN_ms, self(), stat_collect),
    erlang:start_timer(?GAME_OVER_ms, self(), game_over),
    Hdl = hwclock:open(BPM, NTICKS, PPQ),
    ok = hwclock:select(Hdl, self()),

    {ok,
     #state{start_us = Now,
            handle = Hdl,
            bpm = BPM,
            iter = 0,
            e_clock_cumul = 0,
            hw_clock_cumul = 0}}.

handle_info({select, Hdl, _Tick, ready_input}, S) ->
    _ = hwclock:select(Hdl, self()),
    {noreply, S#state{hw_clock_cumul = S#state.hw_clock_cumul + 1}};
handle_info({timeout, _, erlang_clock}, S) ->
    erlang:start_timer(?E_TIMER_ms, self(), erlang_clock),
    % io:format("\t\tER CUM: ~p\n", [S#state.e_clock_cumul]),
    {noreply, S#state{e_clock_cumul = S#state.e_clock_cumul + 1}};
handle_info({timeout, _, stat_collect}, S) ->
    erlang:start_timer(?REFRESH_SCREEN_ms, self(), stat_collect),
    Elapsed_us =
        timer:now_diff(
            os:timestamp(), S#state.start_us),
    io:format("\r# ~p t: ~p ms. acc. events, erlang: ~p, hwclock: ~p~n",
              [S#state.iter + 1,
               Elapsed_us div 100 / 10,
               S#state.e_clock_cumul,
               S#state.hw_clock_cumul]),
    {noreply, S#state{iter = S#state.iter + 1}};
handle_info({timeout, _, gameover}, S) ->
    hwclock:close(S#state.handle),
    {noreply, S};
handle_info(Tellme, S) ->
    io:format("abort on event ~w  ~n", [Tellme]),
    hwclock:close(S#state.handle),
    {noreply, S}.

handle_call(Smth, _From, S) ->
    {reply, Smth, S}.
handle_cast(_Smth, S) ->
    {noreply, S}.
