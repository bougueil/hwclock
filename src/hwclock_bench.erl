-module(hwclock_bench).

-ignore_xref([start/0]).
-export([ start/0
	]).

%%--------------------------------------------------------
%% erl -noshell -pa _build/default/lib/hwclock/ebin -s hwclock_bench | tee output_eee.dat
-define(REFRESH_SCREEN, timer:seconds(10)).
-define(GAME_OVER, 60*?REFRESH_SCREEN).
-define(E_TIMER, 100). %% ms.

-record(state, {e_clock_cumul       ::integer(), 
		hw_clock_cumul      ::integer(),
		timer         ::port(), 
		bpm           ::byte(),
		start_us         ::term(),
		hwclock_every_ms :: integer(),
		iter      ::integer()
	       }).


-spec start() -> true.
start()  ->
    BPM = 60,   %% between 1..255, 60 means 60 beats per minute
    NTicks= 128, %% between 1..255, nticks = 128 with bpm = 60 ensures we receive an hwclock event every 1 second
    HWCLOCK_interval_ms = 1000 * 60 * NTicks div BPM div 128,

    io:format("~nhwclock:open(_BPM=~p, _NTicks=~p)~nevents scheduled every - erlang:~p ms, hwclock:~p ms~nstats are reported every ~p ms.~n",
	      [BPM, NTicks, ?E_TIMER, HWCLOCK_interval_ms, ?REFRESH_SCREEN]),
    Port = hwclock:open(BPM, NTicks),
    Now = os:timestamp(),
    erlang:start_timer(?E_TIMER, self(), erlang_timer),
    erlang:start_timer(?REFRESH_SCREEN, self(), stat_collect),
    erlang:start_timer(?GAME_OVER, self(), game_over),
    loop_bench(#state{start_us=Now, timer=Port, bpm=BPM, e_clock_cumul=0, hw_clock_cumul=0, hwclock_every_ms= HWCLOCK_interval_ms}).

loop_bench(S)  ->
    receive
	{_Port, {data, _Opaque}} -> 
	    %% Tick = hwclock:get_tick(Opaque),
	    %% ElapsedMs = hwclock:get_tick_ms(S#state.bpm, Opaque),
	    %% io:format("hw new data, tick number: ~p, ticks_ms: ~p ms.~n", [Tick, ElapsedMs]),
	    Cumul = S#state.hw_clock_cumul,
	    loop_bench(S#state{hw_clock_cumul=Cumul+1});

	{timeout, _, erlang_timer} ->
	    erlang:start_timer(?E_TIMER, self(), erlang_timer),
	    Cumul = S#state.e_clock_cumul,
	    loop_bench(S#state{e_clock_cumul=Cumul+1});

	{timeout, _, stat_collect} ->
	    erlang:start_timer(?REFRESH_SCREEN, self(), stat_collect),
	    Elapsed_us =  timer:now_diff(os:timestamp(),S#state.start_us),
	    io:format("stats after ~p us.\taccumulated events erlang:~p, hwclock:~p\tdrift us. erl versus hwclock: ~p~n",[Elapsed_us,
														    S#state.e_clock_cumul, S#state.hw_clock_cumul,
														    S#state.e_clock_cumul*?E_TIMER - S#state.hw_clock_cumul*S#state.hwclock_every_ms]),
	    loop_bench(S);

	{timeout, _, game_over} ->
	    io:format("elapsed ! ~n"),
	    hwclock:stop(S#state.timer);

	Tellme ->
	    io:format("abort on event ~w  ~n", [Tellme]),
	    hwclock:stop(S#state.timer)
    end.
