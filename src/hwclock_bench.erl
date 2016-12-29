-module(hwclock_bench).



-ignore_xref([start/0]).
-export([ start/0
	]).

%%--------------------------------------------------------
%% erl -noshell -pa _build/default/lib/hwclock/ebin -s hwclock_bench | tee output_eee.dat
-define(REFRESH_SCREEN, 100*?E_TIMER).
-define(E_TIMER, 1000).

-record(state, {e_clock_cumul       ::integer(), 
		hw_clock_cumul      ::integer(),
		timer         ::port(), 
		bpm           ::integer(),
		offset      ::integer()
	       }).


-spec start() -> true.
start()  ->
    BPM = 60, NTicks= 128,   %% (BPM/60)*(NTicks/128) => 1000 ms 
    Tick_interval_ms = 1000 * BPM * NTicks div 60 div 128,
    io:format("~nhwclock:open(_BPM=~p, _NTicks=~p)  => tick interval = ~p ms.~n~n",
	      [BPM, NTicks, Tick_interval_ms]),
    Port = hwclock:open(BPM, NTicks),
    Now = now_s(erlang:timestamp()),
    erlang:start_timer(?E_TIMER, self(), erlang_timer),
    erlang:start_timer(?REFRESH_SCREEN, self(), stat_collect),
    erlang:start_timer(60*?REFRESH_SCREEN, self(), game_over),
    loop_bench(#state{offset=Now, timer=Port, bpm=BPM, e_clock_cumul=0, hw_clock_cumul=0}).


loop_bench(S)  ->
    receive
	{_Port, {data, _Opaque}} -> 
	    Tick = hwclock:get_tick(_Opaque),
	    ElapsedMs = hwclock:get_tick_ms(S#state.bpm, _Opaque),
	    io:format("Elapsed, ticks: ~p, ticks_ms: ~p ms.~n", [Tick, ElapsedMs]),
	    Cumul = S#state.hw_clock_cumul,
	    loop_bench(S#state{hw_clock_cumul=Cumul+1});

	{timeout, _, erlang_timer} ->
	    erlang:start_timer(?E_TIMER, self(), erlang_timer),
	    Cumul = S#state.e_clock_cumul,
	    loop_bench(S#state{e_clock_cumul=Cumul+1});

	{timeout, _, stat_collect} ->
	    erlang:start_timer(?REFRESH_SCREEN, self(), stat_collect),
	    Elapsed = now_s(erlang:timestamp()) - S#state.offset,
	    io:format("~p\t~p\t~p~n",[Elapsed, S#state.e_clock_cumul- Elapsed, S#state.hw_clock_cumul-Elapsed]),
	    loop_bench(S);

	{timeout, _, game_over} ->
	    io:format("elapsed ! ~n"),
	    hwclock:stop(S#state.timer);

	closed ->
	    io:format("The WebSocket was CLOSED!~n"),
	    hwclock:stop(S#state.timer);

	_Ignore ->
	    io:format("_Ignore ~w  ~n", [_Ignore]),
	    loop_bench(S)
    end.

now_s({_MegaSecs,Secs,_MicroSecs}) ->
    Secs.

