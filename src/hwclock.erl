-module(hwclock).

-export([open/2, open/3, select/2, close/1, open_notify/3, open_notify/4, t/0]).
-export([event_duration_s/2, event_duration_s/3, get_tick_s/2, get_tick_s/3]).

-moduledoc """
A real-time OTP NIF relying on libasound.
""".

-ignore_xref([event_duration_s/2,
              event_duration_s/3,
              open_notify/3,
              get_tick_s/2,
              get_tick_s/3,
              t/0]).


-nifs([{open, 3}, {select, 2}, {close, 1}]).

-on_load on_load/0.

-type hwclock() :: reference().

-define(PPQ_DEFAULT, 128).
-define(DURATION_COEFF_PPQ, 60).
% DURATION_COEFF = 60/128 = 0.46875
-define(DURATION_COEFF, 0.46875).

-doc false.
% Basic usage
% receive an event every second.
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

-doc """
Starts an hwclock process with BPM and NTICKS.

Equivalent to

```erlang
open(BPM, NTICKS, _PPQ = 128).
```
See `open/3`.

See also `m:hwclock_bench` and `m:hwclock_bench2` for examples of use.
""".    
-spec open(BPM :: integer(), NTICKS :: integer()) -> Handle::hwclock().
open(BPM, NTICKS) ->
    open(BPM, NTICKS, ?PPQ_DEFAULT).

-doc """
Starts an hwclock with BPM and NTICKS, where :

- BPM is an integer between 1 and 255 defining the tempo
- NTICKS is an integer between 1 and 255 defining the number of ticks in a tempo
- PPQ is an integer between 1 and 1024 increasing further the clock rate.

BPM, NTICKS and PPQ should be considered as constants.

Returns the `Handle` of the `hwclock` resource.
""".
-spec open(BPM :: integer(), NTICKS :: integer(), PPQ :: integer()) -> Handle::hwclock().
open(_BPM, _NTICKS, _PPQ) ->
    erlang:nif_error(undef).

-doc """
This function is used to receive notifications from the hwclock hardware.

The VM sends then asynchronously, an event to Pid of the form :

<!-- tabs-open -->

### Erlang

```erlang
{select, Hdl, Tick, ready_input}
```

### Elixir

```elixir
{:select, hdl, tick, :ready_input}
```

<!-- tabs-close -->

where Tick is a multiple of NTICKS, Hdl is the `hwclock` `Hdl`.

This function is not blocking and must be followed by a receive/1 to get the event.
""".
-spec select(Hdl :: hwclock(), Pid :: pid()) -> ok | {error, integer()}.
select(_Hdl, _Pid) ->
    erlang:nif_error(undef).

-doc """
Close the `hwclock` resource by its `Handle`.

See `open/2`,  `open/3`.

""".
-spec close(Handle :: hwclock()) -> ok.
close(_Handle) ->
    erlang:nif_error(undef).

-doc """
Equivalent to

```erlang
event_duration_s(BPM, NTICKS, _PPQ = 128).
```

See `event_duration_s/3`.
""".
-spec event_duration_s(BPM :: byte(), NTICKS :: byte()) -> Duration_s :: float().
event_duration_s(BPM, NTICKS) when NTICKS > 0, NTICKS < 256, BPM > 0, BPM < 256 ->
    % formula: 60 / BPM * NTICKS / 128.
    ?DURATION_COEFF / BPM * NTICKS.
-doc """
Returns in seconds, the elapsed_time between two consecutive events,

according to the formula :

```erlang
Duration_s =  60 / BPM * NTICKS / 128 * 128 / PPQ.
```
""".

-spec event_duration_s(BPM :: byte(), NTICKS :: byte(), PPQ :: integer()) ->
                          Duration_s :: float().
event_duration_s(BPM, NTICKS, PPQ)
    when NTICKS > 0, NTICKS < 256, BPM > 0, BPM < 256, PPQ > 0, PPQ =< 1024 ->
    % formula: 60 / BPM * NTICKS / 128 * 128 / PPQ.
    ?DURATION_COEFF_PPQ / BPM * NTICKS / PPQ.

-doc """
Equivalent to

```erlang
get_tick_s(Tick, BPM, _PPQ = 128).
```
See `get_tick_s/3`.

""".
-spec get_tick_s(Tick :: non_neg_integer(), BPM :: non_neg_integer()) ->
                    Timestamp :: float().
get_tick_s(Tick, BPM) ->
    ?DURATION_COEFF * Tick / BPM.

-doc """
Returns the timestamp in seconds of `Tick`,

according to the formula :

```erlang
Timestamp =  (Tick / NTICKS) * 60 / BPM * NTICKS / 128 * 128 / PPQ.
```
""".
-spec get_tick_s(Tick :: non_neg_integer(),
                 BPM :: non_neg_integer(),
                 PPQ :: non_neg_integer()) ->
                    Timestamp :: float().
get_tick_s(Tick, BPM, PPQ) ->
    ?DURATION_COEFF_PPQ * Tick / BPM / PPQ.

-doc """
Equivalent to

```erlang
open_notify(BPM, NTICKS, _PPQ_DEFAULT = 128, Pid_client).
```
See `open_notify/4`.
""".
-spec open_notify(BPM :: integer(), NTICKS :: integer(), Pid_client :: pid()) -> Pid :: pid().
open_notify(BPM, NTICKS, Pid_client) when is_pid(Pid_client) ->
    open_notify(BPM, NTICKS, ?PPQ_DEFAULT, Pid_client).

-doc """
Start an hwclock spawned process with BPM, NTICKS and Pid_client, where :

- BPM is an integer between 1 and 255 defining the tempo
- NTICKS is an integer between 1 and 255 defining the number of ticks in a tempo
- PPQ is an integer between 1 and 1024 increasing further the clock rate
- Pid_client the pid to send the event.

This process does the select/2 , receive/1 and sends to Pid_client an event of the form :

<!-- tabs-open -->

### Erlang

```erlang
 {hwclock, Tick}
```

### Elixir

```elixir
 {:hwclock, tick}
```

<!-- tabs-close -->

where Tick is a multiple of NTICKS as given in open_notify/3, open_notify/4.

Returns the `Pid` of the spawned process.
""".
-spec open_notify(BPM :: integer(),
                  NTICKS :: integer(),
                  PPQ :: integer(),
                  Pid_client :: pid()) ->
                     Pid :: pid().
open_notify(BPM, NTICKS, PPQ, Pid_client)
    when NTICKS > 0, NTICKS < 256, BPM > 0, BPM < 256, PPQ > 0, PPQ =< 1024, is_pid(Pid_client) ->
    Hdl = hwclock:open(BPM, NTICKS, PPQ),
    spawn(fun() -> open_notify_do_select(Hdl, Pid_client) end).

open_notify_do_select(Hdl, Pid) ->
    ok = hwclock:select(Hdl, self()),
    receive
        {select, Hdl, Tick, ready_input} ->
            % io:format("rcv spawn ~p\n", [Tick]),
            Pid ! {hwclock, Tick}
    end,
    open_notify_do_select(Hdl, Pid).