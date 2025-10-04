%% Copyright (c) 2011-2022, Renaud Mariana <rmariana@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(hwclock).

-export([
    open/2, open/3,
    get_tick/1,
    get_tick_s/3,
    stop/1
]).

%% To help with testing internal functions
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-define(HWCLOCK_START, 3).
-define(HWCLOCK_STOP, 4).

-doc """
Start an `hwclock` device with BPM and NTicks, where :
 - BPM is an integer between 1 and 255 defining the tempo
 - Nticks is an integer between 1 and 255 defining the number of ticks in a tempo

Equivalent to open(BPM, NTick, 128).
""".
-spec open(byte(), byte()) -> port().
open(BPM, NTicks) when
    NTicks > 0,
    NTicks < 256,
    BPM > 0,
    BPM < 256
->
    open(BPM, NTicks, 128).

% open hwclock with BPM, NTicks and PPQ, where :
-doc """
Start an `hwclock` device with BPM and NTicks, where :
 - BPM is an integer between 1 and 255 defining the tempo
 - Nticks is an integer between 1 and 255 defining the number of ticks in a tempo
 - PPQ is an between 1 and 1024, see duration_ms calculation

```bash
# duration between hwclock events
duration_ms = 1000* 60/bpm * nticks/128 * 128/ppq 
```
Returns the port of the `hwclock`
## Examples

```erlang
%% receive an event every 1000 ms.
1> hwclock:open(60, 128, 128).
#Port<0.21>

%% receive an event every 500 ms.
1> hwclock:open(60, 64, 128).

%% receive an event every 500 ms.
1> hwclock:open(120, 128, 128).

%% receive an event every 250 ms.
1> hwclock:open(120, 128, 256).
```
""".
-spec open(byte(), byte(), integer()) -> port().
open(BPM, NTicks, PPQ) when
    NTicks > 0,
    NTicks < 256,
    BPM > 0,
    BPM < 256,
    PPQ > 0,
    PPQ =< 1024
->
    Fdlink = "fdlink_" ++ ?MODULE_STRING,
    Exec =
        case code:priv_dir(?MODULE) of
            {error, bad_name} ->
                case code:which(?MODULE) of
                    Filename when is_list(Filename) ->
                        filename:join([filename:dirname(Filename), "../priv", Fdlink]);
                    _ ->
                        filename:join("../priv", Fdlink)
                end;
            Dir ->
                filename:join(Dir, Fdlink)
        end,
    Port = open_port({spawn_executable, Exec}, [
        {packet, 2}, binary, exit_status, {args, [integer_to_list(PPQ)]}
    ]),
    port_command(Port, [?HWCLOCK_START, 2, BPM, NTicks]),
    receive
        {Port, {data, <<0>>}} ->
            ok
    after 5000 ->
        throw({Fdlink, not_responding, Port})
    end,
    Port.

-spec stop(port()) -> true.
stop(Port) ->
    port_command(Port, [?HWCLOCK_STOP]).

-spec get_tick(binary()) -> non_neg_integer().
get_tick(Opaque) ->
    <<0, 50, Tick:32>> = Opaque,
    Tick.

-spec get_tick_s(binary(), non_neg_integer(), non_neg_integer()) ->
    {integer(), float()} | nil.
get_tick_s(Opaque, BPM, PPQ) ->
    case Opaque of
        <<0, 50, Tick:32>> ->
            {Tick, 60 * Tick / BPM / PPQ};
        Other ->
            io:format("Error, invalid tick format: ~w.~n", [Other]),
            nil
    end.
