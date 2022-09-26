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
    open/2,
    get_tick/1,
    get_tick_ms/2,
    stop/1
]).

%% To help with testing internal functions
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

%% 15625 us. => RESOLUTION 2
-define(HWRES, 15625).
-define(HWCLOCK_START, 3).
-define(HWCLOCK_STOP, 4).

-spec open(byte(), byte()) -> port().
open(BPM, NTicks) when
    NTicks >= 0,
    NTicks < 256,
    BPM >= 0,
    BPM < 256
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
    Port = open_port({spawn, Exec}, [{packet, 2}, binary, exit_status]),
    port_command(Port, [3, BPM, NTicks]),
    receive
        {_, {data, <<0>>}} -> ok
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

-spec get_tick_ms(non_neg_integer(), binary()) -> non_neg_integer().
get_tick_ms(BPM, Opaque) ->
    case Opaque of
        <<0, 50, Tick:32>> ->
            round((1000 * BPM * Tick) / (60 * 128));
        Other ->
            %% FIXME
            io:format("get_tick_ms Unknow Value: ~p.~n", [Other]),
            Other
    end.
