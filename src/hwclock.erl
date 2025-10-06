-module(hwclock).

-export([open/2, open/3, get_tick/1, event_duration_s/2, event_duration_s/3, get_tick_s/2,
         get_tick_s/3, stop/1]).

-ignore_xref([get_tick_s/3, open/3, event_duration_s/2, event_duration_s/3]).

-compile({inline, [get_tick/1, event_duration_s/2, event_duration_s/3]}).

-define(HWCLOCK_START, 3).
-define(HWCLOCK_STOP, 4).
-define(PPQ_DEFAULT, 128).
-define(DURATION_COEFF_PPQ, 60).
% 60/128 = 0.46875
-define(DURATION_COEFF, 0.46875).

-doc("Start an `hwclock` process with BPM and NTicks, where :\n - BPM is an integer between 1 and 255 defining the tempo\n - Nticks is an integer between 1 and 255 defining the number of ticks in a tempo\n\nEquivalent to open(BPM, NTick, 128).").

-spec open(BPM :: byte(), NTicks :: byte()) -> Port :: port().
open(BPM, NTicks) when NTicks > 0, NTicks < 256, BPM > 0, BPM < 256 ->
    open(BPM, NTicks, ?PPQ_DEFAULT).

-doc("Start an `hwclock` process with BPM and NTicks, where :\n - BPM is an integer between 1 and 255 defining the tempo\n - Nticks is an integer between 1 and 255 defining the number of ticks in a tempo\n - PPQ is an between 1 and 1024, see duration_ms calculation\n\nReturns the `hwclock` port.\n\n# duration between hwclock events\n```bash\nduration_ms = 1000* 60/bpm * nticks/128 * 128/ppq \n```\n\nSee event_duration_s/3 computation.\n\n## Examples\n\n```erlang\n%% receive an event every 1000 ms.\n1> hwclock:open(60, 128, 128).\n#Port<0.21>\n\n%% receive an event every 500 ms.\n1> hwclock:open(60, 64, 128).\n\n%% receive an event every 500 ms.\n1> hwclock:open(120, 128, 128).\n\n%% receive an event every 250 ms.\n1> hwclock:open(120, 128, 256).\n```\n\n## Handling hwclock Events\n\n```erlang\n%% receive an event every 1000 ms.\nBPM = 60,\nNTicks = 128\nPort = hwclock:open(BPM, NTicks),\n\nreceive do\n  {Port, {:data, <<0xFF>>}} ->\n    # the hwclock process is being closed\n  {Port, {:data, Opaque}} ->\n    {Tick, Elapsed_s} = hwclock:get_tick_s(Opaque, Bpm),\n    # Tick = NTicks\n    # Elapsed_s = 1.0 \nend\n```").

-spec open(BPM :: byte(), NTicks :: byte(), PPQ :: integer()) -> Port :: port().
open(BPM, NTicks, PPQ)
    when NTicks > 0, NTicks < 256, BPM > 0, BPM < 256, PPQ > 0, PPQ =< 1024 ->
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
    Port =
        open_port({spawn_executable, Exec},
                  [{packet, 2}, binary, exit_status, {args, [integer_to_list(PPQ)]}]),
    port_command(Port, [?HWCLOCK_START, 2, BPM, NTicks]),
    receive
        {Port, {data, <<0>>}} ->
            ok
    after 5000 ->
        throw({Fdlink, not_responding, Port})
    end,
    Port.

-doc("Returns in seconds the elapsed_time between two consecutive events\n\nEquivalent to event_duration_s(BPM, NTick, 128).").

-spec event_duration_s(BPM :: byte(), NTicks :: byte()) -> Duration :: float().
event_duration_s(BPM, NTicks) when NTicks > 0, NTicks < 256, BPM > 0, BPM < 256 ->
    % formula: 60 / BPM * NTicks / 128.
    ?DURATION_COEFF / BPM * NTicks.

-doc("Returns in seconds the elapsed_time between two consecutive events").

-spec event_duration_s(BPM :: byte(), NTicks :: byte(), PPQ :: integer()) ->
                          Duration :: float().
event_duration_s(BPM, NTicks, PPQ)
    when NTicks > 0, NTicks < 256, BPM > 0, BPM < 256, PPQ > 0, PPQ =< 1024 ->
    % formula: 60 / BPM * NTicks / 128 * 128 / PPQ.
    ?DURATION_COEFF_PPQ / BPM * NTicks / PPQ.

-doc("Stop an `hwclock` process.\n\nPort is the port value returned by open/2,3\n\nReturns true.").

-spec stop(Port :: port()) -> true.
stop(Port) ->
    port_command(Port, [?HWCLOCK_STOP]).

-doc("From the Opaque event data,\nextracts the tick number.\n\nThe tick number increases after each event by a step of NTicks.\n\nSee open/2.").

-spec get_tick(Opaque :: binary()) -> Tick :: non_neg_integer().
get_tick(<<0, 50, Tick:32>>) ->
    Tick.

-doc("Same as get_tick_s(Opaque, BPM, 128)").

-spec get_tick_s(Opaque :: binary(), BPM :: non_neg_integer()) ->
                    {Tick :: integer(), Timestamp :: float()} | nil.
get_tick_s(Opaque, BPM) ->
    Tick = get_tick(Opaque),
    {Tick, ?DURATION_COEFF * Tick / BPM}.

-doc("From the Opaque event data and a given BPM and PPQ,\nextracts the tick number and tick timestamp in second as a float.\n\nReturns the tuple {tick_number, tick_timestamp_in_s}.\n\n").

-spec get_tick_s(Opaque :: binary(),
                 BPM :: non_neg_integer(),
                 PPQ :: non_neg_integer()) ->
                    {Tick :: integer(), Timestamp :: float()} | nil.
get_tick_s(Opaque, BPM, PPQ) ->
    Tick = get_tick(Opaque),
    {Tick, ?DURATION_COEFF_PPQ * Tick / BPM / PPQ}.
