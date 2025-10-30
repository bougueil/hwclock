hwclock
=====
[![CI](https://github.com/bougueil/hwclock/actions/workflows/ci.yml/badge.svg)](https://github.com/bougueil/hwclock/actions/workflows/ci.yml)


A real-time OTP NIF relying on libasound.

`hwclock` is capable of emitting hardware events at a few kHz.

Build (Linux)
-----

    $ sudo apt-get install libasound2-dev 
    $ make
    # evaluate erlang events drift against hwclock with :
    $ rebar3 shell --eval "hwclock_bench:start()."


## Installation with Elixir

Add hwclock as a dependency in your `mix.exs` file:

```elixir
def deps do
  [
     {:hwclock, git: "https://github.com/bougueil/hwclock.git"}
  ]
end
```

Next, run `mix deps.get` in your shell to fetch and compile `hwclock`. Start an
interactive Elixir shell with `iex -S mix`:

```bash
rebar3 shell --eval "hwclock_bench:start()."
```

Api usage :

<!-- tabs-open -->

### Erlang

```erlang
%% inside rebar3 shell
> hwclock:subscribe_pubsub("my_channel").
> hwclock:open_with_pubsub(60, 128, "my_channel").
> process_info(self(), messages).
{messages,[{hwclock,0},
           {hwclock,128},
           {hwclock,256},
           {hwclock,384},
           {hwclock,512}]}
```

### Elixir

```elixir
# inside iex -S mix
iex> :hwclock.subscribe_pubsub "my_channel"
iex> :hwclock.open_with_pubsub 60, 128, "my_channel"
iex> Process.info self(), :messages
{:messages,
 [
   hwclock: 0,
   hwclock: 128,
   hwclock: 256
 ]}

...
```

<!-- tabs-close -->
