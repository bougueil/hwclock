hwclock
=====
[![CI](https://github.com/bougueil/hwclock/actions/workflows/ci.yml/badge.svg)](https://github.com/bougueil/hwclock/actions/workflows/ci.yml)


A real-time OTP NIF relying on libasound.

`hwclock` is capable of emitting hardware events at a few kHz.

Build (Linux)
-----

    $ sudo apt-get install libasound2-dev 
    $ make
    # evaluate drift of erlang events with :
    $ erl -noshell -pa _build/default/lib/hwclock/ebin -s hwclock_bench | tee output_eee.dat


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

```elixir
iex> :hwclock_bench.start()
```

Api usage :

```elixir
iex> _pid = :hwclock.open_notify 60, 128, self()
iex> flush
# get real time clock data from libasound
{:hwclock, 0}
{:hwclock, 128}
...

```