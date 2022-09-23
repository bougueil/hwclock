hwclock
=====

A real-time OTP library relying on libasound  

Build (Linux)
-----

    $ sudo apt-get install libasound2-dev 
    $ rebar3 compile
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

```elixir
iex> port = :hwclock::hwclock.open 60, 128
iex> flush
{#Port<0.8>, {:data, <<0, 50, ...>>}}
{#Port<0.8>, {:data, <<0, 50, ...>>}}
...
iex> :hwclock::hwclock.stop port
true
```
