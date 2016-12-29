hwclock
=====

A real-time OTP library relying on libasound  

Build (Linux)
-----

    $ rebar3 compile
    $ erl -noshell -pa _build/default/lib/hwclock/ebin -s hwclock_bench | tee output_eee.dat