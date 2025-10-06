.PHONY: all clean clean-all compile test doc dialyzer xref ci

REBAR3 ?= rebar3

all: compile format xref test

ci: compile xref dialyzer test

compile:
	$(REBAR3) compile

clean:
	$(REBAR3) clean

clean-all: clean
	rm -rf _build

test:
	$(REBAR3) do eunit
# 	$(REBAR3) do ct

xref:
	$(REBAR3) xref

doc:
	$(REBAR3) ex_doc

format:
	$(REBAR3) format

dialyzer: compile
	$(REBAR3) dialyzer

bench: compile
	 erl -noshell -pa _build/default/lib/hwclock/ebin -s hwclock_bench | tee output_eee.dat