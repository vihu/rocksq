.PHONY: compile

REBAR=./rebar3

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

release:
	$(REBAR) release

typecheck:
	$(REBAR) dialyzer

test:compile
	$(REBAR) ct
