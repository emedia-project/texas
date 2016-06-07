.PHONY: doc
REBAR = ../rebar3

compile:
	@$(REBAR) compile

dist: compile
	@rm -rf sample.db
	@erl -pa ebin include deps/*/ebin deps/*/include -config config/sys.config

distclean:
	@rm -rf _build rebar.lock mix.lock test/eunit deps

