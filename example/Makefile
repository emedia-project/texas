.PHONY: doc
REBAR = ../rebar3

compile:
	@$(REBAR) compile

dev: compile
	@rm -rf sample.db
	@erl -pa _build/default/lib/*/ebin _build/default/lib/*/include -config config/sys.config

distclean:
	@rm -rf _build rebar.lock mix.lock test/eunit deps

