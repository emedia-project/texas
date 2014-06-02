REBAR       = ../../rebar

.PHONY: compile get-deps 

all: compile

compile: get-deps
	@$(REBAR) compile

get-deps:
	@$(REBAR) get-deps
	@$(REBAR) check-deps

clean:
	@$(REBAR) clean
	rm -f erl_crash.dump

realclean: clean
	@$(REBAR) delete-deps

dev:
	rm -rf sample.db
	@erl -pa ebin include apps/*/ebin apps/*/include deps/*/ebin deps/*/include

