REBAR       = ./rebar

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

test: get-deps
	@$(REBAR) skip_deps=true eunit

doc:
	@mkdir doc
	@cp _doc/* doc
	$(REBAR) skip_deps=true doc

dev:
	@erl -pa ebin include apps/*/ebin apps/*/include deps/*/ebin deps/*/include #-config sys.config

analyze: checkplt
	@$(REBAR) skip_deps=true dialyze

buildplt:
	@$(REBAR) skip_deps=true build-plt

checkplt: buildplt
	@$(REBAR) skip_deps=true check-plt
