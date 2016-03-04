PROJECT = texas_example

DEPS = texas
dep_texas = git https://github.com/emedia-project/texas.git master
DEPS += texas_sqlite 
dep_texas_sqlite = cp ../../texas_sqlite
DEPS += texas_mysql 
dep_texas_mysql = cp ../../texas_mysql
DEPS += texas_pgsql
dep_texas_pgsql = cp ../../texas_pgsql

include ../erlang.mk

app::
	make -C ..

dev:
	rm -rf sample.db
	@erl -pa ebin include deps/*/ebin deps/*/include -config config/sys.config

