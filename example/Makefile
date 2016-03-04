PROJECT = texas_example

DEPS = texas
dep_texas = git https://github.com/emedia-project/texas.git master
DEPS += texas_sqlite 
dep_texas_sqlite = git https://github.com/emedia-project/texas_sqlite.git master
DEPS += texas_mysql 
dep_texas_mysql = git https://github.com/emedia-project/texas_mysql.git master
DEPS += texas_pgsql
dep_texas_pgsql = git https://github.com/emedia-project/texas_pgsql.git master

include ../erlang.mk

dev:
	rm -rf sample.db
	@erl -pa ebin include deps/*/ebin deps/*/include -config config/sys.config

