-module(texas_app).

-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  ok = application:start(syntax_tools),
  ok = application:start(goldrush),
  ok = application:start(lager). 

stop(_State) ->
  ok. 

