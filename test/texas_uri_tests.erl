-module(texas_uri_tests).

-include_lib("eunit/include/eunit.hrl").

tmdb_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
      ?_test(complete()),
      ?_test(auth_with_extra()),
      ?_test(without_auth()),
      ?_test(without_port()),
      ?_test(without_path()),
      ?_test(without_query()),
      ?_test(without_fragment()),
      ?_test(without_authority()),
      ?_test(with_error())
   ]}.

% Tests

complete() ->
  ?assertEqual(
    texas_uri:parse("http://login:password@www.example.com:8080/path?hello=world#go"),
    {ok,
     {"http", "login", "password", "www.example.com", "8080", "path", [{"hello", "world"}], "go"}}).

auth_with_extra() ->
  ?assertEqual(
    texas_uri:parse("http://login:p@ssword@www.example.com:8080/path?hello=world#go"),
    {ok,
     {"http", "login", "p@ssword", "www.example.com", "8080", "path", [{"hello", "world"}], "go"}}),
  ?assertEqual(
    texas_uri:parse("http://login::p@ssword@www.example.com:8080/path?hello=world#go"),
    {ok,
     {"http", "login", ":p@ssword", "www.example.com", "8080", "path", [{"hello", "world"}], "go"}}).

without_auth() ->
  ?assertEqual(
    texas_uri:parse("http://www.example.com:8080/path?hello=world#go"),
    {ok,
     {"http", "", "", "www.example.com", "8080", "path", [{"hello", "world"}], "go"}}),
  ?assertEqual(
    texas_uri:parse("http://login@www.example.com:8080/path?hello=world#go"),
    {ok,
     {"http", "login", "", "www.example.com", "8080", "path", [{"hello", "world"}], "go"}}),
  ?assertEqual(
    texas_uri:parse("http://:password@www.example.com:8080/path?hello=world#go"),
    {ok,
     {"http", "", "password", "www.example.com", "8080", "path", [{"hello", "world"}], "go"}}).

without_port() ->
  ?assertEqual(
    texas_uri:parse("http://login:password@www.example.com/path?hello=world#go"),
    {ok,
     {"http", "login", "password", "www.example.com", "", "path", [{"hello", "world"}], "go"}}).

without_path() ->
  ?assertEqual(
    texas_uri:parse("http://login:password@www.example.com:8080?hello=world#go"),
    {ok,
     {"http", "login", "password", "www.example.com", "8080", "", [{"hello", "world"}], "go"}}),
  ?assertEqual(
    texas_uri:parse("http://login:password@www.example.com:8080/?hello=world#go"),
    {ok,
     {"http", "login", "password", "www.example.com", "8080", "", [{"hello", "world"}], "go"}}).

without_query() ->
  ?assertEqual(
    texas_uri:parse("http://login:password@www.example.com:8080/path#go"),
    {ok,
     {"http", "login", "password", "www.example.com", "8080", "path", [], "go"}}).

without_fragment() ->
  ?assertEqual(
    texas_uri:parse("http://login:password@www.example.com:8080/path?hello=world"),
    {ok,
     {"http", "login", "password", "www.example.com", "8080", "path", [{"hello", "world"}], ""}}).

without_authority() ->
  ?assertEqual(
    texas_uri:parse("file:///path?hello=world#go"),
    {ok,
     {"file", "", "", "", "", "path", [{"hello", "world"}], "go"}}).

with_error() ->
  ?assertEqual(
    texas_uri:parse("this is not an url"), 
    {error, scheme_not_found}).

% Helpers

setup() ->
  ok.

teardown(_) ->
  ok.
