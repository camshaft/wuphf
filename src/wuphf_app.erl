-module(wuphf_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  {ok, _} = gen_cowboy:start_http(wuphf_http_server),
  wuphf_sup:start_link().

stop(_State) ->
  ok.
