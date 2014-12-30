-module(wuphf).

-export([start/0]).

start() ->
  catch rl:make(),
  {ok, _} = application:ensure_all_started(wuphf),
  ok.
