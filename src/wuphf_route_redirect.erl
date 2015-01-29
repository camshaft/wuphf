-module(wuphf_route_redirect).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, _Args) ->
  {ok, Req, noop}.

handle(Req, State) ->
  {Network, Req2} = cowboy_req:qs_val(<<"network">>, Req, <<"facebook">>),

  wuphf_events:queue(Network, <<"shared">>, Req2),

  Body = [
    <<"<!DOCTYPE html><html><head></head><body>">>,
    <<"<script>window.close();</script>">>,
    <<"</body></html>">>
  ],

  {ok, Req3} = cowboy_req:reply(200, [
    {<<"content-type">>, <<"text/html">>},
    {<<"cache-control">>, <<"max-age=3600">>}
  ], Body, Req2),

  {ok, Req3, State}.

terminate(_, _, _) ->
  ok.
