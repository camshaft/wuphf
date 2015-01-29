-module(wuphf_route_network_email).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, _Args) ->
  {ok, Req, noop}.

handle(Req, State) ->
  {Params, Req2} = cowboy_req:qs_vals(Req),

  Body = share(Params, Req),

  {ok, Req3} = cowboy_req:reply(200, [
    {<<"content-type">>, <<"text/html">>},
    {<<"cache-control">>, <<"max-age=2629743">>}
  ], Body, Req2),

  wuphf_events:queue(<<"email">>, <<"shared">>, Req3),

  {ok, Req3, State}.

terminate(_, _, _) ->
  ok.

share(Params, _Req) ->
  Out = [
    {<<"Subject">>, fast_key:get(<<"title">>, Params, <<>>)},
    {<<"Body">>, <<(fast_key:get(<<"description">>, Params, <<>>))/binary, "\n\n", (fast_key:get(<<"url">>, Params, <<>>))/binary>>}
  ],
  [
    <<"<!DOCTYPE html><html><head>">>,
    <<"<meta http-equiv=\"refresh\" content=\"0; url=mailto:?">>,
      binary:replace(cow_qs:qs(Out), <<"+">>, <<" ">>, [global]),
    <<"\" />">>,
    <<"</head><body></body></html>">>
  ].
