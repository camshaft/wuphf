-module(wuphf_route_network_tumblr).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, _Args) ->
  {ok, Req, noop}.

handle(Req, State) ->
  {Params, Req2} = cowboy_req:qs_vals(Req),

  {ok, Req3} = cowboy_req:reply(303, [
    {<<"location">>, share(Params, Req2)}
  ], Req2),

  wuphf_events:queue(<<"tumblr">>, <<"shared">>, Req3),

  {ok, Req3, State}.

terminate(_, _, _) ->
  ok.

share(Params, _Req) ->
  Out = share_params(Params, []),
  <<"https://www.tumblr.com/share/link?", (cow_qs:qs(Out))/binary>>.

share_params([], Acc) ->
  Acc;
share_params([{<<"url">>, _} = Url|Rest], Acc) ->
  share_params(Rest, [Url|Acc]);
share_params([{<<"title">>, Title}|Rest], Acc) ->
  share_params(Rest, [{<<"name">>, Title}|Acc]);
share_params([{<<"description">>, _} = Description|Rest], Acc) ->
  share_params(Rest, [Description|Acc]);
share_params([_|Rest], Acc) ->
  share_params(Rest, Acc).
