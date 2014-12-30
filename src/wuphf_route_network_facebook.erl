-module(wuphf_route_network_facebook).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, _Args) ->
  {ok, Req, noop}.

handle(Req, State) ->
  {Params, Req2} = cowboy_req:qs_vals(Req),

  {ok, Req3} = cowboy_req:reply(303, [
    {<<"location">>, choose(Params, Req2)}
  ], Req2),

  wuphf_events:queue(<<"facebook">>, <<"open">>, Req3),

  {ok, Req3, State}.

terminate(_, _, _) ->
  ok.

choose(Params, Req) ->
  case is_fb_app(Req) of
    true ->
      sharer(Params, Req);
    false ->
      case lists:keymember(<<"title">>, 1, Params) of
        true ->
          feed(Params, Req);
        _ ->
          share(Params, Req)
      end
  end.

is_fb_app(Req) ->
  {UA, _} = cowboy_req:header(<<"user-agent">>, Req, <<>>),
  case binary:split(UA, <<"FBAN/FBIOS">>) of
    [_] ->
      false;
    _ ->
      true
  end.

share(Params, Req) ->
  Out = share_params(Params, [
    {<<"display">>, <<"popup">>},
    {<<"redirect_uri">>, cowboy_base:resolve([<<"redirect?network=facebook">>], Req)}
  ]),
  <<"https://www.facebook.com/dialog/share?", (cow_qs:qs(Out))/binary>>.

share_params([], Acc) ->
  Acc;
share_params([{<<"app_id">>, _} = AppID|Rest], Acc) ->
  share_params(Rest, [AppID|Acc]);
share_params([{<<"url">>, Url}|Rest], Acc) ->
  share_params(Rest, [{<<"href">>, Url}|Acc]);
share_params([_|Rest], Acc) ->
  share_params(Rest, Acc).

feed(Params, Req) ->
  Out = feed_params(Params, [
    {<<"display">>, <<"popup">>},
    {<<"e2e">>, <<"{}">>},
    {<<"redirect_uri">>, cowboy_base:resolve([<<"redirect?network=facebook">>], Req)}
  ]),
  <<"https://www.facebook.com/dialog/feed?", (cow_qs:qs(Out))/binary>>.

feed_params([], Acc) ->
  Acc;
feed_params([{<<"title">>, Title}|Rest], Acc) ->
  feed_params(Rest, [{<<"name">>, Title}|Acc]);
feed_params([{<<"app_id">>, _} = AppID|Rest], Acc) ->
  feed_params(Rest, [AppID|Acc]);
feed_params([{<<"caption">>, _} = Caption|Rest], Acc) ->
  feed_params(Rest, [Caption|Acc]);
feed_params([{<<"description">>, _} = Description|Rest], Acc) ->
  feed_params(Rest, [Description|Acc]);
feed_params([{<<"image">>, Image}|Rest], Acc) ->
  feed_params(Rest, [{<<"picture">>, Image}|Acc]);
feed_params([{<<"url">>, Url}|Rest], Acc) ->
  feed_params(Rest, [{<<"link">>, Url}|Acc]);
feed_params([_|Rest], Acc) ->
  feed_params(Rest, Acc).

sharer(Params, Req) ->
  Out = sharer_params(Params, [
    {<<"redirect_uri">>, cowboy_base:resolve([<<"redirect?network=facebook">>], Req)}
  ]),
  <<"https://www.facebook.com/sharer.php?", (cow_qs:qs(Out))/binary>>.

sharer_params([], Acc) ->
  Acc;
sharer_params([{<<"url">>, Url}|Rest], Acc) ->
  sharer_params(Rest, [{<<"u">>, Url}|Acc]);
sharer_params([_|Rest], Acc) ->
  sharer_params(Rest, Acc).
