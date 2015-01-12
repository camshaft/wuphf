-module(wuphf_route_network_facebook).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([make_ogo/2]).

init(_Type, Req, _Args) ->
  {ok, Req, noop}.

handle(Req, State) ->
  {Params, Req2} = cowboy_req:qs_vals(Req),

  {ok, Req3} = case share(Params, Req2) of
    {ok, Url} ->
      {ok, ResReq} = cowboy_req:reply(303, [
        {<<"location">>, Url}
      ], Req2),
      wuphf_events:queue(<<"facebook">>, <<"opened">>, ResReq),
      {ok, ResReq};
    Other ->
      cowboy_req:reply(400, [], io_lib:format("~p~n", [Other]), Req2)
  end,

  {ok, Req3, State}.

terminate(_, _, _) ->
  ok.

share(Params, Req) ->
  case is_fb_app(Req) of
    true ->
      sharer(Params, Req);
    _ ->
      case lists:keymember(<<"override">>, 1, Params) of
        true ->
          share_open_graph(Params, Req);
        _ ->
          share_individual(Params, Req)
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

sharer(Params, Req) ->
  Out = sharer_params(Params, [
    {<<"redirect_uri">>, redirect_url(Req)}
  ]),
  <<"https://www.facebook.com/sharer.php?", (cow_qs:qs(Out))/binary>>.

sharer_params([], Acc) ->
  Acc;
sharer_params([{<<"url">>, Url}|Rest], Acc) ->
  sharer_params(Rest, [{<<"u">>, Url}|Acc]);
sharer_params([_|Rest], Acc) ->
  sharer_params(Rest, Acc).

share_individual(Params, Req) ->
  Out = [
    {<<"app_id">>, fast_key:get(<<"fb_app_id">>, Params, <<>>)},
    {<<"display">>, <<"popup">>},
    {<<"redirect_uri">>, redirect_url(Req)},
    {<<"version">>, <<"v2.2">>},
    {<<"href">>, fast_key:get(<<"url">>, Params, <<>>)}
  ],
  {ok, <<"https://www.facebook.com/v2.2/dialog/share?", (cow_qs:qs(Out))/binary>>}.

share_open_graph(Params, Req) ->
  case share_params(Params, [], undefined) of
    {ok, Obj} ->
      Out = [
        {<<"app_id">>, fast_key:get(<<"fb_app_id">>, Params, <<>>)},
        {<<"display">>, <<"popup">>},
        {<<"redirect_uri">>, redirect_url(Req)},
        {<<"version">>, <<"v2.2">>},
        {<<"action_type">>, fast_key:get(<<"action_type">>, Params, <<"og.likes">>)},
        {<<"action_properties">>, Obj}
      ],
      {ok, <<"https://www.facebook.com/v2.2/dialog/share_open_graph?", (cow_qs:qs(Out))/binary>>};
    Other ->
      Other
  end.

share_params([], Acc, AppID) ->
  make_ogo(Acc, AppID);
share_params([{<<"fb_app_id">>, AppID}|Rest], Acc, _) ->
  share_params(Rest, Acc, AppID);
share_params([{<<"url">>, Val}|Rest], Acc, AppID) ->
  share_params(Rest, [{<<"url">>, Val}|Acc], AppID);
share_params([{<<"title">>, Val}|Rest], Acc, AppID) ->
  share_params(Rest, [{<<"title">>, Val}|Acc], AppID);
share_params([{<<"image">>, Val}|Rest], Acc, AppID) ->
  share_params(Rest, [{<<"image">>, Val}|Acc], AppID);
share_params([{<<"description">>, Val}|Rest], Acc, AppID) ->
  share_params(Rest, [{<<"description">>, Val}|Acc], AppID);
share_params([_|Rest], Acc, AppID) ->
  share_params(Rest, Acc, AppID).

redirect_url(Req) ->
  {EventUrl, _} = cowboy_req:qs_val(<<"event_url">>, Req, <<>>),
  Out = [
    {<<"network">>, <<"facebook">>},
    {<<"event_url">>, EventUrl}
  ],
  Root = case cowboy_req:qs_val(<<"redirect_uri">>, Req, <<>>) of
    {<<>>, _} ->
      cowboy_base:resolve([<<"redirect">>], Req);
    {R, _} ->
      R
  end,
  <<Root/binary, "?", (cow_qs:qs(Out))/binary>>.

make_ogo(Obj, AppID) ->
  {ok, Token} = get_token(AppID),

  Headers = [
    {"user-agent", "wuphf (+https://github.com/camshaft/wuphf)"}
  ],

  Body = cow_qs:qs([
    {<<"access_token">>, Token},
    {<<"object">>, list_to_binary(json_stringify:from_term(Obj))}
  ]),
  httpc_request(Headers, Body).

httpc_request(Headers, Body) ->
  case httpc:request(post, {"https://graph.facebook.com/app/objects/article", Headers, "application/x-www-form-urlencoded", Body}, [], []) of
    {ok, {{_, 200, _}, _Headers, ResBody}} ->
      parse_body(list_to_binary(ResBody));
    {ok, {_Status, _Headers, ResBody}} ->
      {error, ResBody};
    Error ->
      Error
  end.

parse_body(Body) ->
  case jsx:decode(Body) of
    [{<<"id">>, ID}] ->
      {ok, list_to_binary(json_stringify:from_term(#{<<"object">> => ID}))};
    Other ->
      io:format("~p~n", [Other]),
      {error, <<"failed to share artcile">>}
  end.

get_token(undefined) ->
  {ok, <<>>};
get_token(AppID) ->
  {ok, list_to_binary([AppID, $|, simple_env:get_binary(<<"APP_", AppID/binary>>, <<"">>)])}.
