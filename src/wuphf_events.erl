-module(wuphf_events).

-export([queue/3]).
-export([emit_event/4]).

queue(Network, Event, Req) ->
  case cowboy_req:qs_val(<<"event_url">>, Req, <<>>) of
    {<<>>, _} ->
      ok;
    {EventUrl, Req2} ->
      {Headers, _} = cowboy_req:headers(Req2),
      Filtered = lists:keydelete(<<"host">>, 1, Headers),
      Json = fast_key:set(<<"content-type">>, <<"application/json">>, Filtered),
      spawn(?MODULE, emit_event, [EventUrl, Json, Network, Event]),
      ok
  end.

emit_event(Url, Headers, Network, Event) ->
  {Time, _} = timer:tc(fun perform_request/4, [Url, Headers, Network, Event]),
  io:format("at=event measure#api_request=~p url=~s network=~s event=~s~n", [Time, Url, Network, Event]).

perform_request(Url, Headers, Network, Event) ->
  {ok, {Scheme, _UserInfo, Host, Port, Path, Query}} = http_uri:parse(binary_to_list(Url)),
  {ok, Pid} = gun:open(Host, Port, [{type, type(Scheme)}]),
  Ref = gun:post(Pid, [Path, Query], Headers, json_stringify:from_term(#{
    <<"network">> => Network,
    <<"event">> => Event
  })),
  loop(Pid, Ref).

loop(Pid, Ref) ->
  receive
    {gun_response, Pid, Ref, _, _Status, _Headers} ->
      gun:shutdown(Pid);
    _ ->
      loop(Pid, Ref)
  after 10000 ->
    gun:close(Pid)
  end.

type(http) ->
  tcp;
type(_) ->
  ssl.
