-module(wuphf_route_root).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, _Args) ->
  {ok, Req, noop}.

handle(Req, State) ->
  {Url, Req2} = cowboy_req:qs_val(<<"url">>, Req, <<>>),
  {Title, Req2} = cowboy_req:qs_val(<<"title">>, Req, <<>>),
  {Description, Req2} = cowboy_req:qs_val(<<"description">>, Req, <<>>),
  {Image, Req2} = cowboy_req:qs_val(<<"image">>, Req, <<>>),
  {AppID, Req2} = cowboy_req:qs_val(<<"app_id">>, Req, <<>>),
  {Caption, Req2} = cowboy_req:qs_val(<<"caption">>, Req, <<>>),
  {Via, Req2} = cowboy_req:qs_val(<<"via">>, Req, <<>>),
  {Hashtags, Req2} = cowboy_req:qs_val(<<"hashtags">>, Req, <<>>),
  {EventUrl, Req2} = cowboy_req:qs_val(<<"event_url">>, Req, <<>>),

  Body = json_stringify:from_term(#{
    <<"facebook">> => network(#{
      <<"app_id">> => #{
        <<"type">> => <<"text">>,
        <<"value">> => AppID
      },
      <<"url">> => #{
        <<"type">> => <<"url">>,
        <<"value">> => Url
      },
      <<"title">> => #{
        <<"type">> => <<"text">>,
        <<"value">> => Title
      },
      <<"description">> => #{
        <<"type">> => <<"text">>,
        <<"value">> => Description
      },
      <<"image">> => #{
        <<"type">> => <<"url">>,
        <<"value">> => Image
      },
      <<"caption">> => #{
        <<"type">> => <<"text">>,
        <<"value">> => Caption
      },
      <<"event_url">> => #{
        <<"type">> => <<"url">>,
        <<"value">> => EventUrl
      }
    }, <<"facebook">>, Req),
    <<"twitter">> => network(#{
      <<"url">> => #{
        <<"type">> => <<"url">>,
        <<"value">> => Url
      },
      <<"title">> => #{
        <<"type">> => <<"text">>,
        <<"value">> => Title
      },
      <<"via">> => #{
        <<"type">> => <<"text">>,
        <<"value">> => Via
      },
      <<"hashtags">> => #{
        <<"type">> => <<"text">>,
        <<"value">> => Hashtags
      },
      <<"event_url">> => #{
        <<"type">> => <<"url">>,
        <<"value">> => EventUrl
      }
    }, <<"twitter">>, Req),
    <<"gplus">> => network(#{
      <<"url">> => #{
        <<"type">> => <<"url">>,
        <<"value">> => Url
      },
      <<"event_url">> => #{
        <<"type">> => <<"url">>,
        <<"value">> => EventUrl
      }
    }, <<"gplus">>, Req),
    <<"linkedin">> => network(#{
      <<"url">> => #{
        <<"type">> => <<"url">>,
        <<"value">> => Url
      },
      <<"title">> => #{
        <<"type">> => <<"text">>,
        <<"value">> => Title
      },
      <<"description">> => #{
        <<"type">> => <<"text">>,
        <<"value">> => Description
      },
      <<"event_url">> => #{
        <<"type">> => <<"url">>,
        <<"value">> => EventUrl
      }
    }, <<"linkedin">>, Req),
    <<"pinterest">> => network(#{
      <<"url">> => #{
        <<"type">> => <<"url">>,
        <<"value">> => Url
      },
      <<"title">> => #{
        <<"type">> => <<"text">>,
        <<"value">> => Title
      },
      <<"image">> => #{
        <<"type">> => <<"url">>,
        <<"value">> => Image
      },
      <<"event_url">> => #{
        <<"type">> => <<"url">>,
        <<"value">> => EventUrl
      }
    }, <<"pinterest">>, Req),
    <<"reddit">> => network(#{
      <<"url">> => #{
        <<"type">> => <<"url">>,
        <<"value">> => Url
      },
      <<"title">> => #{
        <<"type">> => <<"text">>,
        <<"value">> => Title
      },
      <<"event_url">> => #{
        <<"type">> => <<"url">>,
        <<"value">> => EventUrl
      }
    }, <<"reddit">>, Req),
    <<"tumblr">> => network(#{
      <<"url">> => #{
        <<"type">> => <<"url">>,
        <<"value">> => Url
      },
      <<"title">> => #{
        <<"type">> => <<"text">>,
        <<"value">> => Title
      },
      <<"description">> => #{
        <<"type">> => <<"text">>,
        <<"value">> => Description
      },
      <<"event_url">> => #{
        <<"type">> => <<"url">>,
        <<"value">> => EventUrl
      }
    }, <<"tumblr">>, Req),
    <<"email">> => network(#{
      <<"url">> => #{
        <<"type">> => <<"url">>,
        <<"value">> => Url
      },
      <<"title">> => #{
        <<"type">> => <<"text">>,
        <<"value">> => Title
      },
      <<"description">> => #{
        <<"type">> => <<"text">>,
        <<"value">> => Description
      },
      <<"event_url">> => #{
        <<"type">> => <<"url">>,
        <<"value">> => EventUrl
      }
    }, <<"email">>, Req),
    <<"apply">> => #{
      <<"action">> => cowboy_base:resolve([], Req),
      <<"method">> => <<"GET">>,
      <<"input">> => #{
        <<"app_id">> => #{
          <<"type">> => <<"text">>,
          <<"value">> => AppID
        },
        <<"url">> => #{
          <<"type">> => <<"url">>,
          <<"value">> => Url
        },
        <<"title">> => #{
          <<"type">> => <<"text">>,
          <<"value">> => Title
        },
        <<"description">> => #{
          <<"type">> => <<"text">>,
          <<"value">> => Description
        },
        <<"image">> => #{
          <<"type">> => <<"url">>,
          <<"value">> => Image
        },
        <<"caption">> => #{
          <<"type">> => <<"text">>,
          <<"value">> => Caption
        },
        <<"via">> => #{
          <<"type">> => <<"text">>,
          <<"value">> => Via
        },
        <<"hashtags">> => #{
          <<"type">> => <<"text">>,
          <<"value">> => Hashtags
        },
        <<"event_url">> => #{
          <<"type">> => <<"url">>,
          <<"value">> => EventUrl
        }
      }
    }
  }),

  {ok, Req3} = cowboy_req:reply(200, [
    {<<"content-type">>, <<"application/json">>},
    {<<"cache-control">>, <<"max-age=3600">>}
  ], Body, Req2),

  {ok, Req3, State}.

network(Inputs, Name, Req) ->
  #{
    <<"action">> => cowboy_base:resolve([<<"networks">>, Name], Req),
    <<"method">> => <<"GET">>,
    <<"input">> => Inputs
  }.

terminate(_, _, _) ->
  ok.
