-module(wuphf_http_server).

-export([init/0]).
-export([middlewares/0]).
-export([reload/0]).
-rl(reload).

init() ->
  ok.

middlewares() ->
  dev_middlewares() ++ [
    cowboy_cors:init([handle_options]),
    cowboy_base:init(),
    wuphf_router,
    cowboy_handler
  ].

dev_middlewares() ->
  case simple_env:get("ERL_ENV") of
    "development" ->
      [
        fun cowboy_dev_logger:execute/3
      ];
    _ ->
      []
  end.

reload() ->
  gen_cowboy:reload(?MODULE).
