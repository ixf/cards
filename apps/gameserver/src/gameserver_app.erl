
-module(gameserver_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->

  Dispatch = cowboy_router:compile([ {'_',
				      [
				       {"/", cowboy_static, {file, "index.html"}},
				       {"/game/:id/:color", ws_gs, #{} },
				       {"/static/[...]", cowboy_static, {dir, "static"}}
				      ]} ]),
  {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
					 env => #{dispatch => Dispatch}
					}),
  gameserver_sup:start_link().

stop(_State) ->
  ok.
