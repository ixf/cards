
-module(gameserver_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(gameserver_sup, []).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [#{id => arbiter,
                    start => {arbiter, start_link, []},
                    shutdown => brutal_kill }],
    {ok, {SupFlags, ChildSpecs}}.
