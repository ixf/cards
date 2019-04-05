
-module(ws_gs).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).


% a player joins -> init, websocket_init
% this calls arbiter:hello with name and color ( from http get params )
% a player then sends a {givestate, ...} with all other players and cards
%
% every arbiter call informs other players about the changes
%
% the initial state is taken from the arbiter





init(Req, State) ->
  Name = cowboy_req:binding(id, Req),
  Color = cowboy_req:binding(color, Req),
  Opts = #{ idle_timeout => 90000 },
  {cowboy_websocket, Req, {Name,Color}, Opts}.

websocket_init({Name,Color}) ->
  {ok, Player} = arbiter:hello(Name, Color),
  {ok, {Name,Player}}.



websocket_handle({text, Json}, State) ->
  Result = jiffy:decode(<<Json/binary>>, [return_maps]),

  Action = binary:bin_to_list(maps:get(binary:list_to_bin("action"), Result)),
  Params = maps:get(binary:list_to_bin("params"), Result),

  Response = act(Action, Params, State),
  respond(Response, State).

respond(ok, State) -> {ok, State};
respond(Response, State) -> {reply, Response, State}.


act("cardclick", Params, {Name, Player}) ->
  Id = maps:get(<<"id">>, Params),
  Player ! {click, Id},
  ok;

act("rotate", Params, {Name, Player}) ->
  Id = maps:get(<<"id">>, Params),
  Player ! {rotate, Id},
  ok;

act("pos_change", Params, {Name, Player}) ->
  player:move(Player, maps:get(<<"cursor">>, Params)),
  ok;

act(Action, _Params, _Player) ->
  io:format("niewiem co to: ~s~n", [Action]),
  ok.




websocket_info({update, What}, State) ->
  {reply, {text, What}, State};

websocket_info(Info, State) ->
  io:format("wrong info: ~s~n", [Info]),
  {ok, State}.

terminate(_Reason, _PartialReq, {Name, State}) ->
  arbiter:bye(Name),
  ok.
