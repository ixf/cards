
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


act("givestate", Params, State) ->
  Response = jiffy:encode(#{<<"action">> => <<"state">>, <<"params">> => State}),
  {State, {text, Response}};

act("cardclick", Params, State) ->
  io:format("~w~n", [Params]),
  Name = maps:get(<<"name">>, State),
  Id = maps:get(<<"id">>, Params),
  arbiter:click(Name, Id),
  {State, ok};

act("update", Params, State) ->

  Pos = maps:get(<<"cursor">>, Params),
  Name = maps:get(<<"name">>, State),
  arbiter:imoved(Name, Pos),
  {State, ok};

act(Action, _Params, State) ->
  io:format("niewiem co to: ~w~n", [Action]),
  {State, ok}.


respond(ok, State) -> {ok, State};
respond(Response, State) -> {reply, Response, State}.


init(Req, State) ->
  io:format("~w~n", [State]),
  Name = cowboy_req:binding(id, Req),
  Color = cowboy_req:binding(color, Req),
  Opts = #{ idle_timeout => 90000 },
  {cowboy_websocket, Req, State#{ <<"name">> => Name, <<"color">> => Color }, Opts}.

websocket_init(State) ->
  Name = maps:get(<<"name">>, State),
  Color = maps:get(<<"color">>, State),
  arbiter:hello(Name, Color),
  {state, NewState} = arbiter:givestate(),
  {ok, NewState#{ <<"name">> => Name, <<"color">> => Color }}.

websocket_handle({text, Json}, State) ->
  Result = jiffy:decode(<<Json/binary>>, [return_maps]),

  % io:format("~w~n", [Result]),

  Action = binary:bin_to_list(maps:get(binary:list_to_bin("action"), Result)),
  Params = maps:get(binary:list_to_bin("params"), Result),

  {NewState, Response} = act(Action, Params, State),
  respond(Response, NewState).

terminate(Reason, PartialReq, State) ->
  arbiter:bye(maps:get(<<"name">>, State)),
  ok.

websocket_info({update, What}, State) ->
  {reply, {text, What}, State};

websocket_info(_Info, State) ->
  io:format("wrong info!~n"),
  {ok, State}.
