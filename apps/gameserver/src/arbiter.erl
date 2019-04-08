

-module(arbiter).

-behaviour(gen_server).

-export([start_link/0]).
-export([hello/2, bye/1, broadcast/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).



start_link() ->
  gen_server:start_link({local, arbiter}, arbiter, [], []).

init(_Args) ->

  ets:new(cards, [set, named_table]),
  ets:new(players, [set, named_table]),
  ets:new(blockers, [bag, named_table]),

  setup_state(),
  {ok, running}.


setup_state() ->
  {ok, StateFile} = file:read_file("state.json"),
  State = jiffy:decode( binary:bin_to_list( StateFile ), [return_maps]),
  init_cards(maps:get(<<"cards">>, State)).

init_cards(M) ->
  I = maps:iterator(M),
  init_cards2(maps:next(I)).

init_cards2(none) -> ok;
init_cards2({Id,Value,Iter}) ->
  CardPid = card:new(Id, Value),
  ets:insert(cards, {Id,CardPid}),
  init_cards2(maps:next(Iter)).



hello(Name, Color) ->
  gen_server:call(arbiter, {hello, Name, Color}).

bye(Name) ->
  gen_server:call(arbiter, {bye, Name}).


broadcast(What) ->
  ets:foldl(fun({_Name, Ws, _Player}, _Acc) ->
                Ws ! What,
                acc
            end, acc, players),
  ok.


handle_call({hello, Name, Color}, {Pid, _Tag}, State) ->

  case ets:lookup(players, Name) of
    [] ->
      ThisPlayer = setup_player(Name, Color, Pid),
      {reply, {ok,ThisPlayer}, State};
    _ ->
      {reply, {error,<<"taken">>}, State}
  end;


handle_call({bye, Name}, _From, State) ->
  io:format("player left: ~s~n", [Name]),

  Update = jiffy:encode(#{ <<"action">> => <<"playerleft">>,
                           <<"params">> => #{ <<"name">> => Name } }),
  broadcast({update, Update}),

  case ets:lookup(players, Name) of
    [] -> {noreply, State};
    [{Name, Ws, Player}] ->
      ets:delete(players, Name),
      Player ! leave,
      {reply, ok, State}
  end.


setup_player(Name, Color, Pid) ->
  ThisPlayer = player:new(Pid, Name, Color),

  io:format("player joins: ~s~n", [Name]),

                                                % inform about cards
  ets:foldl(fun({_Id, CardPid}, _Acc) ->
                card:getjson(CardPid, Pid), % Pid to pid gracza
                acc
            end, acc, cards),

                                                % inform other players about new guy
  ets:foldl(fun({_Name, WsPid, _PlayerPid}, _Acc) ->
                player:getjson(ThisPlayer, WsPid),
                acc
            end, acc, players),

                                                % add the player to table
                                                % {Name, WsPid, PlayerPid}
  ets:insert(players, {Name, Pid, ThisPlayer}),

                                                % inform about all players ( including themselves )
  ets:foldl(fun({_Name, WsPid, PlayerPid}, _Acc) ->
                player:getjson(PlayerPid, Pid),
                acc
            end, acc, players),

                                                % inform the player about existing blockers
  ets:foldl(fun({_,Blocker}, Acc) ->
                Pid ! {update, Blocker},
                acc
            end, acc, blockers),
  ThisPlayer.



handle_info({blocker, X, Y, W, H, Owner}, State) ->
  Update = jiffy:encode(#{ <<"action">> => <<"blockernew">>,
                           <<"params">> => #{
                                             <<"owner">> => Owner,
                                             <<"size">> => #{
                                                             <<"x">> => X,
                                                             <<"y">> => Y,
                                                             <<"w">> => W,
                                                             <<"h">> => H
                                                            } } }),
  ets:insert(blockers, {1, Update}),
  broadcast({update,Update}),
  {noreply, State};

handle_info(clear, State) ->
  broadcast( {update, jiffy:encode( #{ <<"action">> => <<"clear_chatlog">> } ) }),
  {noreply, State};

handle_info(eyes_open, State) ->
  broadcast( {update, jiffy:encode( #{ <<"action">> => <<"eyes_open">> } ) }),
  {noreply, State};

handle_info({eyes, Name}, State) ->
  Open = {update, jiffy:encode( #{ <<"action">> => <<"eyes_open">> } ) },
  Close = {update, jiffy:encode( #{ <<"action">> => <<"eyes_closed">> } ) },
  ets:foldl(fun({Found, Ws, _Player}, _Acc) when Found == Name ->
                Ws ! Open;
               ({Other, Ws, _Player}, _Acc) ->
                Ws ! Close
            end, acc, players),
  {noreply, State};

handle_info(restate, State) ->
  ets:delete(blockers),
  ets:new(blockers, [bag, named_table]),
  ets:foldl(fun({Id, Pid}, Acc) ->
                Pid ! leave
            end, acc, cards),
  setup_state(),
  {noreply, State};

handle_info(info, State) ->
  io:format("~w~n", [State]),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

handle_cast(_, State) ->
  {noreply, State}.
