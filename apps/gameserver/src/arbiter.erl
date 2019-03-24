

-module(arbiter).

-behaviour(gen_server).

-export([start_link/0]).
-export([hello/2, bye/1, imoved/2, givestate/0, click/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
  gen_server:start_link({local, arbiter}, arbiter, [], []).

init(_Args) ->
  % gracz to mapowanie: name => { pozycja, kolor, From }
  % karta : id => size, img itp
  {ok, StateFile} = file:read_file("state.json"),
  InitialState = jiffy:decode( binary:bin_to_list( StateFile ), [return_maps]),
  {ok, InitialState}.

hello(Name, Color) ->
  gen_server:call(arbiter, {hello, Name, Color}).

givestate() ->
  gen_server:call(arbiter, givestate).

imoved(Name, Pos) ->
  gen_server:call(arbiter, {move, Name, Pos}).

bye(Name) ->
  gen_server:call(arbiter, {bye, Name}).

click(Name, Id) ->
  gen_server:call(arbiter, {click, Name, Id}).

handle_cast(_, State) ->
  {noreply, State}.


% sends to all but the sender ( or all, if sender is ok for example )
maybe_send(What, Pid, Sender, Sender) -> 
  ok;
maybe_send(What, Pid, Receiver, Sender) ->
  Pid ! {update,What}.

maybe_send_iter(What, none, Sender) -> ok;
maybe_send_iter(What, {K,V,Iter}, Sender) ->
  Pid = maps:get(<<"pid">>, V),
  maybe_send(What, Pid, K, Sender),
  maybe_send_iter(What, maps:next(Iter), Sender).


handle_call({hello, Name, Color}, {Pid, _Tag}, State) ->
  io:format("player joins: ~s~n", [Name]),
  #{<<"players">> := Players} = State,
  Players2 = Players#{ Name =>
		       #{ <<"pos">> => #{<<"x">> => 0, <<"y">> => 0},
			  <<"color">> => Color,
			  <<"cardheld">> => -1, % redundantne ale moze szybciej bedzie
			  <<"pid">> => Pid
			  }
		     },
  State2 = State#{ <<"players">> := Players2 },
  io:format("players now: ~w~n", [State2]),

  % update to other players
  Update = jiffy:encode(#{<<"action">> => <<"playerjoined">>, <<"params">> => #{ <<"name">> => Name, <<"color">> => Color}}),
  Iter = maps:iterator(Players),
  io:format("players is ~w~n", [Iter]),
  maybe_send_iter(Update, maps:next(Iter), ""),

  {reply, ok, State2};

handle_call({move, Name, Pos}, _From, State) ->
  #{<<"players">> := Players} = State,
  #{Name := Params} = Players,
  Players2 = Players#{ Name := Params#{ <<"pos">> := Pos } },
  State2 = State#{ <<"players">> := Players2 },

  Update = jiffy:encode(#{<<"action">> => <<"playermoved">>, <<"params">> => #{ <<"name">> => Name, <<"newpos">> => Pos}}),
  Iter = maps:iterator(Players2),
  maybe_send_iter(Update, maps:next(Iter), ""),

  % maybe update holds
  State3 = case maps:get(Name, maps:get(<<"holds">>, State), nope) of
	     nope -> State2;
	     Id -> #{ <<"cards">> := Cards } = State2,
		   #{ Id := Card } = Cards,
		   Card2 = Card#{ <<"pos">> := Pos },
		   Cards2 = Cards#{ Id := Card2 },
		   State2#{ <<"cards">> := Cards2 }
  end,
  {reply, ok, State3};


handle_call({click, Name, Id}, _From, State) ->
  #{ <<"holds">> := Holds } = State,
  Result = case maps:get(Name, Holds, niema) of
    niema -> % pick up (maybe)
      Find = fun(T) -> T == Id end,
      case lists:search(Find, maps:values(Holds)) of
	false -> pickup;
	_ -> fail
      end;
    Id -> release;
    HeldId -> fail 
  end,

  State2 = handle_click_result(Result, State, Name, Id),

  {reply, ok, State2};

handle_call(givestate, _From, State) ->
  % usuwamy pid z graczy
  % po nie da sie zamienic na json łatwo
  % robimy tylko raz, przy givestate.
  % czyli na razie ok jezeli jest wolne/głupie
  #{ <<"players">> := Players } = State,
  Fun = fun(_K,Params) -> maps:remove(<<"pid">>, Params) end,
  State2 = State#{ <<"players">> := maps:map(Fun,Players) },
  {reply, {state, State2}, State};

handle_call({bye, Name}, _From, State) ->
  io:format("player left: ~s~n", [Name]),
  #{<<"players">> := Players} = State,
  State2 = State#{ <<"players">> := maps:remove(Name, Players) },
  io:format("players now: ~w~n", [State2]),
  {reply, ok, State2}.



handle_info(info, State) ->
  io:format("~w~n", [State]),
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

handle_click_result(fail, State, _Name, _Id) ->
  io:format("fail lol~n"),
  State;
handle_click_result(pickup, State, Name, Id) -> 
  io:format("pickup~n"),
  Update = jiffy:encode(#{<<"action">> => <<"cardup">>, <<"params">> => #{ <<"name">> => Name, <<"id">> => Id}}),
  Iter = maps:iterator(maps:get(<<"players">>, State)),
  maybe_send_iter(Update, maps:next(Iter), ""),

  #{ <<"holds">> := Holds } = State,
  Holds2 = Holds#{ Name => Id },
  State#{ <<"holds">> := Holds2 };

handle_click_result(release, State, Name, Id) -> 
  io:format("release~n"),
  Update = jiffy:encode(#{<<"action">> => <<"carddown">>, <<"params">> => #{ <<"name">> => Name, <<"id">> => Id}}),
  Iter = maps:iterator(maps:get(<<"players">>, State)),
  maybe_send_iter(Update, maps:next(Iter), ""),

  #{ <<"holds">> := Holds } = State,
  Holds2 = maps:remove(Name, Holds),
  State#{ <<"holds">> := Holds2 }.


