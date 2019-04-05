

-module(card).

-export([new/2, new/6, move/2, lift/2, drop/2, rotate/1, getjson/2]).
-export([laying/7, moving/8]).


new(Id, Map) ->
  Pos = maps:get(<<"pos">>, Map),
  Size = maps:get(<<"size">>, Map),
  Img = maps:get(<<"img">>, Map),
  Back = maps:get(<<"back">>, Map),
  Side  = maps:get(<<"side">>, Map),
  new(Id, Pos, Size, Img, Back, Side).

new(Id, Pos, Size, Img, Back, Side) ->
  spawn(card, laying, [self(), Id,Pos,Size,Img, Back, Side]).

% WhoAsks jest zamiast self() bo bedzie to moze robic room zamist
% procesu gracza
getjson(Card, WhoAsks) -> Card ! {get, WhoAsks}.  

move(Card, Where) -> Card ! {move, self(), Where}.
lift(Card, Name) -> Card ! {lift, self(), Name}.
drop(Card, Name) -> Card ! {drop, self(), Name}.
rotate(Card) -> Card ! {rotate, self()}.
invert(1) -> 0;
invert(_) -> 1.





makejson(Id,Pos,Size,Img,Back,Side,State) ->
  {update, jiffy:encode( #{ <<"action">> => <<"newcard">>,
  <<"params">> => #{
       <<"id">> => Id,
		   <<"pos">> => Pos,
		   <<"size">> => Size,
		   <<"img">> => Img,
		   <<"back">> => Back,
		   <<"side">> => Side,
		   <<"state">> => State
      }
     }
    )}.

laying(Room,Id,Pos,Size,Img,Back,Side) ->
  receive 
    {get, Pid} ->
      Pid ! makejson(Id, Pos, Size, Img, Back, Side, 0),
      laying(Room,Id,Pos,Size,Img,Back,Side);
    {lift, Pid, Name} ->
      Pid ! {lift, ok},
      Update = jiffy:encode( #{ <<"action">> => <<"cardup">>,
			      <<"params">> => #{ <<"id">> => Id,
					       <<"name">> => Name }} ),
      arbiter:broadcast({update, Update}),
      moving(Room,Id,Pos,Size,Img,Back,Side, Pid);
    leave ->
      Update = jiffy:encode( #{ <<"action">> => <<"carddel">>,
				<<"params">> => #{ <<"id">> => Id }} ),
      arbiter:broadcast({update, Update}),
      ok;
    {Action, Pid} ->
      Pid ! {Action, error},
      laying(Room,Id,Pos,Size,Img,Back,Side);
    {Action, Pid, _} ->
      Pid ! {Action, error},
      laying(Room,Id,Pos,Size,Img,Back,Side)
  end.

moving(Room,Id,Pos,Size,Img,Back,Side,Holder) ->
  receive 
    {get, Pid} ->
      Pid ! makejson(Id, Pos, Size, Img, Back, Side, 1),
      moving(Room,Id,Pos,Size,Img,Back,Side, Holder);

    {move, Holder, NewPos} ->

      Update = jiffy:encode( #{ <<"action">> => <<"cardmove">>,
			      <<"params">> => #{ <<"id">> => Id,
					       <<"pos">> => NewPos }} ),
      arbiter:broadcast({update, Update}),

      moving(Room,Id,NewPos,Size,Img,Back,Side, Holder);

    {drop, Pid, Name} ->
      Pid ! {drop, ok},
      Update = jiffy:encode( #{ <<"action">> => <<"carddown">>,
			      <<"params">> => #{ <<"id">> => Id,
					       <<"name">> => Name }} ),
      arbiter:broadcast({update, Update}),
      laying(Room,Id,Pos,Size,Img,Back,Side);

    {rotate, Holder} ->

      Update = jiffy:encode( #{ <<"action">> => <<"rotate">>,
			      <<"params">> => #{ <<"id">> => Id }} ),
      arbiter:broadcast({update, Update}),
      moving(Room,Id,Pos,Size,Img,Back,invert(Side),Holder);

    leave ->
      Update = jiffy:encode( #{ <<"action">> => <<"carddel">>,
				<<"params">> => #{ <<"id">> => Id }} ),
      arbiter:broadcast({update, Update}),
      ok;

    {Action, Pid, _} ->
      Pid ! {Action, error},
      moving(Room,Id,Pos,Size,Img,Back,Side, Holder);
    {Action, Pid} ->
      Pid ! {Action, error},
      moving(Room,Id,Pos,Size,Img,Back,Side, Holder)
  end.

