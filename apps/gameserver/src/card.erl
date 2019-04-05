

-module(card).

-export([new/2, new/3, move/2, lift/2, drop/2, rotate/1, getjson/2]).
-export([laying/3, moving/4]).


new(Id, Map) ->
  Cat = maps:get(<<"cat">>, Map),
  Pos = maps:get(<<"pos">>, Map),
  Size = maps:get(<<"size">>, Map),
  Img = maps:get(<<"img">>, Map),
  Back = maps:get(<<"back">>, Map),
  Side  = maps:get(<<"side">>, Map),
  new({Id, Cat, Size, Img, Back}, Pos, Side).

new(Attrs, Pos, Side) ->
  spawn(card, laying, [Attrs, Pos, Side]).

% WhoAsks jest zamiast self() bo bedzie to moze robic room zamist
% procesu gracza
getjson(Card, WhoAsks) -> Card ! {get, WhoAsks}.  

move(Card, Where) -> Card ! {move, self(), Where}.
lift(Card, Name) -> Card ! {lift, self(), Name}.
drop(Card, Name) -> Card ! {drop, self(), Name}.
rotate(Card) -> Card ! {rotate, self()}.
invert(1) -> 0;
invert(_) -> 1.





makejson({Id,Cat,Size,Img,Back},Pos,Side,State) ->
  {update, jiffy:encode( #{ <<"action">> => <<"newcard">>,
  <<"params">> => #{
       <<"id">> => Id,
		   <<"cat">> => Cat,
		   <<"pos">> => Pos,
		   <<"size">> => Size,
		   <<"img">> => Img,
		   <<"back">> => Back,
		   <<"side">> => Side,
		   <<"state">> => State
      }
     }
    )}.

laying(Attrs={Id,_,_,_,_}, Pos, Side) ->
  receive 
    {get, Pid} ->
      Pid ! makejson(Attrs, Pos, Side, 0),
      laying(Attrs,Pos,Side);
    {lift, Pid, Name} ->
      Pid ! {lift, ok},
      Update = jiffy:encode( #{ <<"action">> => <<"cardup">>,
			      <<"params">> => #{ <<"id">> => Id,
					       <<"name">> => Name }} ),
      arbiter:broadcast({update, Update}),
      moving(Attrs, Pos, Side, Pid);
    leave ->
      Update = jiffy:encode( #{ <<"action">> => <<"carddel">>,
				<<"params">> => #{ <<"id">> => Id }} ),
      arbiter:broadcast({update, Update}),
      ok;
    {Action, Pid} ->
      Pid ! {Action, error},
      laying(Attrs,Pos,Side);
    {Action, Pid, _} ->
      Pid ! {Action, error},
      laying(Attrs,Pos,Side)
  end.

moving(Attrs={Id,_,_,_,_},Pos,Side,Holder) ->
  receive 
    {get, Pid} ->
      Pid ! makejson(Attrs, Pos, Side, 1),
      moving(Attrs, Pos, Side, Holder);

    {move, Holder, NewPos} ->

      Update = jiffy:encode( #{ <<"action">> => <<"cardmove">>,
			      <<"params">> => #{ <<"id">> => Id,
					       <<"pos">> => NewPos }} ),
      arbiter:broadcast({update, Update}),

      moving(Attrs, NewPos, Side, Holder);

    {drop, Pid, Name} ->
      Pid ! {drop, ok},
      Update = jiffy:encode( #{ <<"action">> => <<"carddown">>,
			      <<"params">> => #{ <<"id">> => Id,
					       <<"name">> => Name }} ),
      arbiter:broadcast({update, Update}),
      laying(Attrs, Pos, Side);

    {rotate, Holder} ->

      Update = jiffy:encode( #{ <<"action">> => <<"rotate">>,
			      <<"params">> => #{ <<"id">> => Id }} ),
      arbiter:broadcast({update, Update}),
      moving(Attrs, Pos, invert(Side), Holder);

    leave ->
      Update = jiffy:encode( #{ <<"action">> => <<"carddel">>,
				<<"params">> => #{ <<"id">> => Id }} ),
      arbiter:broadcast({update, Update}),
      ok;

    {Action, Pid, _} ->
      Pid ! {Action, error},
      moving(Attrs,Pos, Side, Holder);
    {Action, Pid} ->
      Pid ! {Action, error},
      moving(Attrs,Pos, Side, Holder)
  end.

