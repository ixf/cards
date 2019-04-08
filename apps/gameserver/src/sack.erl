
-module(sack).

-export([new/2, move/2, getjson/2]).
-export([default/5, holding/6]).

new(Color, Pos) ->
  spawn(sack, laying, [self(), Color, Pos]).

move(Player, Where) -> Player ! {move, self(), Where}.
getjson(Player, WhoAsks) -> Player ! {get, WhoAsks}.  


makejson(Name, Color, Pos) ->
  {update, 
        jiffy:encode(#{ <<"action">> => <<"newsack">>,
        <<"params">> =>
                #{ <<"pos">> => Pos,
                    <<"name">> => Name,
                    <<"color">> => Color
                  }
        })}.

default(Room, PidWs, Name, Color, Pos) ->
  receive 
    {click, Id} ->
      [{Id, CardPid}] = ets:lookup(cards, Id),
      card:lift(CardPid, Name),
      receive 
        {lift, ok} -> holding(Room, PidWs, Name, Color, Pos, Id);
        {lift, error} -> default(Room, PidWs, Name, Color, Pos)
      end;
    {get, Pid} ->
      Pid ! makejson(Name, Color, Pos),
      default(Room, PidWs, Name, Color, Pos);
    {move, PidWs, Where} ->
      Update = jiffy:encode( #{<<"action">> => <<"playermoved">>,
                              <<"params">> => #{ <<"name">> => Name,
                                               <<"newpos">> => Where} } ),
      arbiter:broadcast({update, Update}),
      default(Room, PidWs, Name, Color, Where);
    leave -> ok;
    _ ->
      default(Room, PidWs, Name, Color, Pos)
  end.

holding(Room, PidWs, Name, Color, Pos, CardId) ->
  receive 
    {click, CardId} ->
      [{CardId, CardPid}] = ets:lookup(cards, CardId),
      card:drop(CardPid, Name),
      receive 
        {drop, ok} -> default(Room, PidWs, Name, Color, Pos);
        {drop, error} -> holding(Room, PidWs, Name, Color, Pos, CardId)
      end;
    {get, Pid} ->
      Pid ! makejson(Name, Color, Pos),
      holding(Room, PidWs, Name, Color, Pos, CardId);
    {move, PidWs, Where} ->
      Update = jiffy:encode( #{<<"action">> => <<"playermoved">>,
                              <<"params">> => #{ <<"name">> => Name,
                                               <<"newpos">> => Where} } ),
      arbiter:broadcast({update, Update}),
      [{CardId, CardPid}] = ets:lookup(cards, CardId),
      card:move(CardPid, Pos),
      holding(Room, PidWs, Name, Color, Where, CardId);
    {rotate, CardId} ->
      [{CardId, CardPid}] = ets:lookup(cards, CardId),
      card:rotate(CardPid),
      holding(Room, PidWs, Name, Color, Pos, CardId);
    leave ->
      [{CardId, CardPid}] = ets:lookup(cards, CardId),
      card:drop(CardPid, Name),
      ok;
    _ ->
      holding(Room, PidWs, Name, Color, Pos, CardId)
  end.
