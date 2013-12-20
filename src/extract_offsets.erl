%-----------------------------------------------
% Supposed you understand how bitmaps works.This
% module is used to extract offsets from value redis
% stored in bitmap.
% @author Joshua Chi<joshokn@gmail.com>
%------------------------------------------------
-module(extract_offsets).
-export([loop_positions/3, start/1]).

list_offsets(Value, Position) ->
  Value + Position * 8.

loop([], _Position, Result) ->
  Result;
loop([H|T], Position, Result) ->
  NewResult = case H of
    [{V,_}] -> 
      lists:append(Result, [list_offsets(V, Position)]);
    _ -> 
      Result
  end,
  loop(T, Position, NewResult).
      

loop_positions([], _Position, Result)->
  Result;
loop_positions([H|T], Position, Result) ->
  NewResult = case re:run(string:right(hd(io_lib:format("~.2B", [H])), 8, $0), "1", [global]) of
    {match, V} ->
      loop(V, Position, Result);
    nomatch ->
      Result
  end,
  loop_positions(T, Position+1, NewResult).

%Input - formats: <<"U">>, <<1,80>>
start(Input) ->
  Result = case binary_to_list(Input) of
    [H|T] ->
      loop_positions([H|T], 0, []);
    _ ->
      []
  end,
  Result.
