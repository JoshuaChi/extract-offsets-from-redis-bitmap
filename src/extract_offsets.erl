%-----------------------------------------------
% Suppose you understand how bitmaps works.This
% module is used to extract offsets from value 
% that redis stored in bitmap.
%
% @author Joshua Chi<joshokn@gmail.com>
%------------------------------------------------
-module(extract_offsets).
-export([loop_positions/3, start/1, parition_binary/5]).

-define(THRESHOLD, 1000).
-define(ENABLE_PROFILE, 0).
-define(ENABLE_STATISTICS, 1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              Public API                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

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
loop_positions([H|T], Position, Result) when H =:= 0 ->
  loop_positions(T, Position+1, Result);
loop_positions([H|T], Position, Result) when H =/= 0 ->
  NewResult = case re:run(string:right(hd(io_lib:format("~.2B", [H])), 8, $0), "1", [global]) of
    {match, V} ->
      loop(V, Position, Result);
    nomatch ->
      Result
  end,
  loop_positions(T, Position+1, NewResult).

parition_binary(Result, <<>>, _Offset, _Limit, _Pointer) ->
  Result;
parition_binary(Result, Binary, Offset, Limit, Pointer) when Offset =< byte_size(Binary) ->
  BinarySize = byte_size(Binary),
  NewLimit = case Offset + Limit > BinarySize of
    true ->
       BinarySize - Offset;
    false ->
      Limit
  end,
  PartBinary = binary:part(Binary,{Offset, NewLimit}),
  %io:format("Header: ~p~n",[PartBinary]),
  NewResult = case [X || X <- binary_to_list(PartBinary)] of
    [H|T] ->
      loop_positions([H|T], Pointer, Result);
    _ ->
      []
  end,
  NewOffset = Offset+NewLimit,
  parition_binary(lists:append(Result, NewResult), binary:part(Binary, NewOffset, BinarySize-NewOffset), 0, Limit, Pointer+NewOffset).



%Input - formats: <<"U">>, <<1,80>>
start(Input) ->
  %Server_Process_ID = spawn(extract_offsets, start_link, []),
  case ?ENABLE_PROFILE =:= 1 of
    true ->
      eprof:start(),
      eprof:start_profiling([self()]);
    false ->
      nothing_to_do
  end,
  case ?ENABLE_STATISTICS =:= 1 of
    true ->
      statistics(runtime),
      statistics(wall_clock);
    false ->
      nothing_to_do
  end,
  Result = parition_binary([], Input, 0, ?THRESHOLD, 0),
  %io:format("Result: ~p~n", [Result]),
  case ?ENABLE_STATISTICS =:= 1 of
    true ->
      {_, CpuTime} = statistics(runtime),
      {_,ElapsedTime}=statistics(wall_clock),
      io:format("Cpu Time: ~p microseconds; Elapsed Time: ~p microseconds~n", [CpuTime* 1000, ElapsedTime*1000]);
    false ->
      nothing_to_do
  end,
  case ?ENABLE_PROFILE =:= 1 of
    true ->
      eprof:stop_profiling(),
      eprof:analyze(total);
    false ->
      nothing_to_do
  end,
  Result.
