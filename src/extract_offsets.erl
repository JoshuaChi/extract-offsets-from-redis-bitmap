%-----------------------------------------------
% Suppose you understand how bitmaps works.This
% module is used to extract offsets from value 
% that redis stored in bitmap.
%
% @author Joshua Chi<joshokn@gmail.com>
%------------------------------------------------
-module(extract_offsets).
-export([loop_positions/2, start/1, parition_binary/5]).

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
      

loop_positions([], Result)->
  %io:format("#loop_positions# result: ~p~n", [Result]),
  Result;
loop_positions([[H, Position]|T], Result) ->
  %io:format("H:~p;P:~p:~n", [H, Position]),
  NewResult = case re:run(string:right(hd(io_lib:format("~.2B", [H])), 8, $0), "1", [global]) of
    {match, V} ->
      loop(V, Position, Result);
    nomatch ->
      Result
  end,
  loop_positions(T, NewResult).


%------------------------------------------------------
% Paritioning big binary into parts
%------------------------------------------------------
parition_binary(Result, <<>>, _Offset, _Limit, _Pointer) ->
  io:format("#parition_binary Result 1: ~p~n", [Result]),
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
  NewResult = case prepare_list(binary_to_list(PartBinary), Pointer, []) of
    [H|T] ->
      loop_positions([H|T], Result);
    _ ->
      []
  end,
  io:format("#NewResult: ~p;#Result:~p~n", [NewResult, Result]),
  NewOffset = Offset+NewLimit,
  parition_binary(lists:append(Result, NewResult), binary:part(Binary, NewOffset, BinarySize-NewOffset), 0, Limit, Pointer+NewOffset).

%------------------------------------------------------
% Input: <<1, 7, 0, 0, 9, 0>>; 
% Output: [[1, 0], [7, 1], [9, 4]]
%------------------------------------------------------
prepare_list([], InnerPosition, Result) ->
  Result;
prepare_list([H|T], InnerPosition, Result) ->
  case H of
    0 ->
      prepare_list(T, InnerPosition+1, Result);
    _ ->
      prepare_list(T, InnerPosition+1, lists:append(Result, [[H, InnerPosition]]))
  end.

  

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
