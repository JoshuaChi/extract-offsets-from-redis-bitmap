%-----------------------------------------------
% Suppose you understand how bitmaps works.This
% module is used to extract offsets from value 
% that redis stored in bitmap.
%
% @author Joshua Chi<joshokn@gmail.com>
%------------------------------------------------
-module(extract_offsets).
-export([loop_positions/3, start/1, parition_binary/4]).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0]).

-define(THRESHOLD, 1000).
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
loop_positions([H|T], Position, Result) ->
  NewResult = case re:run(string:right(hd(io_lib:format("~.2B", [H])), 8, $0), "1", [global]) of
    {match, V} ->
      loop(V, Position, Result);
    nomatch ->
      Result
  end,
  loop_positions(T, Position+1, NewResult).

parition_binary(Result, <<>>, _Offset, _Limit) ->
  Result;
parition_binary(Result, Binary, _Offset, _Limit) when byte_size(Binary) < ?THRESHOLD ->
  Result = case [X || X <- binary_to_list(Binary), X =/=0] of
    [H|T] ->
      loop_positions([H|T], 0, []);
    _ ->
      []
  end,
  Result;
parition_binary(Result, Binary, Offset, Limit) when Offset =< byte_size(Binary) ->
  io:format("Size:~p; Offset: ~p Limit: ~p~n", [byte_size(Binary), Offset, Limit]),
  NewLimit = case Offset+Limit > byte_size(Binary) of
    true ->
       byte_size(Binary) - Offset;
    false ->
      Limit
  end,
  PartBinary = binary:part(Binary,{Offset, NewLimit}),
  NewResult = case [X || X <- binary_to_list(PartBinary), X =/=0] of
    [H|T] ->
      loop_positions([H|T], 0, []);
    _ ->
      []
  end,
  NewOffset = Offset+NewLimit,
  io:format("NEwSize: ~pNewOFfset:~p NewLimit:~p~n", [byte_size(Binary),NewOffset, NewLimit]),
  parition_binary(lists:append(Result, NewResult), binary:part(Binary,{NewOffset, Limit}), 0, Limit).



%Input - formats: <<"U">>, <<1,80>>
start(Input) ->
  Server_Process_ID = spawn(extract_offsets, start_link, []),
  parition_binary([], Input, 0, ?THRESHOLD).
%  binary:part(Input,{10, 1000}).
%  Result = case [X || X <- binary_to_list(Input), X =/=0] of
%    [H|T] ->
%      loop_positions([H|T], 0, []);
%    _ ->
%      []
%  end,
%  Result.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    gen_server:call(?MODULE, stop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                              gen_server                                  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

init([]) ->
    {ok, []}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(Msg, _From, State) ->
    lager:warning("Unknown call to ~p: ~p", [?MODULE, Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    lager:warning("Unknown cast to ~p: ~p", [?MODULE, Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    lager:warning("Unknown info to ~p: ~p", [?MODULE, Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    lager:info("Terminating ~p", [?MODULE]),
    ok.

code_change(OldVsn, State, _Extra) -> 
  lager:info("Code Change OldVsn:~p~n", [OldVsn]),
  {ok, State}.

