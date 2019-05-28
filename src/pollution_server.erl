%%%-------------------------------------------------------------------
%%% @author tom
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2019 8:55 AM
%%%-------------------------------------------------------------------


%% todo response z servera


-module(pollution_server).
-author("tom").
-include("pollution_const.hrl").

%% API
-export([start/0,start/1, stop/0, crash/0, addValue/4, addStation/2, removeValue/3, getMinMaxValue/3, getDailyMean/2, getStationMean/2, getOneValue/3]).

start() -> register(server, spawn_link(fun() -> init([]) end)).
start(Stations) -> register(server, spawn_link(fun() -> init(Stations) end)).

init(Stations) -> loop(#monitor{stations = Stations}).

stop() -> server ! stopServer.


loop(Monitor) ->
  receive
    {Pid, crash, Arguments} ->
      A = 1 / 0,
      io:format("fail ~w~n", [A]);
    {Pid, Function, Arguments} when Function == addStation
      orelse Function == addValue
      orelse Function == removeValue ->

      case apply(pollution, Function, [Monitor | Arguments]) of
        {NewM, ?OK} ->
          Pid ! {ok,NewM},
          loop(NewM);
        {_, Error} ->
          Pid ! Error,
          loop(Monitor)
      end;
    {Pid, Function, Arguments} when Function == getOneValue
      orelse Function == getStationMean
      orelse Function == getDailyMean
      orelse Function == getMinMaxValue ->

      P = apply(pollution, Function, [Monitor | Arguments]),
      case P of
        {Result, ?OK} ->
          Pid ! {value, Result};
        {_, Error} ->
          Pid ! Error
      end,
      loop(Monitor);
    stopServer ->
      io:format("OK~n")
  end.

call(Command, Args) ->
  server ! {self(), Command, Args},
  receive
    {ok,Monitor} -> {ok,Monitor};
    {value, Value} -> Value;
    Error -> io:fwrite("Error code: ~2.2.0w~n", [Error])
  end.

addStation(StationName, {X, Y}) -> call(addStation, [StationName, {X, Y}]).
addValue(Station, Datetime, Type, Value) -> call(addValue, [Station, Datetime, Type, Value]).
removeValue(Station, Datetime, Type) -> call(removeValue, [Station, Datetime, Type]).
getOneValue(StationNameOrCoords, Datetime, Type) ->
  call(getOneValue, [Type, Datetime, StationNameOrCoords]).
getStationMean(StationNameOrCoords, Type) -> call(getStationMean, [Type, StationNameOrCoords]).
getDailyMean(Type, Date) -> call(getDailyMean, [Type, Date]).
getMinMaxValue(MeasurementType, Date, Coords) ->
  call(getMinMaxValue, [MeasurementType, Date, Coords]).

crash() -> server ! {self(), crash, []}.