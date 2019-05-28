%%%-------------------------------------------------------------------
%%% @author tom
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. May 2019 9:48 AM
%%%-------------------------------------------------------------------
-module(pollution_server_db).
-author("tom").
-include("pollution_const.hrl").

%% API
-export([start/0, stop/0, crash/0, addValue/4, addStation/2, removeValue/3, getMinMaxValue/3, getDailyMean/2, getStationMean/2, getOneValue/3]).

start() ->
  OldStations = pollution_database:read_stations(),
  pollution_server:start(OldStations).

stop() -> pollution_server:stop(),
  pollution_database:delete_stations().

call(Lambda) ->
  case Lambda() of
    {ok, Monitor} ->
      pollution_database:update_stations(Monitor#monitor.stations),
      ok;
    X -> X
  end.


addStation(StationName, {X, Y}) ->
  call(fun() -> pollution_server:addStation(StationName, {X, Y}) end).
addValue(Station, Datetime, Type, Value) ->
  call(fun() -> pollution_server:addValue(Station, Datetime, Type, Value) end).
removeValue(Station, Datetime, Type) ->
  call(fun() -> pollution_server:removeValue(Station, Datetime, Type) end).
getOneValue(StationNameOrCoords, Datetime, Type) -> pollution_server:getOneValue(StationNameOrCoords,Datetime, Type).
getStationMean(StationNameOrCoords, Type) -> pollution_server:getStationMean(StationNameOrCoords, Type).
getDailyMean(Type, Date) -> pollution_server:getDailyMean(Type, Date).
getMinMaxValue(MeasurementType, Date, Coords) -> pollution_server:getMinMaxValue(MeasurementType, Date, Coords).
crash() -> pollution_server:crash().


