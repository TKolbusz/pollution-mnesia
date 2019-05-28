%%%-------------------------------------------------------------------
%%% @author tom
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Mar 2019 11:06 PM
%%%-------------------------------------------------------------------
-module(pollution).
-author("tom").

%% API
-export([createMonitor/0,createMonitor/1,addStation/3,addValue/5,removeValue/4,getOneValue/4,getDailyMean/3,getMinMaxValue/4,getStationMean/3,test/0]).

-include("pollution_const.hrl").


createMonitor() -> #monitor{stations = []}.
createMonitor(Stations) -> #monitor{stations = Stations}.
addStation(Monitor, StationName, Coords) ->
  case lists:any(fun(#station{name = N, coords = C}) ->
    (N == StationName) or (C == Coords) end, Monitor#monitor.stations) of
    true -> {Monitor, ?STATION_ALREADY_EXIST};
    false ->
      {#monitor{stations = [#station{name = StationName, coords = Coords, measurements = []} | Monitor#monitor.stations]}, ?OK}
  end.

addValue(Monitor, StationNameOrCoords, DateTime, MeasurementType, Value) ->
  case getStation(StationNameOrCoords, Monitor) of
    {_, ?STATION_NOT_EXIST} -> {Monitor, ?STATION_NOT_EXIST};
    {Station, ?OK} ->
      OldMeasurements = Station#station.measurements,
      case getMeasurement(DateTime, MeasurementType, OldMeasurements) of
        {Measurement, ?OK} -> {Monitor, ?MEASUREMENT_ALREADY_EXIST};
        {_, ?MEASUREMENT_NOT_EXIST} ->
          UpdatedStation = Station#station{measurements = [#measurement{datetime = DateTime, type = MeasurementType, value = Value} | OldMeasurements]},
          StationsWithoutUpdatedStation = lists:filter(fun(S) ->
            not (S =:= Station) end, Monitor#monitor.stations),
          {#monitor{stations = [UpdatedStation | StationsWithoutUpdatedStation]}, ?OK}
      end
  end.
removeValue(Monitor, StationNameOrCoords, DateTime, MeasurementType) ->
  case getStation(StationNameOrCoords, Monitor) of
    {_, ?STATION_NOT_EXIST} -> {Monitor, ?STATION_NOT_EXIST};
    {Station, ?OK} ->
      case getMeasurement(DateTime, MeasurementType, Station#station.measurements) of
        {_, ?MEASUREMENT_NOT_EXIST} -> {Monitor, ?OK};
        {Measurement, ?OK} ->
          MeasurementsWithoutDeleted = lists:filter(fun(M) ->
            not (M =:= Measurement) end, Station#station.measurements),
          UpdatedStation = Station#station{measurements = MeasurementsWithoutDeleted},
          StationsWithoutUpdatedStation = lists:filter(fun(S) ->
            not (S =:= Station) end, Monitor#monitor.stations),
          {#monitor{stations = [UpdatedStation | StationsWithoutUpdatedStation]}, ?OK}
      end
  end.


getOneValue(Monitor, MeasurementType, DateTime, StationNameOrCoords) ->
  case getStation(StationNameOrCoords, Monitor) of
    {_, ?STATION_NOT_EXIST} -> {Monitor, ?STATION_NOT_EXIST};
    {Station, ?OK} ->
      case getMeasurement(DateTime, MeasurementType, Station#station.measurements) of
        {M, ?OK} -> {M#measurement.value, ?OK};
        {_, ?MEASUREMENT_NOT_EXIST} -> {null, ?MEASUREMENT_NOT_EXIST}
      end
  end.

getStationMean(Monitor, MeasurementType,StationNameOrCoords) ->
  case getStation(StationNameOrCoords, Monitor) of
    {_, ?STATION_NOT_EXIST} -> {Monitor, ?STATION_NOT_EXIST};
    {Station, ?OK} ->
      Filtered = lists:filter(fun(#measurement{type = T}) ->
        T == MeasurementType end, Station#station.measurements),
      Values = lists:map(fun(#measurement{value = V}) -> V end, Filtered),
      {mean(Values),?OK}
  end.

mean(X) ->
  mean(X, 0, 0).

mean([H | T], Length, Sum) ->
  mean(T, Length + 1, Sum + H);
mean([], Length, Sum) ->
  case Length of
    0 -> 0;
    _ -> Sum/Length
  end.


getDailyMean(Monitor, MeasurementType, Date) ->
  FilteredByDateAndType = getMeasurementsFilteredByDateAndType(Monitor#monitor.stations, MeasurementType, Date),
  Values = getValuesFromMeasurements(FilteredByDateAndType),
  {mean(Values),?OK}.

getMinMaxValue(Monitor, MeasurementType, Date, Coords) ->
  case getStationByCoordinates(Coords, Monitor#monitor.stations) of
    {_, ?STATION_NOT_EXIST} -> {null, ?STATION_NOT_EXIST};
    {Station, ?OK} ->
      FilteredByDateAndType = getMeasurementsFilteredByDateAndType([Station], MeasurementType, Date),
      Values = getValuesFromMeasurements(FilteredByDateAndType),
      case Values of
        [] -> {{0,0},?OK};
        _ -> {{lists:min(Values), lists:max(Values)}, ?OK}
      end
  end.

getMeasurementsFilteredByDateAndType(Stations, MeasurementType, Date) ->
  AllMeasurements = lists:flatmap(fun(#station{measurements = M}) -> M end, Stations),
  lists:filter(fun(#measurement{type = T, datetime = {D, _}}) ->
    (T == MeasurementType) and (Date == D) end, AllMeasurements).

getValuesFromMeasurements(Measurements) ->
  lists:map(fun(#measurement{value = V}) -> V end, Measurements).
getMeasurement(DateTime, MeasurementType, [M = #measurement{datetime = DateTime, type = MeasurementType} | _]) ->
  {M, ?OK};
getMeasurement(DateTime, MeasurementType, [_ | OtherMeasurements]) ->
  getMeasurement(DateTime, MeasurementType, OtherMeasurements);
getMeasurement(_, _, []) -> {null, ?MEASUREMENT_NOT_EXIST}.

getStation({X, Y}, Monitor) -> getStationByCoordinates({X, Y}, Monitor#monitor.stations);
getStation(Name, Monitor) -> getStationByName(Name, Monitor#monitor.stations).

getStationByCoordinates({X, Y}, [S = #station{coords = {X, Y}} | _]) -> {S, ?OK};
getStationByCoordinates({X, Y}, [_ | OtherStations]) ->
  getStationByCoordinates({X, Y}, OtherStations);
getStationByCoordinates({_, _}, []) -> {null, ?STATION_NOT_EXIST}.

getStationByName(Name, [S = #station{name = Name} | _]) -> {S, ?OK};
getStationByName(Name, [_ | OtherStations]) -> getStationByName(Name, OtherStations);
getStationByName(_, []) -> {null, ?STATION_NOT_EXIST}.

test() -> P = createMonitor(),
  {P1, _} = addStation(P, "TEST", {0, 0}),
  {P2, _} = addStation(P1, "TEST2", {1, 1}),
  {P3, _} = addValue(P2, "TEST", {{1, 2, 3}, {4, 5, 6}}, "PM10", 0),
  {P4, _} = addValue(P3, "TEST", {{1, 2, 3}, {5, 5, 6}}, "PM10", 1),%removed
  {P5, _} = addValue(P4, "TEST", {{1, 2, 3}, {6, 5, 6}}, "PM10", 2),
  {P6, _} = addValue(P5, {0, 0}, {{1, 2, 4}, {7, 5, 6}}, "PM10", 3),
  {P7, _} = addValue(P6, {0, 0}, {{12, 22, 32}, {42, 52, 62}}, "PM2,5", 12),
  {P8, _} = addValue(P7, "TEST2", {{1, 2, 3}, {55, 66, 77}}, "PM10", -1),
  {P9, _} = removeValue(P8, "TEST", {{1, 2, 3}, {5, 5, 6}}, "PM10"),
  {Station, _} = getStation("TEST", P9),
  Value = getOneValue(P9, "PM10", {{1, 2, 3}, {4, 5, 6}}, Station),
  StationMean = getStationMean(P9, Station, "PM10"),
  DailyMean = getDailyMean(P9, "PM10", {1, 2, 3}),
  MinMax = getMinMaxValue(P9, "PM10", {1, 2, 3}, {0, 0}),
  StationMean.
