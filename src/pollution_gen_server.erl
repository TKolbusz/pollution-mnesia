%%%-------------------------------------------------------------------
%%% @author tom
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Apr 2019 10:45 AM
%%%-------------------------------------------------------------------

-module(pollution_gen_server).
-author("tom").
-include("pollution_const.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1, init/1, handle_call/3, terminate/0, handle_cast/2]).
-export([crash/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getMinMaxValue/3]).


start_link(_) ->
  gen_server:start_link({local, pollution_gen_server}, pollution_gen_server, 0, []).

init(_) -> {ok, pollution:createMonitor()}.
addStation(Name, {X, Y}) ->
  gen_server:call(pollution_gen_server, {addStation, {Name, {X, Y}}}).

addValue(StationNameOrCoords, DateTime, MeasurementType, Value) ->
  gen_server:call(pollution_gen_server, {addValue, {StationNameOrCoords, DateTime, MeasurementType, Value}}).

removeValue(StationNameOrCoords, DateTime, MeasurementType) ->
  gen_server:call(pollution_gen_server, {removeValue, {StationNameOrCoords, DateTime, MeasurementType}}).

getOneValue(MeasurementType, DateTime, StationNameOrCoords) ->
  gen_server:call(pollution_gen_server, {getOneValue, {MeasurementType, DateTime, StationNameOrCoords}}).

getStationMean(StationNameOrCoords,MeasurementType ) ->
  gen_server:call(pollution_gen_server, {getStationMean, {MeasurementType, StationNameOrCoords}}).

getDailyMean(MeasurementType, Date) ->
  gen_server:call(pollution_gen_server, {getDailyMean, {MeasurementType, Date}}).

getMinMaxValue(MeasurementType, Date, Coords) ->
  gen_server:call(pollution_gen_server, {getMinMaxValue, {MeasurementType, Date, Coords}}).

crash() -> gen_server:call(pollution_gen_server, {crash, []}).

handle_call({addStation, Args}, _From, Monitor) ->
  {Name, Coords} = Args,
  Response = pollution:addStation(Monitor, Name, Coords),
  case Response of
    {M, ?OK} -> {reply, ok, M};
    {_,Error} -> {reply, formError(Error), Monitor}
  end;

handle_call({addValue, Args}, _From, Monitor) ->
  {StationNameOrCoords, DateTime, MeasurementType, Value} = Args,
  Response = pollution:addValue(Monitor, StationNameOrCoords, DateTime, MeasurementType, Value),
  case Response of
    {M, ?OK} -> {reply, ok, M};
    {_,Error} -> {reply, formError(Error), Monitor}
  end;

handle_call({removeValue, Args}, _From, Monitor) ->
  {StationNameOrCoords, DateTime, MeasurementType} = Args,
  case pollution:removeValue(Monitor, StationNameOrCoords, DateTime, MeasurementType) of
    {M, ?OK} -> {reply, ok, M};
    {_,Error} -> {reply, formError(Error), Monitor}
  end;

handle_call({getOneValue, Args}, _From, Monitor) ->
  {MeasurementType, DateTime, StationNameOrCoords} = Args,
  case pollution:getOneValue(Monitor, MeasurementType, DateTime, StationNameOrCoords) of
    {Result, ?OK} ->
      {reply, Result, Monitor};
    {_, Error} ->
      {reply, formError(Error), Monitor}
  end;


handle_call({getStationMean, Args}, _From, Monitor) ->
  {MeasurementType, StationNameOrCoords} = Args,
  case pollution:getStationMean(Monitor, MeasurementType, StationNameOrCoords) of
    {Result, ?OK} ->
      {reply, Result, Monitor};
    {_, Error} ->
      {reply, formError(Error), Monitor}
  end;

handle_call({getDailyMean, Args}, _From, Monitor) ->
  {MeasurementType, Date} = Args,
  case pollution:getDailyMean(Monitor, MeasurementType, Date) of
    {Result, ?OK} ->
      {reply, Result, Monitor};
    {_, Error} ->
      {reply, formError(Error), Monitor}
  end;
handle_call({getMinMaxValue, Args}, _From, Monitor) ->
  {MeasurementType, Date, Coords} = Args,
  case pollution:getMinMaxValue(Monitor, MeasurementType, Date, Coords) of
    {Result, ?OK} ->
      {reply, Result, Monitor};
    {_, Error} ->
      {reply, formError(Error), Monitor}
  end;

handle_call({crash, Args}, _From, Monitor) -> {reply, 1 / 0, Monitor}.

terminate() -> gen_server:stop(pollution_gen_server).


formError(Error) ->   io:fwrite("Error code: ~2.2.0w~n", [Error]).


handle_cast(Request, State) ->
  erlang:error(not_implemented).