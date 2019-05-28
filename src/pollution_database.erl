%%%-------------------------------------------------------------------
%%% @author tom
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. May 2019 10:21 PM
%%%-------------------------------------------------------------------
-module(pollution_database).
-author("tom").

%% API
-export([start/0, stop/0, insert_stations/1, read_stations/0, delete_stations/0, update_stations/1, insert_test_station/0]).
-include("pollution_const.hrl").

start() ->
  case mnesia:start() of
    {aborted, _} ->
      io:format("Creating database~n"),
      mnesia:create_schema([node()]),
      mnesia:start();
    ok ->
      io:format("Database already exists~n"),
      {atomic, ok}
  end,
  create_tables_if_not_exists().

create_tables_if_not_exists() ->
  case
    mnesia:create_table(station, [
      {attributes, record_info(fields, station)}])
  of
    {aborted, {already_exists, _}} ->
      io:format("Table station exists~n"),
      ok;
    {aborted, Error} ->
      io:format("Error ~p~n", [Error]);
      {atomic, ok}->ok
end .


stop() ->
  io:format("Stopping database~n"),
  mnesia:stop().

insert_stations([Station | Stations]) ->
%%  io:format("Inserting station ~s~n", [Station#station.name]),
  InsertFunction = fun() -> mnesia:write(Station) end,
  case mnesia:transaction(InsertFunction) of
    {atomic, _} -> insert_stations(Stations);
    {aborted, Reason} -> io:format("Inserting error ~s~n", [Reason])
  end;
insert_stations([]) -> ok.

delete_stations() -> mnesia:clear_table(station).
read_stations() ->
  CatchAll = [{'_', [], ['$_']}],
  SelectFun = fun() -> mnesia:select(station, CatchAll) end,
  {atomic, Stations} = mnesia:transaction(SelectFun),
  Stations.

update_stations(Stations) -> delete_stations(), insert_stations(Stations).


insert_test_station() ->
  Measurement1 = #measurement{type = "PM10", datetime = {{0, 0, 0}, {0, 0, 0}}, value = 10},
  Measurement2 = #measurement{type = "PM2,5", datetime = {{0, 0, 0}, {0, 0, 0}}, value = 7},
  Station = #station{name = "TEST", coords = {0, 0}, measurements = [Measurement1, Measurement2]},
  Station2 = #station{name = "TEST2", coords = {1, 1}, measurements = [Measurement1]},
  pollution_database:insert_stations([Station, Station2]).
