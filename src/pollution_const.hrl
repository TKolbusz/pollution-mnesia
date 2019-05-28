%%%-------------------------------------------------------------------
%%% @author tom
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Apr 2019 10:23 AM
%%%-------------------------------------------------------------------
-author("tom").
-define(OK, 0).
-define(STATION_ALREADY_EXIST, 1).
-define(STATION_NOT_EXIST, 2).
-define(MEASUREMENT_ALREADY_EXIST, 3).
-define(MEASUREMENT_NOT_EXIST, 4).
-record(monitor, {stations}).
-record(station, {name, coords, measurements}).
-record(measurement, {datetime, type, value}).
-record(station_measurement, {station,measurement}).