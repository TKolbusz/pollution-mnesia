%%%-------------------------------------------------------------------
%%% @author tom
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Apr 2019 10:09 AM
%%%-------------------------------------------------------------------
-module(pollution_server_sup).
-author("tom").
-behaviour(supervisor).


%% API
-export([start/0, stop/0]).

start() ->
  pollution_database:start(),
  register(sup, spawn(fun() -> loop() end)).

stop() -> sup ! stop,
  pollution_database:stop().

loop() ->
  process_flag(trap_exit, true),
  pollution_server_db:start(),
  receive
    {'EXIT', Pid, Reason} -> io:format("crash detected ~w~n", [Reason]),
      loop();
    stop -> pollution_server_db:stop()
  end.



