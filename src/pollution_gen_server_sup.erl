%%%-------------------------------------------------------------------
%%% @author tom
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Apr 2019 11:35 AM
%%%-------------------------------------------------------------------
-module(pollution_gen_server_sup).
-author("tom").
-behaviour(supervisor).


%% API
-export([start/0, stop/0]).

start() -> register(gen_sup, spawn(fun() -> loop() end)).

stop() -> gen_sup ! stop.

loop() ->
  process_flag(trap_exit, true),
  pollution_gen_server:start_link([]),
  receive
    {'EXIT', Pid, Reason} -> io:format("crash detected ~w~n", [Reason]),
      loop();
    stop -> pollution_gen_server:terminate()
  end.


