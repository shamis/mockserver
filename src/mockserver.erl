-module(mockserver).

-behaviour(application).

-export([start/1, stop/0, restart/1]).
%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, StartArgs) ->
    mockserver_sup:start_link(StartArgs).

stop(_State) ->
    ok.

start(Port) ->
	start(normal, Port).

stop() ->
	exit(whereis(mockserver_sup), normal),
	stop(normal).

restart(Port) ->
	stop(),
	timer:sleep(10),
	start(Port).

