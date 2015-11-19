-module(mockserver).

-behaviour(application).

-export([start/1, stop/0, restart/1]).
%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, StartArgs) ->
	io:format("StartArgs : ~p~n", [StartArgs]),
    mockserver_sup:start_link(StartArgs).

start(Port) ->
	start(normal, Port).

stop() ->
	stop(normal).

restart(Port) ->
	stop(),
	timer:sleep(10),
	start(Port).


stop(_State) ->
    io:format("Shutting down mockserver~n"),
    case whereis(mockserver_sup) of
		undefined -> no_op;
		Pid -> exit(Pid, normal)
	end.