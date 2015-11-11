-module(mockserver_worker).

-behaviour(gen_server).

-export([start_link/1, stop/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) ->
	io:format("Starting elli on Port: ~p~n", [Port]),
	io:format("Reading config~n"),
	io:format("~p~n", [c:pwd()]),
	{ok, [#{rules := Rules, response := Response}]} = file:consult("rules.config"),
	CallbackArgs = maps:map(fun(K, V) -> 
		maps:map(fun(K2, V2) ->
			lists:keyfind(V2, 1, maps:get(K2, maps:get(K, Rules)))
			end, V) end, Response),
	io:format("CallbackArgs : ~p~n", [CallbackArgs]),
	% CallbackArgs = #{'GET' => #{<<"provs">> => {200, [], <<"got_provs">>}}},
	{ok, _Pid} = elli:start_link([{callback, mock_rest}, 
								  {port, Port}, 
								  {callback_args, CallbackArgs}]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop() ->
	terminate(stopping, {}).