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
	{ok, [#{rules := Rules, responses := Responses}]} = file:consult("rules.config"),
	CallbackArgs = maps:map(fun(K, V) -> 
		lists:foldl(fun(E, Acc) ->
				M = maps:map(fun(K2, V2) ->
					Req = maps:get(K2, maps:get(K, Rules)),
					#{response => lists:keyfind(V2, 1, 
						maps:get(response, Req)),
					  data => maps:get(data, Req, #{}),
					  auth => maps:get(auth, Req, #{})
					 }
				end, E), 
			maps:merge(Acc, M) end, #{}, V) end,
		Responses),
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