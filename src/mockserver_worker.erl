-module(mockserver_worker).

-behaviour(gen_server).

-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init([]) ->
	io:format("Starting elli~n"),
	CallbackArgs = [{'GET', 
		[
			{<<"provs">>, [{ok, [], <<"got_provs">>}]}]
	}],
	{ok, Pid} = elli:start_link([{callback, mock_rest}, {port, 3000}, {callback_args, CallbackArgs}]),
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