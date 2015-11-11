-module(mockserver_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Args, Type), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(mockserver_worker, [Port], worker)]} }.

