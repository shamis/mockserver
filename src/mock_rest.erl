-module(mock_rest).

-export([handle/2, handle_event/3]).

-include("deps/elli/include/elli.hrl").
-behaviour(elli_handler).

-include_lib("kernel/include/file.hrl").

%%
%% ELLI REQUEST CALLBACK
%%

handle(Req, Args) ->
    try handle(Req#req.method, elli_request:path(Req), Req, Args) of
        Response -> Response
    catch
        E1:E2 -> 
            io:format("Error ~p:~p~n", [E1, E2]),
            erlang:display(erlang:get_stacktrace()),
            {404, [], <<"Not a valid rule">>}
    end.

handle(Type, [Path], Req, Args) when ((Type == 'POST') or (Type == 'PATCH')) ->
     % io:format("path from config : ~p~n", [maps:get(Path, maps:get(Type, Args))]),
     #{data := Data, response := Response} = maps:get(Path, 
        maps:get(Type, Args)),
     PData = jsxn:decode(elli_request:post_args(Req)),
     case check_data(PData, Data) of
        true -> Response;
        false -> {400, [], <<"Not all required parameters present in the request">>}
    end;
     %io:format("Post Args : ~p~n", [elli_request:post_args(Req)]),

handle(Type, [Path], _Req, Args) ->
    maps:get(response, 
        maps:get(Path,
            maps:get(Type, Args)));

% handle('GET',[<<"hello">>, <<"world">>], _Req, _Args) ->
%     %% Reply with a normal response. 'ok' can be used instead of '200'
%     %% to signal success.
%     {ok, [], <<"Hello World!">>};

% handle('GET', [<<"hello">>], Req, _Args) ->
%     %% Fetch a GET argument from the URL.
%     Name = elli_request:get_arg(<<"name">>, Req, <<"undefined">>),
%     {ok, [], <<"Hello ", Name/binary>>};

% handle('POST', [<<"hello">>], Req, _Args) ->
%     %% Fetch a POST argument from the POST body.
%     Name = elli_request:post_arg(<<"name">>, Req, <<"undefined">>),
%     %% Fetch and decode
%     City = elli_request:post_arg_decoded(<<"city">>, Req, <<"undefined">>),
%     {ok, [], <<"Hello ", Name/binary, " of ", City/binary>>};

handle(_, _, _Req, _Args) ->
    io:format("Not a valid rule~n"),
    {404, [], <<"Not a valid rul">>}.

handle_event(_, _, _) ->
    ok.

check_data(PData, Data) ->
    lists:all(fun(E) -> 
        maps:is_key(E, PData)
        end, Data).
