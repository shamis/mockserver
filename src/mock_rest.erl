-module(mock_rest).

-export([handle/2, handle_event/3]).
-export([chunk_loop/1]).

-include("deps/elli/include/elli.hrl").
-behaviour(elli_handler).

-include_lib("kernel/include/file.hrl").

%%
%% ELLI REQUEST CALLBACK
%%

handle(Req, Args) ->
    io:format("Req Args : ~p~n", [Args]),
    handle(Req#req.method, elli_request:path(Req), Req, Args).



handle(Type, [Path], _Req, Args) ->
    maps:get(Path,maps:get(Type, Args, #{}), {404, [], <<"Not a valid rule">>});

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



%% Send 10 separate chunks to the client.
chunk_loop(Ref) ->
    chunk_loop(Ref, 10).

chunk_loop(Ref, 0) ->
    elli_request:close_chunk(Ref);
chunk_loop(Ref, N) ->
    timer:sleep(10),

    %% Send a chunk to the client, check for errors as the user might
    %% have disconnected
    case elli_request:send_chunk(Ref, [<<"chunk">>, integer_to_list(N)]) of
        ok -> ok;
        {error, Reason} ->
            io:format("error in sending chunk: ~p~n", [Reason])
    end,

    chunk_loop(Ref, N-1).


%%
%% ELLI EVENT CALLBACKS
%%


%% elli_startup is sent when Elli is starting up. If you are
%% implementing a middleware, you can use it to spawn processes,
%% create ETS tables or start supervised processes in a supervisor
%% tree.
handle_event(elli_startup, [], _) -> ok;

%% request_complete fires *after* Elli has sent the response to the
%% client. Timings contains timestamps of events like when the
%% connection was accepted, when request parsing finished, when the
%% user callback returns, etc. This allows you to collect performance
%% statistics for monitoring your app.
handle_event(request_complete, [_Request,
                                _ResponseCode, _ResponseHeaders, _ResponseBody,
                                _Timings], _) -> ok;

%% request_throw, request_error and request_exit events are sent if
%% the user callback code throws an exception, has an error or
%% exits. After triggering this event, a generated response is sent to
%% the user.
handle_event(request_throw, [_Request, _Exception, _Stacktrace], _) -> ok;
handle_event(request_error, [_Request, _Exception, _Stacktrace], _) -> ok;
handle_event(request_exit, [_Request, _Exception, _Stacktrace], _) -> ok;

%% invalid_return is sent if the user callback code returns a term not
%% understood by elli, see elli_http:execute_callback/1.
%% After triggering this event, a generated response is sent to the user.
handle_event(invalid_return, [_Request, _ReturnValue], _) -> ok;


%% chunk_complete fires when a chunked response is completely
%% sent. It's identical to the request_complete event, except instead
%% of the response body you get the atom "client" or "server"
%% depending on who closed the connection.
handle_event(chunk_complete, [_Request,
                              _ResponseCode, _ResponseHeaders, _ClosingEnd,
                              _Timings], _) -> ok;

%% request_closed is sent if the client closes the connection when
%% Elli is waiting for the next request on a keep alive connection.
handle_event(request_closed, [], _) -> ok;

%% request_timeout is sent if the client times out when
%% Elli is waiting for the request.
handle_event(request_timeout, [], _) -> ok;

%% request_parse_error fires if the request is invalid and cannot be
%% parsed by erlang:decode_packet/3 or it contains a path Elli cannot
%% parse or does not support.
handle_event(request_parse_error, [_], _) -> ok;

%% client_closed can be sent from multiple parts of the request
%% handling. It's sent when the client closes the connection or if for
%% any reason the socket is closed unexpectedly. The "Where" atom
%% tells you in which part of the request processing the closed socket
%% was detected: receiving_headers, receiving_body, before_response
handle_event(client_closed, [_Where], _) -> ok;

%% client_timeout can as with client_closed be sent from multiple
%% parts of the request handling. If Elli tries to receive data from
%% the client socket and does not receive anything within a timeout,
%% this event fires and the socket is closed.
handle_event(client_timeout, [_Where], _) -> ok;

%% bad_request is sent when Elli detects a request is not well
%% formatted or does not conform to the configured limits. Currently
%% the Reason variable can be any of the following: {too_many_headers,
%% Headers}, {body_size, ContentLength}
handle_event(bad_request, [_Reason], _) -> ok;

%% file_error is sent when the user wants to return a file as a
%% response, but for some reason it cannot be opened.
handle_event(file_error, [_ErrorReason], _) -> ok.