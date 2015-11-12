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
    #{data := Data, response := Response, auth := Auth} = maps:get(Path, 
        maps:get(Type, Args)),
    case authenticate(Auth, elli_request:get_header(<<"Authorization">>,Req)) of
        true ->
            PData = jsxn:decode(elli_request:post_args(Req)),
            case check_data(PData, Data) of
                true -> Response;
                false -> {400, [], <<"Not all required parameters present in the request">>}
            end;
        false -> {401, [], <<"Not authorized">>}
    end;

handle(Type, [Path], _Req, Args) ->
    maps:get(response, 
        maps:get(Path,
            maps:get(Type, Args)));

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
    {404, [], <<"Not a valid rule">>}.

handle_event(_, _, _) ->
    ok.

check_data(PData, Data) ->
    lists:all(fun(E) -> 
        maps:is_key(E, PData)
        end, Data).

authenticate([basic, Username, Password], AuthHeader) ->
    Auth = base64:encode(Username ++ ":" ++ Password),
    case binary:split(AuthHeader, <<" ">>) of
        [<<"Basic">>, Auth] -> true;
        _ -> false
    end;
authenticate([Type|_], _) ->
    io:format("Authentication type : ~p not supoorted~n", [Type]).
