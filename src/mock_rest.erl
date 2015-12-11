-module(mock_rest).

-export([handle/2, handle_event/3]).

-include("deps/elli/include/elli.hrl").
-behaviour(elli_handler).

-include_lib("kernel/include/file.hrl").

%%
%% ELLI REQUEST CALLBACK
%%

handle(Req, Args) ->
    Path = elli_request:path(Req),
    io:format("got Request path : ~p Type : ~p~n", [Path, Req#req.method]),
    case maps:get(Path, maps:get(Req#req.method, Args, #{}),undefined) of
        undefined ->
            {404, [], <<"Not a valid rule">>};
        Rule ->
            case authenticate(maps:get(auth, Rule, undefined),
                elli_request:get_header(<<"Authorization">>,Req, undefined)) of
                true ->
                    handle(Req#req.method, Req, Rule);
                false ->
                    {401, [], <<"Not authorized">>}
            end
    end.

handle(Type, Req, #{response := Response, data := Data}) 
    when ((Type == 'POST') or (Type == 'PUT')) ->
    PData = jsxn:decode(elli_request:post_args(Req)),
    case check_data(PData, Data) of
        true -> Response;
        false -> {400, [], <<"Not all required parameters present in the request">>}
    end;

handle(_Type, _Req, #{response := Response}) ->
   Response; 

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

handle(_, _Req, _Args) ->
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
    io:format("Authentication type : ~p not supoorted~n", [Type]),
    true;
authenticate(undefined, _) -> true;
authenticate(_, undefined) -> true.
