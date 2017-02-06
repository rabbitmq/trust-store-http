-module(trust_store_http_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    CurrentDir = get_default_dir(),
    Directory = case application:get_env(trust_store_http, directory) of
        undefined ->
            application:set_env(trust_store_http, directory, CurrentDir),
            CurrentDir;
        Dir -> Dir
    end,
    Dispatch = cowboy_router:compile([
        {'_', [{"/", list_handler, []},
               {"/certs/[...]", cowboy_static, {dir, Directory, [{mimetypes, {<<"text">>, <<"html">>, []}}]}}]}
    ]),
    cowboy:start_http(my_http_listener, 100, [{port, 8080}],
        [{env, [{dispatch, Dispatch}]}]
    ),
    trust_store_http_sup:start_link().

stop(_State) ->
	ok.



get_default_dir() ->
    case os:getenv("CERT_DIR") of
        false ->
            {ok, CurrentDir} = file:get_cwd(),
            CurrentDir;
        Dir -> Dir
    end.