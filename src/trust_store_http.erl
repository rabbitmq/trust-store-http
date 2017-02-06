-module(trust_store_http).


-export([main/1]).

main([]) ->
    application:set_env(lager, handlers, [{lager_console_backend, info}]),
    io:format("~nStarting trust store server ~n", []),
    application:ensure_all_started(trust_store_http),
    io:format("~nTrust store server started on port ~p ~n",
              [application:get_env(trust_store_http, port, undefined)]),
    user_drv:start(),
    timer:sleep(infinity).