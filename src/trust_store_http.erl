-module(trust_store_http).


-export([main/1]).

main([]) ->
    io:format("~nStarting trust store server ~n", []),
    application:ensure_all_started(trust_store_http),
    io:format("~nTrust store server started on port 8080 ~n", []),
    user_drv:start(),
    timer:sleep(infinity).