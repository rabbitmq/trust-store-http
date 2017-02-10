-module(list_handler).
-behaviour(cowboy_http_handler).

-include_lib("kernel/include/file.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {ok, Directory} = application:get_env(trust_store_http, directory),
    case modified(Req, Directory) of
        {true, Mtime} ->
            case list_files(Directory) of
                {ok, Files}  -> respond(Mtime, Files, Req, State);
                {error, Err} -> respond_error(Err, Req, State)
            end;
        false           -> respond_no_change(Req, State);
        {error, Reason} -> respond_error(Reason, Req, State)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

respond(Mtime, Files, Req, State) ->
    ResponseBody = json_encode(Files),
    Headers = [{<<"Last-Modified">>, cowboy_clock:rfc1123(Mtime)},
               {<<"content-type">>, <<"application/json">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, ResponseBody, Req),
    {ok, Req2, State}.

respond_no_change(Req, State) ->
    {ok, Req2} = cowboy_req:reply(304, Req),
    {ok, Req2, State}.

respond_error(Reason, Req, State) ->
    Error = io_lib:format("Error listing certificates ~p", [Reason]),
    lager:log(error, "~s", [Error]),
    {ok, Req2} = cowboy_req:reply(500, [], iolist_to_binary(Error), Req),
    {ok, Req2, State}.

json_encode(Files) ->
    Map = #{certificates => [ #{id   => cert_id(FileName, FileDate),
                                path => cert_path(FileName)}
                              || {FileName, FileDate} <- Files ]},
    jsx:encode(Map).

cert_id(FileName, FileDate) ->
    iolist_to_binary(io_lib:format("~s:~p", [FileName, FileDate])).

cert_path(FileName) ->
    iolist_to_binary(["/certs/", FileName]).

-spec list_files(string()) -> [{string(), file:date_time()}].
list_files(Directory) ->
    case file:list_dir(Directory) of
        {ok, FileNames} ->
            PemFiles = [ FileName || FileName <- FileNames,
                                     filename:extension(FileName) == ".pem" ],
            {ok, lists:map(
                    fun(FileName) ->
                        FullName = filename:join(Directory, FileName),
                        {ok, Mtime} = modification_time(FullName, posix),
                        {FileName, Mtime}
                    end,
                    PemFiles)};
        {error, Err} -> {error, Err}
    end.

modified(Req, Directory) ->
    case modification_time(Directory, universal) of
        {ok, Mtime} ->
            {ok, HeaderTime, _Req} =
                cowboy_req:parse_header(<<"if-modified-since">>, Req),
            case date_later(Mtime, HeaderTime) of
                true ->
                    {true, Mtime};
                false ->
                    false
            end;
        {error, Reason} -> {error, Reason}
    end.

date_later(_Mtime, undefined) -> true;
date_later(Mtime = {{_,_,_},{_,_,_}}, OldTime = {{_,_,_},{_,_,_}}) ->
    MtimeSec = calendar:datetime_to_gregorian_seconds(Mtime),
    OldTimeSec = calendar:datetime_to_gregorian_seconds(OldTime),
    MtimeSec > OldTimeSec.

modification_time(FileName, Type) ->
    case file:read_file_info(FileName, [{time, Type}]) of
        {ok, #file_info{mtime = Mtime}} -> {ok, Mtime};
        {error, Reason} -> {error, Reason}
    end.










