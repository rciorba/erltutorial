-module(hello_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {}).

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

bin_to_lower(String) ->
    <<<<(string:to_lower(C))/utf8>> || <<C/utf8>> <= <<String/binary>>>>.

method_bin_to_atom(Method) ->
    _valid_atoms = {get, post, put, delete},
    binary_to_existing_atom(bin_to_lower(Method), latin1).

prepare_request(Req) ->
    {Path, _} = cowboy_req:path(Req),
    {MethodBinary, _} = cowboy_req:method(Req),
    Method = method_bin_to_atom(MethodBinary),
    {HeadersBinary, _} = cowboy_req:headers(Req),
    Headers = [{binary:bin_to_list(Header), binary:bin_to_list(Value)} ||
                  {Header, Value} <- HeadersBinary],
    Host = "http://localhost:5050",
    case Method of
        post ->
            {Method, {Host ++ binary_to_list(Path), Headers, "", <<"">>}};
        _ ->
            {Method, {Host ++ binary_to_list(Path), Headers}}
    end.

proxy_request(Req) ->
    {Method, Request} = prepare_request(Req),
    httpc:request(Method, Request, [], []).

handle(Req, State=#state{}) ->
    Resp = proxy_request(Req),
    io:fwrite("~p ~n", [Resp]),
    case Resp of
        {ok, {{_Version, Status, _Reason}, Headers, Body}} ->
            {ok, Req2} = cowboy_req:reply(
                           Status,
                           Headers ++ [{"X-Proxy", "yobwoc"}],
                           Body,
                           Req),
            {ok, Req2, State};
        {error, _Reason} ->
            {ok, Req2} = cowboy_req:reply(
                           503,
                           [],
                           "error",
                           Req),
            {ok, Req2, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.
