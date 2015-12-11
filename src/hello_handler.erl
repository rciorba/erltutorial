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
    %% Headers = [{binary:bin_to_list(Header), binary:bin_to_list(Value)} ||
    %%               {Header, Value} <- HeadersBinary],
    Host = "http://localhost:5050",
    case Method of
        post ->
            {Method, Host ++ binary_to_list(Path), HeadersBinary, <<>>, []};
        _ ->
            {Method, Host ++ binary_to_list(Path), HeadersBinary, <<>>, []}
    end.

proxy_request(Req) ->
    {Method, Url, Headers, Body, Options} = prepare_request(Req),
    hackney:request(Method, Url, Headers, Body, Options).

handle(Req, State=#state{}) ->
    Resp = proxy_request(Req),
    io:fwrite("~p ~n", [Resp]),
    case Resp of
        {ok, StatusCode, RespHeaders, ClientRef} ->
            %% BinaryHeaders = [{binary:list_to_bin(H), binary:list_to_bin(V)} ||
            %%                     {H, V} <- Headers],
            {ok, Body} = hackney:body(ClientRef),
            {ok, Req2} = cowboy_req:reply(
                           StatusCode,
                           RespHeaders ++ [{<<"X-Proxy">>, <<"yobwoc">>}],
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
