%%I'VE JUST MADE THIS COMMENT

-module(a).
-compile(export_all).

client() ->
    SomeHostInNet = {192,168,0,103},
    {ok, Sock} = gen_tcp:connect(SomeHostInNet, 8080,[binary]),
    io:format("Sock:  ~p = ~p\n", [Sock, inet:sockname(Sock)]),
    ok = gen_tcp:send(Sock, "Some Data"),
    ok = gen_tcp:close(Sock).


server() ->
    {ok, LSock} = gen_tcp:listen(8080, [binary]),
    io:format("LSock: ~p = ~p\n", [LSock, inet:sockname(LSock)]),
    {ok, Sock} = gen_tcp:accept(LSock),
    io:format("Sock:  ~p = ~p\n", [Sock, inet:sockname(Sock)]),
    {_, Bin} = do_recv(Sock, []),
    ok = gen_tcp:close(Sock),
    Bin.

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            do_recv(Sock, [Bs, B]);
        {error, _} ->
            {ok, list_to_binary(Bs)}
    end.