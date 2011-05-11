-module(server).
-compile(export_all).

start() ->
	{ok, Sock} = gen_udp:open(8888, [binary,{active, true}]),
	listen(Sock, []).


listen(Sock, [])->
   receive
   	{udp, Socket, IP, InPortNo, Packet} ->
   		io:format("recieved first client: ~p Port:~p\n", [IP, InPortNo]),
   		case inet:sockname(Socket) of
   			{ok, {Address, Port}} ->
   				io:format("Socket is Add: ~p Port: ~p\n", [Address, Port]);
   			{error, _} ->
   				io:format("Some error\n", [])
   		end,
   		listen(Sock, [IP])
    end;

listen(Sock, [H|T])->
   receive
   	{udp, Socket, IP, InPortNo, Packet} ->
	   	io:format("recieved second client: ~p Port:~p\n", [IP, InPortNo]),
	   	case inet:sockname(Socket) of
   			{ok, {Address, Port}} ->
   				io:format("Socket is Add: ~p Port: ~p\n", [Address, Port]);
   			{error, _} ->
   				io:format("Some error\n", [])
   		end,
   		gen_udp:send(Sock, IP, 8888, term_to_binary([H, IP])),
	   	io:format("sent message to second client: ~p\n", [H]),
   		listen(Sock, [IP]++[H|T])
    end.
