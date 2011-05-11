-module(client).
-compile(export_all).
-define(SERVER, 'linne.it.uu.se').
-define(PORT, 8888).

hello() ->
	{ok, Sock} = gen_udp:open(?PORT, [binary,{active, true}]), 
	gen_udp:send(Sock,?SERVER,?PORT,"hello?."),
	receive
		{udp, Socket, IP, InPortNo, Packet} ->
			[ClientAdd, Me] = binary_to_term(Packet),
			io:format("got a message from ~p containing: ~p\n", [IP, ClientAdd]),
			if
				(IP /=  ClientAdd) ->
					gen_udp:send(Sock, ClientAdd, ?PORT, term_to_binary([Me, Me])),
					io:format("sent a message to ~p containing: ~p\n", [ClientAdd, Me]);
			   	true ->
			   		io:format("contact is established\n", [])
			end,
   	end,
   	start(ClientAdd, ?PORT, Sock).
   			
start(Address, Port, Sock) ->
	gen_udp:send(Sock,Address,?PORT,"Connected."),		
	Wpid = spawn(chat_frame, start, [self()]),
	manager(Sock,Address,?PORT, Wpid).

manager(Sock,Address,Port, Wpid) ->
	receive
		{udp, Socket, IP, InPortNo, Packet} ->
    			Data = binary_to_list(Packet),
    			Wpid ! {chat_line, Data};
		{send_chat, Data} -> 
			send(Sock,Address,Port, Data)
	end,
	manager(Sock,Address,Port, Wpid).

send(Sock,Address,Port, <<Chunk:100/binary, Rest/binary>>) ->
	gen_udp:send(Sock,Address,Port, Chunk),
	send(Sock,Address,Port, Rest);

send(Sock,Address,Port,Rest) ->
	gen_udp:send(Sock,Address,Port, Rest).
