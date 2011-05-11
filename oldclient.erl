-module(oldclient).
-compile(export_all).
-define(SERVER, 'beurling.it.uu.se').
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
   			start(ClientAdd, ?PORT, Sock)
   	end.

start(Address, Port, Sock) ->
	%{ok, Sock} = gen_udp:open(Port, [binary,{active, false}]),
	gen_udp:send(Sock,Address,Port,"Connected."),
    	%spawn(client, listen, [Sock, self()]),
    	S_PID = spawn(client, write, [Sock, self()]),
	manager(Sock,Address,Port, S_PID).
	
manager(Sock,Address,Port, Write) ->
	receive
		{udp, Socket, IP, InPortNo, Packet} ->
    			Data = binary_to_list(Packet),
    			io:format("~p\n", [Data]);
		{Write, Data} -> 
			send(Sock,Address,Port, Data)
	end,
	manager(Sock,Address,Port,Write).

write(Sock, MotherPID)->
	A = io:get_line("ME: "),
	MotherPID ! {self(), A},
	write(Sock, MotherPID).

send(Sock,Address,Port, <<Chunk:100/binary, Rest/binary>>) ->
	gen_udp:send(Sock,Address,Port, Chunk),
	send(Sock,Address,Port, Rest);

send(Sock,Address,Port,Rest) ->
	gen_udp:send(Sock,Address,Port, Rest).

listen(Sock, MotherPID)->
    receive
    	{udp, Socket, IP, InPortNo, Packet} ->
    		Data = binary_to_list(Packet),
    		io:format("~p\n", [Data])
    end,
    listen(Sock, MotherPID).
