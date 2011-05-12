-module(peer).
-compile(export_all). 
status(Status) ->
    receive
        {status, Pid} ->
            Pid!Status,
            status(Status);
        {change, NewStatus} ->
            status(NewStatus);
	{send, PID, IpAddress, RemotePort, Sender, Receiver, Data} ->
	    PID!(spawn(peer,send,[IpAddress, RemotePort, Sender, Receiver, Data])),
	    status(Status);
        stop ->
            true;
        _Any->
            status(Status)
    end.

start(NetworkInterface, ListenPort) ->
	try 
		register(chat, spawn(peer, status, [{NetworkInterface,ListenPort,[],[]}]))
	catch
		_:_ ->  io:format("The chat is allready running!~n")
	end,
	try 
		register(chat_server, spawn(peer, server, [NetworkInterface, ListenPort]))
	catch
		_:_ ->  io:format("The server is allready running!~n")
	end.

shut_down() ->
	try
		exit(whereis(chat_server),kill)
	catch
		_:_ -> io:format("chat_server allready down!~n")
	end,
	try
		exit(whereis(chat),kill)
	catch
		_:_ -> io:format("chat allready down!~n")
	end.

host_info() -> inet:gethostbyname(net_adm:localhost()). 
get_status()->
	chat!{status, self()},
	receive 
		Status ->
			Status
	end.  
%%-----------------------------------------------------------------------------------------------
server(NetworkInterface, ListenPort)->
	case gen_tcp:listen(ListenPort, [binary, {active, false},{ip, NetworkInterface}]) of
		{ok, ListenSocket} ->
			wait_connect(ListenSocket),
			server(NetworkInterface, ListenPort);
		{error, _} ->
			"error on server start up!"
	end.

wait_connect(ListenSocket) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, Socket} ->
    			get_request(Socket,[]),
			wait_connect(ListenSocket);
		{error, _} ->
			"connection refused!"
	end.

get_request(Socket, BinaryList) ->
	case gen_tcp:recv(Socket,0) of
		{ok, Binary} ->
			get_request(Socket, [Binary|BinaryList]);
      	{error, closed} ->
			{{Y,M,D},{H,MM,S}} = erlang:localtime(),
			Timestamp = lists:flatten(io_lib:format("~p/~p/~p - ~p:~p:~p", [Y,M,D,H,MM,S])),
			try
				{_, Sender, Receiver, Data} = binary_to_term(binary:list_to_bin(lists:reverse(BinaryList))),
			      	io:format("~p~n", [Sender ++ " to " ++ Receiver ++ ": " ++ Data]),
				ok = file:write_file("log_file.txt", Timestamp ++ " " ,[append]),
     		  		ok = file:write_file("log_file.txt", Sender ++ 
					" to " ++ Receiver ++ ": " ++ Data ++ "\n",[append])
			catch
				_:_ -> io:format("Sending error =( ~n")
			end
     		end.
   
%%--------------------------------------------------------------------------------------------------
send(IpAddress, RemotePort, Sender, Receiver, Data) ->
	case gen_tcp:connect(IpAddress, RemotePort, [binary,{packet,0},{port, 0}])   of
		{ok, Sock} ->
			try
				{{Y,M,D},{H,MM,S}} = erlang:localtime(),
				Timestamp = lists:flatten(io_lib:format("~p/~p/~p - ~p:~p:~p", [Y,M,D,H,MM,S])),
				ok = file:write_file("log_file.txt", Timestamp ++ " " ,[append]),
  				ok = file:write_file("log_file.txt", Sender ++ " to " ++ Receiver ++ ": " 
				++ Data ++ "\n",[append]),
				sendB(Sock, term_to_binary({Sock, Sender, Receiver, Data})),
				gen_tcp:close(Sock)
			catch
				_:_ -> io:format("Sending error =( ~n")
			end;
		{error, _} ->
			"connection error!"
	end.
   

sendB(Socket, <<Chunk:100/binary, Rest/binary>>) ->
    gen_udp:send(Socket, Chunk),
    sendB(Socket, Rest);
sendB(Socket,Rest) ->
    gen_udp:send(Socket, Rest).
