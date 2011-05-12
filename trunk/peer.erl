-module(peer).
-compile(export_all). 
status(Status) ->
    receive
        {status, Pid} ->
            Pid!Status,
            status(Status);
        {change, NewStatus} ->
            status(NewStatus);
        stop ->
            true;
        _Any->
            status(Status)
    end.

start(Me, NetworkInterface, ListenPort) ->
	try 
		register(chat, spawn(peer, status, [{NetworkInterface,ListenPort,Me,[]}]))
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
				{Ni, Lp, Sd, R, Data, Mode, Bin} = 
					binary_to_term(list_to_binary(lists:reverse(BinaryList))),
				case Mode of 
					file ->
						{FileName, File} = Bin,
						file:write_file(FileName, File),
						io:format("File ~p written on disk~n", [FileName]),
						file:write_file("log_file.txt", Timestamp ++ " " ,[append]),
						file:write_file("log_file.txt", "File " ++ FileName 
							++ " written on disk",[append]);
					ping ->
						ul:set_online(Sd, Ni, Lp,[],[]),
						send(Sd, "pong", pong,[]),
						io:format("ping~n");
				   	pong ->
						io:format("pong~n");
					_Any ->
			      			io:format("~p~n", [Sd ++ " to " ++ R ++ ": " ++ Data]),
						file:write_file("log_file.txt", Timestamp ++ " " ,[append]),
     		  				file:write_file("log_file.txt", Sd ++ " to " 
							++ R ++ ": " ++ Data ++ "\n",[append])
				end
			catch
				_:_ -> io:format("Sending error ojoj =( ~n")
			end
     		end.
   
%%--------------------------------------------------------------------------------------------------
send(Receiver, Data, Mode, Obj) ->
	chat!{status, self()},
	receive
		{Ni,Lp,Sender,_}  ->
		try
			[_,_,IpAddress, RemotePort,_,_] = ul:take(Receiver),
			case gen_tcp:connect(IpAddress, RemotePort, [binary,{packet,0},{port, 0}]) of
				{ok, Sock} ->
					try
						{{Y,M,D},{H,MM,S}} = erlang:localtime(),
						Timestamp = lists:flatten(io_lib:format(
							"~p/~p/~p - ~p:~p:~p",[Y,M,D,H,MM,S])),
						file:write_file("log_file.txt", Timestamp ++ " " ,[append]),
	  					file:write_file("log_file.txt", Sender ++ " to " ++ Receiver ++ ": " 
							++ Data ++ "\n",[append]),
						sendB(Sock, term_to_binary({Ni, Lp, Sender, Receiver, Data, Mode, Obj}))
					catch
						_:_ -> io:format("Sending error =( ~n")
					end,
					gen_tcp:close(Sock);
				{error, _} ->
					"connection error!"
			end
		catch 
			_:_ -> 
			{error,user_offline}
		end
	end.

send_file(Receiver, Filename, Name) ->
	{ok,Bin} = file:read_file(Filename),
	send(Receiver, [], file, {Name, Bin}).

sendB(Socket, <<Chunk:100/binary, Rest/binary>>) ->
    gen_udp:send(Socket, Chunk),
    sendB(Socket, Rest);
sendB(Socket,Rest) ->
    gen_udp:send(Socket, Rest).

ping_friends() ->
	ul:open(),
	dets:traverse(ul, (fun({R, _}) -> send(R,"ping", ping, []), continue  end)),
	ul:close().
