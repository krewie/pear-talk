-module(peer).
-compile(export_all). 

start() ->
	try
		rul:friends(),
		
		{ok, File} = file:open("chat.ini", read),

		Me = (lists:nthtail(9, io:get_line(File, "")) -- " ") -- "\n",
		PublicIp = read_address((lists:nthtail(9, io:get_line(File, "")) -- " ") -- "\n"),
		ListenPortS = io:get_line(File, ""),
		ServerAddress = read_address((lists:nthtail(14, io:get_line(File, "")) -- " ") -- "\n"),
		ServerPortS = io:get_line(File, ""),

		{_,[{_,_,_},{_,_},{_,_,ListenPort}],_} = erl_scan:string(ListenPortS--"\n"),
		{_,[{_,_,_},{_,_},{_,_,ServerPort}],_} = erl_scan:string(ServerPortS--"\n"),
	
		{ok,{_, _, _, _, _, [NetworkInterface|_]}}= host_info(),
		
		file:close(File),
				
		register(chat, spawn(peer, status, [{PublicIp, NetworkInterface,
			ListenPort, Me, ServerAddress, ServerPort, friends, pingoff}])),

		register(chat_server, spawn(peer, server, [NetworkInterface, ListenPort])),

		register(ping_pong, spawn(peer, ping_loop, []))

	catch Ek:En ->
			[{Ek, En}] ++ [shut_down()]
			
	end.

shut_down() ->
	try
		exit(whereis(chat_server),kill)
	catch
		Ek1:En1  -> {Ek1, En1}
	end,
	try
		exit(whereis(chat),kill)
	catch
		Ek2:En2  -> {Ek2, En2}
	end,
	try
		exit(whereis(ping_pong),kill)
	catch
		Ek3:En3  -> {Ek3, En3}
	end.

host_info() -> inet:gethostbyname(net_adm:localhost()). 

get_status()->
	chat!{status, self()},
	receive 
		Status ->
			Status
	end.  

mess(Receiver, Data) ->	chat!{send, Receiver, Data, [], []}, sent.

ping(Receiver) -> chat!{send, Receiver, [], ping, []}, sent.

send_file(Receiver, Path, Name) ->
	try
		{ok,Bin} = file:read_file([Path ++ Name]),
		chat!{send, Receiver, [], file, {Name, Bin}},
		sent
	catch _:_ ->
		{error, file_not_found}
	end.
pingon() ->
	chat!{status,self()},
	receive
		{PublicIp, NetworkInterface,
				ListenPort, Me, ServerAddress, ServerPort, friends, _} ->
		chat!{change,{PublicIp, NetworkInterface,
					ListenPort, Me, ServerAddress, ServerPort, friends, pingon}},
		pingon
	end.

pingoff() ->
	chat!{status,self()},
	receive
		{PublicIp, NetworkInterface,
				ListenPort, Me, ServerAddress, ServerPort, friends, _} ->
		chat!{change,{PublicIp, NetworkInterface,
					ListenPort, Me, ServerAddress, ServerPort, friends, pingoff}},
		pingoff
	end.

%%-----------------------------------------------------------------------------------------------
status(Status) ->
    receive
        {status, Pid} ->
            Pid!Status,
            status(Status);
        {change, NewStatus} ->
            status(NewStatus);
	{send, Receiver, Data, Mode, Obj} ->
            	spawn(peer,send,[Receiver, Data, Mode, Obj]),
		status(Status);
        stop ->
            true;
        _Any->
            status(Status)
    end.

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
			%io:format("~p",[inet:peername(Socket)]),
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
				{PublicIp, Lp, _Skt, Sd, R, Data, Mode, ServerIp, ServerPort, Bin} = 
					binary_to_term(list_to_binary(lists:reverse(BinaryList))),
				case Mode of 
					friendlist ->
						chat!{status, self()},
						receive
							{PIp, NetworkInterface,ListenPort, Me, 
								ServerIp, ServerPort, _, PingMode} ->
								chat!{change,{PIp, NetworkInterface,
									ListenPort,Me, ServerIp, ServerPort, Bin, PingMode}}
					end;	
					file ->
						{FileName, File} = Bin,
						file:write_file(FileName, File),
						io:format("File ~p written on disk~n", [FileName]),
						file:write_file("log_file.txt", Timestamp ++ " " ,[append]),
						file:write_file("log_file.txt", "File " ++ FileName 
							++ " written on disk",[append]);
					ping ->
						chat!{status, self()},
						receive
							{_, _, _, _, _, _, FriendList,PingMode}  -> 
								rul:set_online(FriendList, Sd, PublicIp, Lp,[],[])
						end,
						
						chat!{send, Sd, [], pong, []},
						if 
							PingMode == pingon ->
								io:format("ping from ~p ~n", [Sd]);
							true ->

								[]
						end;
				   	pong -> 
						chat!{status, self()},
						receive
							{_, _, _, _, _, _, FriendList,PingMode}  -> 
								rul:set_online(FriendList, Sd, PublicIp, Lp,[],[])
						end,
						if 
							PingMode == pingon ->
								io:format("pong from ~p ~n", [Sd]);
							true ->
								[]
						end;
					_Any ->
			      		io:format("~p~n", [Sd ++ " to " ++ R ++ ": " ++ Data]),
						file:write_file("log_file.txt", Timestamp ++ " " ,[append]),
     		  				file:write_file("log_file.txt", Sd ++ " to " 
							++ R ++ ": " ++ Data ++ "\n",[append])
				end
			catch
				Ek:En -> {Ek,En}
			end
     		end.
   
%%--------------------------------------------------------------------------------------------------
send(Receiver, Data, Mode, Obj) ->
	E = [],
	chat!{status, self()},
	receive
		{PublicIp, _, Lp, Sender, ServerIp, ServerPort, FriendList, _}  ->
		try
			[_,_,IpAddress, RemotePort,_,_] = rul:take(FriendList, Receiver),
			case gen_tcp:connect(IpAddress, RemotePort, [binary,{packet,0},{port, 0}]) of
				{ok, Sock} ->
					try
						{{Y,M,D},{H,MM,S}} = erlang:localtime(),
						Timestamp = lists:flatten(io_lib:format(
							"~p/~p/~p - ~p:~p:~p",[Y,M,D,H,MM,S])),
						if Data /= "" ->
							
							file:write_file("log_file.txt", Timestamp ++ " " ,[append]),
	  						file:write_file("log_file.txt", Sender ++ " to " ++ Receiver ++ ": " 
								++ Data ++ "\n",[append]);
						true ->
							ok			
						end,
						sendB(Sock, term_to_binary({PublicIp, Lp, Sock, Sender, Receiver, Data, Mode, 
							ServerIp, ServerPort,Obj}))
					catch
						Ek:En -> E = E ++ [{Ek, En}]
					end,
					gen_tcp:close(Sock);
				{Er, Err} ->
					E ++ [{Er, Err}]
			end
		catch 
			_:_ -> 
			E ++ [{error,user_offline}]
		end
	end,
	E.

sendB(Socket, <<Chunk:100/binary, Rest/binary>>) ->
    gen_udp:send(Socket, Chunk),
    sendB(Socket, Rest);
sendB(Socket,Rest) ->
    gen_udp:send(Socket, Rest).

ping_friends(Table) ->
	try 
		rul:traverse(Table, (fun([{R, _}|_L]) -> ping(R) end))
	catch 
		_:_ ->
			error
	end.

ping_loop() ->
	chat!{status, self()},
	receive
		{_, _ ,_ ,_ , _, _, FriendList, _}  ->
			ping_friends(FriendList)
	end,
	timer:sleep(10000),
	ping_loop().

read_address(Address) ->
	case inet_parse:address(Address) of
		{ok,Ip} ->
			Ip;
		_ -> 
			Address
	end.
		
addme() ->
	try
		chat!{status, self()},
		receive
			{PIp, _,ListenPort, Me, 
						ServerIp, ServerPort, _,_} ->
			rul:add(friends, Me, "Me", [], PIp, ListenPort, ServerIp, ServerPort)
		end
	catch _:_ ->
		{error, start_peer_first}
	end.
