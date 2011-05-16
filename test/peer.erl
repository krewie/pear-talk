-module(peer).
-compile(export_all).
-export([start/6]).
-define(Server, 'linne.it.uu.se').
-define(Port, 8888).
-define(ListenPort, 9997).

authentication(Mail) ->
%%	getServerList(),  TBI
	rul:friends(),
	{ok, Sock} = gen_tcp:connect(?Server, ?Port, [binary,{active, true}]),
	gen_tcp:send(Sock, term_to_binary({client, login, Mail, ?ListenPort})),
	io:format("Sent the request for friends\n", []),
	receive
		{tcp, Sock, Packet} ->
				{server, friendlist, FriendList} = binary_to_term(Packet),
				rul:fillTable(friends, FriendList)
	end,
	io:format("Added friends in to ets\n", []),
	{ok, {MyIP, _}} = inet:sockname(Sock),
	gen_tcp:close(Sock),
	start(MyIP, "Krewi", Mail, ?ListenPort, ?Server, ?Port).
	%start(MyIP, ShowedName, Mail, ListenPort, ServerIP, ServerPort).
	
		
start(MyIP, ShowedName, Mail, ListenPort, ServerIP, ServerPort) ->
	try				
		register(chat, spawn(peer, status, [{ShowedName, Mail, MyIP, ListenPort, ServerIP, ServerPort}])),
		register(chat_server, spawn(peer, server, [MyIP, ListenPort])),
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

server(MyIP, ListenPort)->
	case gen_tcp:listen(ListenPort, [binary, {active, false}, {ip, MyIP}]) of
		{ok, ListenSocket} ->
			wait_connect(ListenSocket),
			server(MyIP, ListenPort);
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
				{Sender, Receiver, Data, Mode, Bin} = binary_to_term(list_to_binary(lists:reverse(BinaryList))),
				case Mode of
					file ->
						{FileName, File} = Bin,
						file:write_file(FileName, File),
						io:format("File ~p written on disk~n", [FileName]),
						file:write_file("log_file.txt", Timestamp ++ " " ,[append]),
						file:write_file("log_file.txt", "File " ++ FileName 
							++ " written on disk",[append]);
					ping ->
						{ok, {SenderIP, SenderPort}} = inet:peername(Socket), 
						rul:set_online(friends, Sender, SenderIP, SenderPort),
						chat!{send, Sender, [], pong, []};
						%io:format("ping from ~p ~n", [Sd]);
				   	pong -> 
						{ok, {SenderIP, SenderPort}} = inet:peername(Socket),
						rul:set_online(friends, SenderIP, SenderPort);
						%io:format("pong from ~p ~n", [Sd]);
					_Any ->
			      			io:format("~p~n", [Sender ++ " to " ++ Receiver ++ ": " ++ Data]),
						file:write_file("log_file.txt", Timestamp ++ " " ,[append]),
     		  				file:write_file("log_file.txt", Sender ++ " to " ++ Receiver ++ ": " ++ Data ++ "\n",[append])
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
		{ShowedName, ID, _MyIP, _ListenPort, _ServerIP, _ServerPort} ->
		try
			[RecName, RecIP, RecPort] = rul:take(friends, Receiver),
			case gen_tcp:connect(RecIP, RecPort, [binary,{packet,0},{port, 0}]) of
				{ok, Sock} ->
					try
						{{Y,M,D},{H,MM,S}} = erlang:localtime(),
						Timestamp = lists:flatten(io_lib:format(
							"~p/~p/~p - ~p:~p:~p",[Y,M,D,H,MM,S])),
						if Data /= "" ->
							file:write_file("log_file.txt", Timestamp ++ " " ,[append]),
	  						file:write_file("log_file.txt", ShowedName ++ " to " ++ RecName ++ ": " 
								++ Data ++ "\n",[append]);
						true ->
							ok			
						end,
						sendB(Sock, term_to_binary({ID, RecName, Data, Mode, Obj}))
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
    gen_tcp:send(Socket, Chunk),
    sendB(Socket, Rest);
sendB(Socket,Rest) ->
    gen_tcp:send(Socket, Rest).

ping_friends(Table) ->
	try 
		rul:traverse(Table, (fun([{R, _}|_L]) -> ping(R) end))
	catch 
		_:_ ->
			error
	end.

ping_loop() ->
	ping_friends(friends),
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
