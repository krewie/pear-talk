-module(server).
-compile(export_all).
-define(TCP_OPTIONS, [binary, {active, false}]).
-define(PORT, 9945).
-define(TIMEOUT, 12000).

						%Server calls
						% ------- funktioner att implementera för servernätverk

						%join_network(server) ->

						% --------

shut_down() ->
    global ! {server, getport, self()},
    receive
	{server, getport, Port} ->
	    {ok, Con} = gen_tcp:connect({127,0,0,1}, Port, []),
	    send(Con, term_to_binary({server, shutdown})),
	    gen_tcp:close(Con)
    end.

globalPort(Port) ->
    receive
	{server, getport, Pid} ->
	    Pid ! {server, getport, Port}
    end.

start_server(Port) ->
    {ok, ListenSock} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    DBpid = spawn(serv_ul, start, []),
    spawn(?MODULE, wait_for_connection, [ListenSock, DBpid]),
    register(global, spawn(?MODULE, globalPort, [Port])).

wait_for_connection(ListenSocket, DBPid) ->
    case gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
	    case gen_tcp:recv(Socket, 0) of
		{ok, Packet} ->
		    case Data = binary_to_term(Packet) of
			{server, shutdown} ->
			    io:format("Server shutdown\n", []),
			    DBPid ! Data,
			    gen_tcp:close(ListenSocket);
			_ ->
			    Lpid = spawn(?MODULE, listen_state, [Socket, DBPid]), 
			    Lpid ! Data,
			    wait_for_connection(ListenSocket, DBPid)
		    end;
		{error, _ } ->
		    wait_for_connection(ListenSocket, DBPid)
	    end;
	{error, _ } ->
	    wait_for_connection(ListenSocket, DBPid)
    end.


listen_state(Socket, DBPid) ->
    io:format("recieved request~n"),
    receive
        {client, reminder, Usermail, Port} ->
            {ok, {Address, _}} = inet:peername(Socket),	
            DBPid ! {client, reminder, Usermail,[Address, Port], self()},	
            listen_state(Socket,DBPid);	
             
	{client, login, Username, Password, Port} ->
	    {ok, {Address, _}} = inet:peername(Socket),
	    DBPid ! {client, login, Username, [Address, Port], Password, self()},
	    listen_state(Socket, DBPid);

	{client, addfriend, MyID, FriendID} ->
	    DBPid ! {client, addfriend, MyID, FriendID, self()},
	    listen_state(Socket, DBPid);

	{client, acceptfriend, MyID, FriendID} ->
	    DBPid ! {client, acceptfriend, MyID, FriendID, self()},
	    listen_state(Socket, DBPid);

	{client, removefriend, MyID, FriendID} ->
	    DBPid ! {client, removefriend, MyID, FriendID, self()},
	    listen_state(Socket, DBPid);

	{client, changename, ID, Name} ->
	    DBPid ! {client, changename, ID, Name, self()},
	    listen_state(Socket, DBPid);

	{client, changepass, ID, New_password, Old_password} ->
	    DBPid ! {client, changepass, ID, New_password, Old_password, self()},
	    listen_state(Socket, DBPid);

	{client, search, ID, Search} ->
	    DBPid ! {client, search, ID, Search, self()},
	    listen_state(Socket, DBPid);

	{client, adduser, ID, Name, Password, Port} ->
	    {ok, {Address, _}} = inet:peername(Socket),
	    DBPid ! {client, adduser, ID, Name, Password, [Address, Port], self()},
	    listen_state(Socket, DBPid);

	{client, removeuser, ID, Port} ->
	    {ok, {Address, _}} = inet:peername(Socket),
	    DBPid ! {client, removeuser, ID, [Address, Port], self()},
	    listen_state(Socket,DBPid);

						%Server-calls
						%{server, serverlogin, {Netinfo}} ->
						%se till så att servern blir up-to-date
						%{server, userinfo, {server, client, Netinfo}} ->
						%skicka tillbaka clientinfo till server som ännu inte har infon
						%funkar för clientlogin likaså när man skapar vänlistan
						%{server, 

	{db, friendlist, NetInfo, Friendlist} ->
	    io:format("sending friendlist: ~p to ~w~n", [Friendlist, NetInfo]),
	    Data = term_to_binary({friendlist, Friendlist}),
	    [Ip, Port] = NetInfo,
	    case gen_tcp:connect(Ip, Port, []) of
		{ok, Sock} ->
		    send(Sock, Data),
		    gen_tcp:close(Sock),
		    io:format("Sent\n", []);
		_ -> io:format("Socket closed\n", []), ok
	    end;

	{db, badPass, NetInfo} ->
	    [Ip, Port] = NetInfo,
	    case gen_tcp:connect(Ip, Port, []) of
		{ok, Sock} ->
		    send(Sock, term_to_binary({server_badlogin, badPass})),
		    io:format("Sent badPass to ~w\n", [Sock]),
		    gen_tcp:close(Sock);
		_ -> ok
	    end;

	{db, badID, NetInfo} ->
	    [Ip, Port] = NetInfo,
	    case gen_tcp:connect(Ip, Port, []) of
		{ok, Sock} ->
		    send(Sock, term_to_binary({server_badlogin, badID})),
    		    io:format("Sent badID to ~w\n", [Sock]),
		    gen_tcp:close(Sock);
		_ -> ok
	    end;

	{db, addfriend, NetInfo, FriendList, ok} ->
	    case NetInfo of
		[Ip, Port] ->
		    case gen_tcp:connect(Ip, Port, []) of
			{ok, Sock} ->
			    Data = term_to_binary({friendlist, FriendList}),
			    send(Sock, Data),
			    gen_tcp:close(Sock);
			_ -> io:format("Socket closed\n", []),
			     ok
		    end;
		[] -> ok
	    end;

	{db, search, NetInfo, Results} -> 
	    io:format("Search results: ~p~n", Results),
	    case NetInfo of
		[Ip, Port] ->
		    case gen_tcp:connect(Ip, Port, []) of
			{ok, Sock} ->
			    Data = term_to_binary({search, Results}),
			    send(Sock, Data),
			    gen_tcp:close(Sock);
			_ -> io:format("Socket closed\n", []),
			     ok
		    end;
		[] -> ok
	    end;

	{db, removefriend, NetInfo, ok} -> 					
	    case NetInfo of
		[Ip, Port] ->
		    case gen_tcp:connect(Ip, Port, []) of
			{ok, Sock} ->
			    Data = term_to_binary({removefriend, ok}),
			    send(Sock, Data),
			    gen_tcp:close(Sock);
			_ -> io:format("Socket closed\n", []),
			     ok
		    end;
		[] -> ok
	    end;	%friend succesfully removed

	{db, changename, NetInfo, ok} -> 
	    case NetInfo of
		[Ip, Port] ->
		    case gen_tcp:connect(Ip, Port, []) of
			{ok, Sock} ->
			    Data = term_to_binary({changename, ok}),
			    send(Sock, Data),
			    gen_tcp:close(Sock);
			_ -> io:format("Socket closed\n", []),
			     ok
		    end;
		[] -> ok
	    end;	%user succesfully changed name

	{db, changepass, NetInfo, ok} -> 
	    case NetInfo of
		[Ip, Port] ->
		    case gen_tcp:connect(Ip, Port, []) of
			{ok, Sock} ->
			    Data = term_to_binary({changepass, ok}),
			    send(Sock, Data),
			    gen_tcp:close(Sock);
			_ -> io:format("Socket closed\n", []),
			     ok
		    end;
		[] -> ok
	    end;	%user succesfullt changed password

	{db, acceptfriend, MyFrList, FriendFrList, MyInfo, FriendInfo} ->
	    case MyInfo of
		[MyIp, MyPort] ->
		    case gen_tcp:connect(MyIp, MyPort, []) of
			{ok, MySock} ->
			    MyData = term_to_binary({friendlist, MyFrList}),
			    send(MySock, MyData),
			    gen_tcp:close(MySock);
			_ -> io:format("Socket closed\n", []),
			     ok
		    end;
		[] -> ok
	    end,
	    case FriendInfo of
		[FriendIp, FriendPort] ->
		    case gen_tcp:connect(FriendIp, FriendPort, []) of
			{ok, FriendSock} ->
			    FriendData = term_to_binary({friendlist, FriendFrList}),
			    send(FriendSock, FriendData),
			    gen_tcp:close(FriendSock);
			_ -> io:format("Socket closed\n", []),
			     ok
		    end;
		[] -> ok
	    end;

	{db, adduser, Netinfo, Mail, Password} -> 		
	    case Netinfo of
		[Ip, Port] ->
		    case gen_tcp:connect(Ip, Port, []) of
			{ok, Sock} ->
			    Data = term_to_binary({adduser, Mail}),
			    send(Sock, Data),
			    gen_tcp:close(Sock);
			_ -> io:format("Socket closed\n", []),
			     ok
		    end;
		[] -> ok
	    end,
	    spawn(smtp, reminder, [Mail, "Welcome to Pear-Talk, Pear-Talk is back!", Mail, Password]);

	{db, removeuser, Netinfo, ok} -> 
	    case Netinfo of
		[Ip, Port] ->
		    case gen_tcp:connect(Ip, Port, []) of
			{ok, Sock} ->
			    Data = term_to_binary({removeuser, ok}),
			    send(Sock, Data),
			    gen_tcp:close(Sock);
			_ -> io:format("Socket closed\n", []),
			     ok
		    end;
		[] -> ok
	    end;

	{db, reminder, ID,Netinfo, Password} ->
	spawn(smtp, reminder, [ID, "Cake", ID, Password]),
	case Netinfo of
		[Ip, Port] ->
		    case gen_tcp:connect(Ip, Port, []) of
			{ok, Sock} ->
			    Data = term_to_binary({reminderSent, ID}),
			    send(Sock, Data),
			    gen_tcp:close(Sock);
			_ -> io:format("Socket closed\n", []),
			     ok
		    end;
		[] -> ok
	    end;

	{db, noUser, Netinfo} ->
	case Netinfo of
		[Ip, Port] ->
		    case gen_tcp:connect(Ip, Port, []) of
			{ok, Sock} ->
			    Data = term_to_binary({noUser, ok}),
			    send(Sock, Data),
			    gen_tcp:close(Sock);
			_ -> io:format("Socket closed\n", []),
			     ok
		    end;
		[] -> ok
	    end;
	    
	    {db, usedID, Netinfo}->
	    	case Netinfo of
		[Ip, Port] ->
		    case gen_tcp:connect(Ip, Port, []) of
			{ok, Sock} ->
			    Data = term_to_binary({usedID, ok}),
			    send(Sock, Data),
			    gen_tcp:close(Sock);
			_ -> io:format("Socket closed\n", []),
			     ok
		    end;
		[] -> ok
	    end
	    
	end.

send(Socket, <<Chunk:100/binary, Rest/binary>>) ->
    gen_tcp:send(Socket, Chunk),
    send(Socket, Rest);
send(Socket, Rest) ->
    gen_tcp:send(Socket, Rest).
