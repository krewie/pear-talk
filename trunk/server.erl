-module(server).
-compile(export_all).
-define(TCP_OPTIONS, [binary, {active, false}]).
-define(PORT, 9945).
-define(TIMEOUT, 12000).

%Server calls
% ------- funktioner att implementera för servernätverk

%join_network(server) ->

% --------

start_server() ->
    {ok, ListenSock} = gen_tcp:listen(?PORT, ?TCP_OPTIONS),
    DBpid = spawn(serv_ul, start, []),
    spawn(?MODULE, wait_for_connection, [ListenSock, DBpid]).
	 
wait_for_connection(ListenSocket, DBPid) ->
    case gen_tcp:accept(ListenSocket) of
	 	% när någon connectar spawnar vi en process som hanterar meddelanden från den socketen,
	 	% samtidigt som vi loopar och väntar på fler connections.
	{ok, Socket} ->
		case gen_tcp:recv(Socket, 0) of
			{ok, Packet} ->
				Lpid = spawn(?MODULE, listen_state, [Socket, DBPid]), 
				Lpid ! binary_to_term(Packet);
			{error, _ } -> ok
		end;
	{error, _ } -> ok
    end,
    wait_for_connection(ListenSocket, DBPid).


listen_state(Socket, DBPid) ->
    io:format("recieved request~n"),
    receive
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
		%friend succesfully added
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
		
	{db, acceptfriend, MyFrList, FriendFrList, MyInfo, FriendInfo} -> 0;
		%friend accept
	{db, removefriend, NetInfo, ok} -> 0;
		%friend succesfully removed
	{db, changename, NetInfo, ok} -> 0;
		%user succesfully changed name
	{db, changepass, NetInfo, ok} -> 0
    end.
	
send(Socket, <<Chunk:100/binary, Rest/binary>>) ->
    gen_tcp:send(Socket, Chunk),
    send(Socket, Rest);
send(Socket, Rest) ->
    gen_tcp:send(Socket, Rest).
	