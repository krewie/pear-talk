-module(server).
-compile(export_all).
-define(TCP_OPTIONS, [binary, {active, true}]).
-define(PORT, 9945).
-define(TIMEOUT, 12000).

%Server calls
% ------- funktioner att implementera för servernätverk

%join_network(server) ->

% --------

start_server() ->
    {ok, ListenSock} = gen_tcp:listen(?PORT, ?TCP_OPTIONS),
    DBpid = spawn(serv_ul, start, []),
    wait_for_connection(ListenSock, DBpid).
	 
wait_for_connection(ListenSocket, DBpid) ->
    case gen_tcp:accept(ListenSocket) of
	 	% när någon connectar spawnar vi en process som hanterar meddelanden från den socketen,
	 	% samtidigt som vi loopar och väntar på fler connections.
	{ok, Socket} -> spawn(?MODULE, wait_for_connection, [ListenSocket, DBpid]),
			io:format("spawned process~n"),
			listen_state(Socket, DBpid);
	_ -> ok
    end.


listen_state(Socket, DBPid) ->
    receive
	{tcp, Socket, Packet} ->
	    io:format("recieved request~n"),
	    case binary_to_term(Packet) of
	    %Client-calls
		{client, login, Username, Password, Port} ->
		    {ok, {Address, _}} = inet:peername(Socket),
		    DBPid ! {client, login, Username, [Address, Port], Password, self()},
		    listen_state(Socket, DBPid);
		
		{client, addfriend, MyID, FriendID} ->
		    DBPid ! {client, addfriend, MyID, FriendID, self()},
		    listen_state(Socket, DBPid);
		
		{client, removefriend, MyID, FriendID} ->
		    DBPid ! {client, removefriend, MyID, FriendID, self()},
		    listen_state(Socket, DBPid);
		
		{client, changename, ID, Name} ->
		    DBPid ! {client, changename, ID, Name, self()},
		    listen_state(Socket, DBPid);
		
		{client, changepass, ID, New_password, Old_password} ->
		    DBPid ! {client, changepass, ID, New_password, Old_password, self()},
		    listen_state(Socket, DBPid)
	     %Server-calls
	       %{server, serverlogin, {Netinfo}} ->
	         %se till så att servern blir up-to-date
	       %{server, userinfo, {server, client, Netinfo}} ->
	         %skicka tillbaka clientinfo till server som ännu inte har infon
	         %funkar för clientlogin likaså när man skapar vänlistan
	       %{server, 
			
	    end;
	
	{db, friendlist, NetInfo, Friendlist} ->
	    io:format("sending friendlist: ~w to ~w~n", [Friendlist, NetInfo]),
	    Data = term_to_binary({friendlist, Friendlist}),
	    [Ip, Port] = NetInfo,
	    case gen_tcp:connect(Ip, Port, []) of
		{ok, Sock} ->
		    send(Sock, Data),
		    gen_tcp:close(Sock);
		_ -> ok
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
	
	{db, addfriend, NetInfo, ok} -> 0;
		%friend succesfully added
	{db, removefriend, NetInfo, ok} -> 0;
		%friend succesfully removed
	{db, changename, NetInfo, ok} -> 0;
		%user succesfully changed name
	{db, changepass, NetInfo, ok} -> 0
%	_ -> io:format("Something That isn't handled\n", [])
    end,
    gen_tcp:close(Socket).
	
send(Socket, <<Chunk:100/binary, Rest/binary>>) ->
    gen_tcp:send(Socket, Chunk),
    send(Socket, Rest);
send(Socket, Rest) ->
    gen_tcp:send(Socket, Rest).
	
