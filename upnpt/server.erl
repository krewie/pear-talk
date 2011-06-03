%% @author Grupp 2 (Staffan Rodgren, Gabriel Tholsgård, Kristian Ionescu, Mårten Blomberg, Göran Hagelin, Staffan Reinius)
%% @doc Servermodul för pear-talk. 

-module(server).
-compile(export_all).
-define(TCP_OPTIONS, [binary, {active, false}]).
-define(PORT, 9945).
-define(TIMEOUT, 12000).

%%Server calls
%% ------- funktioner att implementera för servernätverk

%%join_network(server) ->

%% --------

%% @doc Anropar servern för att skriva ut användarlistan.
%% @spec showAll() -> ok | {error, Reason}
showAll() ->
    global ! {server, getport, self()},
    receive
	{server, getport, Port} ->
	    {ok, Con} = gen_tcp:connect({127,0,0,1}, Port, []),
	    send(Con, term_to_binary({server, showAll})),
	    gen_tcp:close(Con)
    end.

%% @doc Stänger av servern.
%% @spec shut_down() -> | {error, Reason}s
shut_down() ->
    global ! {server, getport, self()},
    receive
	{server, getport, Port} ->
	    {ok, Con} = gen_tcp:connect({127,0,0,1}, Port, []),
	    send(Con, term_to_binary({server, shutdown})),
	    gen_tcp:close(Con)
    end.

%% @doc Returnerar serverns port Port till anropande Pid
%% @spec globalPort(Port) -> ok | {error, Reason}
globalPort(Port) ->
    receive
	{server, getport, Pid} ->
	    Pid ! {server, getport, Port}
    end.

%% @doc Initierar server.
%% @spec start_server(Port) -> bool()
%% Port = integer()
start_server(Name) ->
	Port = upnp:server(Name),
    {ok, ListenSock} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    DBpid = spawn(serv_ul, start, []),
    spawn(?MODULE, wait_for_connection, [ListenSock, DBpid]),
    register(global, spawn(?MODULE, globalPort, [Port])).

%% @doc Väntar på uppkoppling, spawnar process för att hantera uppkopplingen. 
%% @spec wait_for_connect(ListenSocket, DBPid) -> ok | {error, Reason}
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
			{server, showAll} ->
			    DBPid ! Data,
			    wait_for_connection(ListenSocket, DBPid);
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

%% @doc Hanterar ett client request genom att kontakta DB och därefter invänta svar för hantering
%% @spec listen_state(Socket, DBPid) -> ok | {error, Reason}
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

	%%Server-calls
	%%{server, serverlogin, {Netinfo}} ->
	%%se till så att servern blir up-to-date
	%%{server, userinfo, {server, client, Netinfo}} ->
	%%skicka tillbaka clientinfo till server som ännu inte har infon
	%%funkar för clientlogin likaså när man skapar vänlistan
	%%{server, 

	{db, friendlist, NetInfo, Friendlist} ->
	    io:format("sending friendlist: ~p to ~w~n", [Friendlist, NetInfo]),
	    Data = {friendlist, Friendlist},
	   [Receiver_username, _IP, _Port] = NetInfo,
	    
		    send(Receiver_username, Data),

		    io:format("Sent\n", []);


	{db, badPass, NetInfo} ->
	   [Receiver_username, _IP, _Port] = NetInfo,
	    
		    send(Receiver_username, {server_badlogin, badPass});


	{db, badID, NetInfo} ->
	   [Receiver_username, _IP, _Port] = NetInfo,
	    
		    send(Receiver_username, {server_badlogin, badID});
    		    
	{db, addfriend, NetInfo, FriendList, ok} ->
	    case NetInfo of
		[Receiver_username, _IP, _Port] ->

			    Data = {friendlist, FriendList},
			    send(Receiver_username, Data);

		[] -> ok
	    end;

	{db, search, NetInfo, Results} -> 
	    io:format("Search results: ~p~n", [Results]),
	    case NetInfo of
		[Receiver_username, _IP, _Port] ->

			    Data = {search, Results},
			    send(Receiver_username, Data);

		[] -> ok
	    end;

	{db, removefriend, NetInfo, ok} -> 					
	    case NetInfo of
		[Receiver_username, _IP, _Port] ->

			    Data = {removefriend, ok},
			    send(Receiver_username, Data);

		[] -> ok
	    end;	
	%%friend succesfully removed

	{db, changename, NetInfo, ok} -> 
	    case NetInfo of
		[Receiver_username, _IP, _Port] ->

			    Data = {changename, ok},
			    send(Receiver_username, Data);

		[] -> ok
	    end;	
	%%user succesfully changed name

	{db, changepass, NetInfo, ok} -> 
	    case NetInfo of
		[Receiver_username, _IP, _Port] ->

			    Data = {changepass, ok},
			    send(Receiver_username, Data);

		[] -> ok
	    end;	
	%%user succesfullt changed password

	{db, acceptfriend, MyFrList, FriendFrList, MyInfo, FriendInfo} ->
	    case MyInfo of
		[MyID, _MyIP, _MyPort] ->

			    MyData = {friendlist, MyFrList},
			    send(MyID, MyData);

		[] -> ok
	    end,
	    case FriendInfo of
		[FriendID, _FriendIP, _FriendPort] ->

			    FriendData = {friendlist, FriendFrList},
			    send(FriendID, FriendData);
		[] -> ok
	    end;

	{db, adduser, Netinfo, Mail, Password} -> 		
	    case Netinfo of
		[Receiver_username, _IP, _Port] ->

			    Data = {addUser, Mail},
			    send(Receiver_username, Data);
		[] -> ok
	    end,
	    spawn(smtp, reminder, [Mail, "Welcome to Pear-Talk, Pear-Talk is back!", Mail, Password]);

	{db, removeuser, Netinfo, ok} -> 
	    case Netinfo of
		[Receiver_username, _IP, _Port] ->

			    Data = {removeuser, ok},
			    send(Receiver_username, Data);

		[] -> ok
	    end;

	{db, reminder, ID,Netinfo, Password} ->
	    spawn(smtp, reminder, [ID, "Cake", ID, Password]),
	    case [ID|Netinfo] of
		[Receiver_username, _IP, _Port] ->

			    Data = {reminderSent, ID},
			    send(Receiver_username, Data);

		[] -> ok
	    end;

	{db, noUser, Netinfo} ->
	    case Netinfo of
		[Receiver_username, _IP, _Port] ->

			    Data = {noUser, ok},
			    send(Receiver_username, Data);

		[] -> ok
	    end;

	{db, usedID, Netinfo}->
	    case Netinfo of
		[Receiver_username, _IP, _Port] ->

			    Data = {usedID, ok},
			    send(Receiver_username, Data);

		[] -> ok
	    end

    end.

%% @doc Regulerar hur stora paketen som skickas skall vara, samt skickar delarna till Socket.
%% @spec send(Socket, Rest) -> ok | {error, Reason}
%% Rest = binary()
send(FriendID, Data) ->
   upnp!{message, FriendID, Data}.

