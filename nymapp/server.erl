-module(server).
-compile(export_all).
-define(TCP_OPTIONS, [binary, {active, true}]).
-define(PORT, 8888).
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
				{client, login, ID, Port} ->
					{ok, {Address, _}} = inet:peername(Socket),
					DBPid ! {client, login, ID, Address, Port, self()},
					listen_state(Socket, DBPid);
				{client, addfriend, {MyID, FriendID}} ->
					DBPid ! {client, addfriend, {MyID, FriendID}, self()};
				{client, removefriend, {MyID, FriendID}} ->
					DBPid ! {client, removefriend, {MyID, FriendID}, self()};
				{client, changename, ID, Name} ->
					DBPid ! {client, changename, ID, Name, self()}
		%Server-calls
				%{server, serverlogin, {Netinfo}} ->
					%se till så att servern blir up-to-date
				%{server, userinfo, {server, client, Netinfo}} ->
					%skicka tillbaka clientinfo till server som ännu inte har infon
					%funkar för clientlogin likaså när man skapar vänlistan
				%{server, 
				
			end;
			
		{db, friendlist, Friendlist} ->
				io:format("sending friendlist: ~w to ~w~n", [Friendlist, Socket]),
				Data = term_to_binary({[], [], [], [], [], friendlist, [], [], Friendlist}),
				send(Socket, Data)
		%{db, addfriend, ok} ->
			%friend succesfully added
		%{db, removefriend, ok} ->
			%friend succesfully removed
		%{db, changename, ok} ->
			%user succesfully changed name
			
	%after ?TIMEOUT ->
	%		gen_tcp:close(Socket)	
	end,
	listen_state(Socket, DBPid).
	
	send(Socket, <<Chunk:100/binary, Rest/binary>>) ->
		gen_tcp:send(Socket, Chunk),
		send(Socket, Rest);
	send(Socket, Rest) ->
		gen_tcp:send(Socket, Rest).
	