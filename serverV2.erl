-module(server).
-compile(export_all).
-define(TCP_OPTIONS, [binary]).
-define(PORT, 8888).
-define(TIMEOUT, 12000).

%Server calls
% ------- funktioner att implementera för servernätverk

join_network(server) ->

% --------

start_server() ->
	 {ok, ListenSock} = gen_tcp:listen(?PORT, ?TCP_OPTIONS),
	 DBpid = spawn(serv_ul, start, [self()])
	 wait_for_connection(ListenSock, DBpid).
	 
wait_for_connection(ListenSocket, DBpid) ->
	 case gen_tcp:accept(ListenSock) of
	 	% när någon connectar spawnar vi en process som hanterar meddelanden från den socketen,
	 	% samtidigt som vi loopar och väntar på fler connections.
	 	{ok, Socket} -> spawn(?MODULE, listen_state, [Socket, DBpid]),
	 					wait_for_connection(ListenSocket, DBpid);
		_ -> ok
	end.


listen_state(Socket, DBPid) ->
	receive
		{tcp, Socket, Packet} ->
			case binary_to_term(Packet) of
		%Client-calls
				{client, login, {ID, Netinfo}} ->
					DBPid ! {client, login, ID, Netinfo, self()};
				{client, addfriend, {MyID, FriendID}} ->
					DBPid ! {client, addfriend, {MyID, FriendID}, self()};
				{client, removefriend, {MyID, FriendID}} ->
					DBPid ! {client, removefriend, {MyID, FriendID}, self()};
				{client, changename, ID, Name} ->
					DBPid ! {client, changename, ID, Name, self()};
		%Server-calls
				{server, serverlogin, {Netinfo}} ->
					%se till så att servern blir up-to-date
				{server, userinfo, {Server, Client, Netinfo}} ->
					%skicka tillbaka clientinfo till server som ännu inte har infon
					%funkar för clientlogin likaså när man skapar vänlistan
				{server, 
				
			end
		{db, friendlist, Friendlist} ->
				gen_tcp:send(Socket, {server, friendlist, Friendlist});
		{db, addfriend, ok} ->
			%friend succesfully added
		{db, removefriend, ok} ->
			%friend succesfully removed
		{db, changename, ok} ->
			%user succesfully changed name
			
	after ?TIMEOUT ->
			gen_tcp:close(Socket)	
	end
	listen_state(Socket, DBPid).
	