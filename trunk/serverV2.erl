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
	 wait_for_connection(ListenSock).
	 
wait_for_connection(ListenSocket) ->
	 case gen_tcp:accept(ListenSock) of
	 	% när någon connectar spawnar vi en process som hanterar meddelanden från den socketen,
	 	% samtidigt som vi loopar och väntar på fler connections.
	 	{ok, Socket} -> spawn(?MODULE, listen_state, [Socket]),
	 					wait_for_connection(ListenSocket);
		_ -> ok
	end.


listen_state(Socket) ->
	receive
		{tcp, Socket, Packet} ->
			case binary_to_term(Packet) of
		%Client-calls
				{client, login, {info-om-anv}} ->
					% moduler user implementerar DETS, funktionerna online/1, get_friendlist/1 och add/2 
					% behöver skrivas...
					{ok ,Friendlist} = users:get_friendlist(ID),
					users:online(ID),
					gen_tcp:send(Socket, {server, friendlist, Friendlist})
					listen_state(Socket);
		
				{client, add_friend, MyID, FriendID} ->
					users:add(MyID, FriendID),
					listen_state(Socket)
		%Server-calls
				{server, ping} ->
		
				{server, network_request} ->
			end		
	after ?TIMEOUT ->
			gen_tcp:close(Socket)
	end.
	
	