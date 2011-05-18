%% @author Kristian Ionesku & Staffan Rodgren.
%% @doc The module for the peer client.
%% @copyright 2011 Peer-talk.

-module(peer).
-compile(export_all). 
-export([start/0,shut_down/0,host_info/0,get_status/0,mess/2,ping/1,send_file/3,pingon/0,pingoff/0, send/3,friends/0,sendFr/3,my/1,read_address/1]).

-spec start() -> list().
%% @doc <h4>start()</h4> Starts the client and creates an empty ets called friends that is the default friend list 
start() ->
	try		

		Me = "guest",
		Vsn = Me,
		ListenPortS = io:get_line("Insert your local port: "),
		ServerAddressS = io:get_line("Insert the address of server to connect: ") -- "\n",
		ServerAddress = read_address(ServerAddressS),
		
		{ok,[{integer,1,ListenPort}],1} = erl_scan:string(ListenPortS--"\n"),
		
		ServerPort = 9997,

		{ok,{_, _, _, _, _, [NetworkInterface|_]}}= host_info(),

		register(chat_server, spawn(peer, server, [NetworkInterface, ListenPort])),
					
		register(chat, spawn(peer, status, [[{network_interface, NetworkInterface},
			{listen_port, ListenPort}, {id,{Me, Vsn}}, {server_address, ServerAddress}, 
				{server_port, ServerPort}, {ping_mode, pingoff}]])),
		
		chat!newfriends,	

		peer:addme(),

		register(ping_pong, spawn(peer, ping_loop, [])),
		
		login_frame:start("username:")
		
		
	catch Ek:En ->
			{Ek, En}
			
	end.


-spec shut_down() -> 
	term().
%% @doc <h4>shut_down()</h4> stops the client
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


-spec host_info() -> 
	term().
%% @doc <h4>host_info()</h4> gives information about your computer
host_info() -> inet:gethostbyname(net_adm:localhost()). 

-spec get_status() -> 
	term().
%% @doc <h4>get_status()</h4> gives information about your status

get_status()->
	chat!{status, self()},
	receive 
		Status ->
			Status
	end.
	
	
-spec my(atom()) -> 
	term().
%% @doc <h4>my(Status_element)</h4> returns the value of Status_element
my(Status_element) -> 
	case	lists:keysearch(Status_element, 1, get_status()) of
		false ->
			{error, nomatch};
		{value, {_, V}} ->
			V
	end.


-spec mess(string(),string()) -> 
	term().
%% @doc <h4>mess(Receiver , String)</h4>  sends String to Receiver if Receiver is in the friend list  

mess(Receiver_username, String) ->	
	try
		{My_username, _} = my(id),
		[Receiver_showed_name, _, _] = rul:take(friends, Receiver_username),
		chat!{send, Receiver_username, string,{My_username, String}}, 
		{{Y,M,D},{H,MM,S}} = erlang:localtime(),
		Timestamp = lists:flatten(io_lib:format("~p/~p/~p - ~p:~p:~p",[Y,M,D,H,MM,S])),
		file:write_file("log_file.txt", Timestamp ++ " " ,[append]),
	  	file:write_file("log_file.txt", "me to " ++ Receiver_showed_name ++ ": " 
			++ String ++ "\n",[append]),
		sent
	catch 
		Ek:En ->
			{Ek,En}
	end.


%% @doc <h4>ping(Receiver)</h4>  pings Receiver if Receiver is in the friend list  
-spec ping(string()) -> 
	term().
ping(Receiver) -> 
	{My_username, _} = my(id),
	chat!{send, Receiver, ping, {my(listen_port), My_username}}, 
	sent.


-spec send_file(string(),string(),string()) -> 
	term().
%% @doc <h4>send_file(Receiver, Path, Name)</h4> sends the file Path ++ Name to Receiver if Receiver is in the friend list
send_file(Receiver, Path, Name) ->
	try
		{ok, File} = file:read_file([Path ++ Name]),
		chat!{send, Receiver, file, {Name, File}},
		sent
	catch _:_ ->
		{error, file_not_found}
	end.


-spec pingon() -> 
	term().
%% @doc <h4>pingon()</h4> allows you to see who pings you.
pingon() ->
	chat!{change, ping_mode, pingon}.



-spec pingoff() -> 
	term().
%% @doc <h4>pingoff()</h4> disallows you to see who pings you.
pingoff() ->
	chat!{change, ping_mode, pingoff}.

%%-----------------------------------------------------------------------------------------------
-spec status(term()) -> 
	nil().
%% @doc <h4>status(Status)</h4> starts a process that will hold Status.
status(Status) ->
    receive
    {chat_send, Pid, Message} ->
    	{value, {Receiver, _}} = lists:keysearch(Pid, 2, Status),
    	spawn(peer, send, [Receiver, string, {"ME", Message}]),
    	status(Status);
    {chat_window, Receiver} ->
    	Pid = spawn(chat_frame, start, []),
    	NewStatus=lists:keystore(Receiver, 1, Status, {Receiver, Pid}),
    	status(NewStatus);
	{login, {Username, Password}}  ->
		spawn(peer,autentication,[Username, Password]),
		spawn(contacts, start, []),
		status(lists:keystore(id, 1, Status, {id, {Username,Username}}));
	newfriends ->
		rul:friends(),
		status(lists:keystore(friend_list, 1, Status, {friend_list, friends}));
        {status, Pid} ->
            Pid!Status,
            status(Status);
    {change, Status_element, New_status} ->
		status(lists:keystore(Status_element, 1, Status, {Status_element, New_status}));
	{send, Receiver, Mode, Obj} ->
            	spawn(peer,send,[Receiver, Mode, Obj]),
		status(Status);
        stop ->
            true;
        _Any->
            status(Status)
    end.

-spec server(term(), integer()) -> 
	term().
%% @doc <h4>server(NetworkInterface, ListenPort)</h4> starts alistening proces on NetworkInterface:ListenPort.
server(NetworkInterface, ListenPort)->
	case gen_tcp:listen(ListenPort, [binary, {active, false},{ip, NetworkInterface}]) of
		{ok, ListenSocket} ->
			wait_connect(ListenSocket),
			server(NetworkInterface, ListenPort);
		{error, _} ->
			"error on server start up!"
	end.

-spec wait_connect(term()) -> 
	term().
%% @doc <h4>wait_connect(ListenSocket)</h4> starts a proces waiting for a connection on ListenSocket.
wait_connect(ListenSocket) ->
	case gen_tcp:accept(ListenSocket) of
		{ok, Socket} ->
			 {ok,{Sender_address, _}} = inet:peername(Socket),
    			get_request(Sender_address,Socket,[]),
			wait_connect(ListenSocket);
		{error, _} ->
			"connection refused!"
	end.


-spec get_request(term(), term(), binary()) -> 
	term().
%% @doc <h4>get_request(Sender_address, Socket, BinaryList)</h4> process the request BinaryList incoming on Socket.
get_request(Sender_address, Socket, BinaryList) ->
	case gen_tcp:recv(Socket,0) of
		{ok, Binary} ->
			get_request(Sender_address, Socket, [Binary|BinaryList]);
      	{error, closed} ->
			{{Y,M,D},{H,MM,S}} = erlang:localtime(),
			Timestamp = lists:flatten(io_lib:format("~p/~p/~p - ~p:~p:~p", [Y,M,D,H,MM,S])),
			try
				{Mode, Obj} = 
					binary_to_term(list_to_binary(lists:reverse(BinaryList))),
				case Mode of
					confirmfriend ->
						{Sender_listen_port, Sender_username, Sender_showed_name} = Obj,
						ets:insert(friends, {Sender_username, [Sender_showed_name, Sender_address, 								Sender_listen_port]});
					befriends ->
						{Sender_listen_port, Sender_username, Sender_showed_name} = Obj,
						spawn(peer,acceptFr,[Sender_username, Sender_showed_name, Sender_address, 								Sender_listen_port]);
					friendlist ->						
						{Showed_name, FriendList} = Obj,
						{Username,_} = my(id),
						chat!{change,id,{Username,Showed_name}},
						rul:fillTable(friends, FriendList),
						io:format("The friend list has been updated!~n"); 

					file ->
						{FileName, File} = Obj,
						file:write_file(FileName, File),
						io:format("File ~p written on disk~n", [FileName]),
						file:write_file("log_file.txt", Timestamp ++ " " ,[append]),
						file:write_file("log_file.txt", "File " ++ FileName 
							++ " written on disk",[append]);
					ping ->
						{Sender_listen_port, Sender_username} = Obj,
						rul:set_online(friends, Sender_username, Sender_address, Sender_listen_port),
						chat!{send, Sender_username, pong, my(id)},
						case my(ping_mode) of
							pingon ->
								io:format("ping from ~p ~n", [Sender_username]);
							_ ->
								[]
						end;
				   	pong -> 
				   		{Sender_username, _} = Obj,
						case my(ping_mode) of
							pingon ->
								io:format("pong from ~p ~n", [Sender_username]);
							_ ->
								[]
						end;

					string ->
						{Sender_username, String} = Obj,
						[Sender_showed_name ,_, _] = rul:take(friends, Sender_username),
			      			io:format("~p~n", [Sender_showed_name ++ " to " ++ "me: " ++ String]),
						file:write_file("log_file.txt", Timestamp ++ " " ,[append]),
     		  				file:write_file("log_file.txt", Sender_showed_name ++ " to me:" ++ ": " 
							++ String ++ "\n",[append]),
							{value, {_, Pid}} = lists:keysearch(Sender_username, 1, get_status()),
							 Pid ! {message_received, Sender_showed_name, String};
					_Any -> []
				end
			catch
				Ek:En -> {Ek,En}
			end
     		end.
   
%%--------------------------------------------------------------------------------------------------
-spec send(string(), atom(), any()) -> 
	term().
%% @doc <h4>send(Receiver, Mode, Obj)</h4> sends Mode and Obj to Receiver if Receiver is in the friend list,
%%Mode specifies for Receiver what kind of data that is beeing sent.<br>When Mode is set to file, friendlist, 
%%ping or pong, Receiver will neglect Data and process Obj that will be supposed to contain respectively
%%a file a friend list or an empty list in the case of a ping or a pong.</br>
send(Receiver_username, Mode, Obj) ->
		try
			[_ ,Receiver_address, Receiver_listen_port] = rul:take(friends, Receiver_username),
			{ok, Socket} = gen_tcp:connect(Receiver_address, Receiver_listen_port, 
						[binary,{packet,0},{port, 0}]),
			sendB(Socket, term_to_binary({Mode, Obj})),
			gen_tcp:close(Socket)
		catch 
			Ek:En -> 
				{Ek,En}
		end.

-spec sendB(term(), binary()) -> 
	nil().
%% @doc <h4>sendB(Socket,Bin)</h4> sends Bin to sockets divided in 100bit-chunks. 
sendB(Socket, <<Chunk:100/binary, Rest/binary>>) ->
    gen_tcp:send(Socket, Chunk),
    sendB(Socket, Rest);
sendB(Socket,Rest) ->
    gen_tcp:send(Socket, Rest).


-spec ping_friends(atom())-> 
	any().
%% @doc <h4>ping_friends(Table)</h4> pings all the friends in Table. 
ping_friends(Table) ->
	try 
		rul:traverse(Table, (fun([{R, _}|_L]) -> ping(R) end))
	catch 
		_:_ ->
			error
	end.

-spec ping_loop()-> 
	any().
%% @doc <h4>ping_loop()</h4> start a process that pings all the friends in Table every 10000ms. 
ping_loop() ->
	ping_friends(friends),
	timer:sleep(10000),
	ping_loop().


-spec read_address(string())-> 
	any().
%% @doc <h4>read_address(Address)</h4> reads Address and if Address is a domain it returns Address 
%%if Address is an ip-address of the type xx.yy.zz.ww  it returns a tuple of the 
%%type {xx,yy,zz,ww}
read_address(Address) ->
	case inet_parse:address(Address) of
		{ok,Ip} ->
			Ip;
		_ -> 
			Address
	end.
		
-spec addme()-> 
	any().
%% @doc <h4>addme()</h4> adds the owner of the client process to the friend list.
addme() ->
	try
		{Me, Vn} = my(id),
		rul:add(friends, Me, Vn), 
		rul:set_online(friends, Me, my(network_interface), my(listen_port))			
	catch Ek:En ->
		{Ek, En}
	end.

-spec friends()-> 
	any().
%% @doc <h4>friends()</h4> shows the friend list.
friends() -> rul:show(friends).

-spec acceptFr(string(),string(),term(),integer())-> 
	any().
%% @doc <h4>acceptFr(Sd,Vn,PublicIp,Lp)</h4> asks if you accept Sd as friend.
acceptFr(Sender_username, Sender_showed_name, Sender_address, Sender_listen_port) ->
	Line = io_lib:format("accept friend request from ~p ~p (y/n)? ", [Sender_username, Sender_showed_name]),
	case io:get_line(Line) of
		"y\n" ->
			{My_username,My_showed_name} = my(id),
			rul:add(friends, Sender_username, Sender_showed_name),
			rul:set_online(friends, Sender_username, Sender_address, Sender_listen_port),
			chat!{send,Sender_username, confirmfriend, {my(listen_port),My_username,My_showed_name}};
		"n\n" ->
			io:format("Friend request denied.~n");
		_ ->
			acceptFr(Sender_username, Sender_showed_name, Sender_address, Sender_listen_port)
	end.

-spec sendFr(string(), term(),integer())-> 
	any().
%% @doc <h4>sendFr(Receiver_username, Friend_public_ip, Friend_listen_port)</h4> Sends a friend request to your friend.
sendFr(Receiver_username, Receiver_address, Receiver_listen_port) ->
	{My_username, My_showed_name} = my(id),
	rul:add(friends, Receiver_username, "unknown"),
	rul:set_online(friends, Receiver_username, Receiver_address, Receiver_listen_port),
	send(Receiver_username, befriends, {my(listen_port), My_username, My_showed_name}),
	rul:logout(friends, Receiver_username).

autentication(Username, Password) ->
	try
		{ok, Sock} = gen_tcp:connect(my(server_address), my(server_port), [binary,{active, false}]),
		gen_tcp:send(Sock, term_to_binary({client,login, Username, Password, my(listen_port)}))
	catch 
		Ek:En ->
			{Ek,En}
	end.

