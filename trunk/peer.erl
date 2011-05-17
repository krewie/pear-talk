%% @author Staffan Rodgren.
%% @doc First draft of the chat window.
%% @copyright 2011 Peer-talk.

-module(peer).
-compile(export_all). 
-export([start/0,shut_down/0,host_info/0,get_status/0,mess/2,ping/1,send_file/3,pingon/0,pingoff/0, send/4,friends/0,sendFr/3]).

-spec start() -> list().
%% @doc <h4>start()</h4> Starts the client and creates an empty ets called friends that is the default friend list 
start() ->
	try		
		
		{ok, File} = file:open("chat.ini", read),

		Me = (lists:nthtail(9, io:get_line(File, "")) -- " ") -- "\n",
		Vsn = (lists:nthtail(9, io:get_line(File, "")) -- " ") -- "\n",
		PublicIp = read_address((lists:nthtail(9, io:get_line(File, "")) -- " ") -- "\n"),
		ListenPortS = io:get_line(File, ""),
		ServerAddressS = (lists:nthtail(14, io:get_line(File, "")) -- " ") -- "\n",
		ServerAddress = read_address(ServerAddressS),
		ServerPortS = io:get_line(File, ""),

		{_,[{_,_,_},{_,_},{_,_,ListenPort}],_} = erl_scan:string(ListenPortS--"\n"),
		{_,[{_,_,_},{_,_},{_,_,ServerPort}],_} = erl_scan:string(ServerPortS--"\n"),
	
		{ok,{_, _, _, _, _, [NetworkInterface|_]}}= host_info(),
		
		file:close(File),

		register(chat_server, spawn(peer, server, [NetworkInterface, ListenPort])),
		
				
		register(chat, spawn(peer, status, [{PublicIp, NetworkInterface,
			ListenPort, {Me, Vsn}, ServerAddress, ServerPort, [], pingoff}])),
		
		chat!newfriends,	

		peer:addme(),

		register(ping_pong, spawn(peer, ping_loop, [])),
		
		
		try
			{ok, Sock} = gen_tcp:connect(ServerAddress, ServerPort, [binary,{active, true}]),
			gen_tcp:send(Sock, term_to_binary({client, login, PublicIp, ListenPort}))
		catch _:_ ->
			io:format("Connection to server ~p on port ~p failed!! ~n",[ServerAddressS,ServerPort])
		end

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


-spec mess(string(),string()) -> 
	term().
%% @doc <h4>mess(Receiver , String)</h4>  sends String to Receiver if Receiver is in the friend list  

mess(Receiver, Data) ->	chat!{send, Receiver, Data, [], []}, sent.


%% @doc <h4>ping(Receiver)</h4>  pings Receiver if Receiver is in the friend list  
-spec ping(string()) -> 
	term().
ping(Receiver) -> chat!{send, Receiver, [], ping, []}, sent.


-spec send_file(string(),string(),string()) -> 
	term().
%% @doc <h4>send_file(Receiver, Path, Name)</h4> sends the file Path ++ Name to Receiver if Receiver is in the friend list
send_file(Receiver, Path, Name) ->
	try
		{ok,Bin} = file:read_file([Path ++ Name]),
		chat!{send, Receiver, [], file, {Name, Bin}},
		sent
	catch _:_ ->
		{error, file_not_found}
	end.


-spec pingon() -> 
	term().
%% @doc <h4>pingon()</h4> allows you to see who pings you.
pingon() ->
	chat!{status,self()},
	receive
		{PublicIp, NetworkInterface,
				ListenPort, Me, ServerAddress, ServerPort, friends, _} ->
		chat!{change,{PublicIp, NetworkInterface,
					ListenPort, Me, ServerAddress, ServerPort, friends, pingon}},
		pingon
	end.



-spec pingoff() -> 
	term().
%% @doc <h4>pingoff()</h4> disallows you to see who pings you.
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
-spec status(term()) -> 
	nil().
%% @doc <h4>status(Status)</h4> starts a process that will hold Status.
status(Status) ->
    receive
	newfriends ->
		rul:friends(),
		{PublicIp, NetworkInterface,
				ListenPort, Me, ServerAddress, ServerPort, _, PingMode} = Status,
		status({PublicIp, NetworkInterface,
				ListenPort, Me, ServerAddress, ServerPort, friends, PingMode});
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
			 {ok,{SendingAddress, SendingPort}} = inet:peername(Socket),
    			get_request(SendingAddress, SendingPort, Socket,[]),
			wait_connect(ListenSocket);
		{error, _} ->
			"connection refused!"
	end.


-spec get_request(term(), integer(), term(), binary()) -> 
	term().
%% @doc <h4>get_request(SendingAddress, SendingPort, Socket, BinaryList)</h4> process the request BinaryList incoming on Socket.
get_request(SendingAddress, SendingPort, Socket, BinaryList) ->
	case gen_tcp:recv(Socket,0) of
		{ok, Binary} ->
			get_request(SendingAddress, SendingPort, Socket, [Binary|BinaryList]);
      	{error, closed} ->
			{{Y,M,D},{H,MM,S}} = erlang:localtime(),
			Timestamp = lists:flatten(io_lib:format("~p/~p/~p - ~p:~p:~p", [Y,M,D,H,MM,S])),
			try
				{PublicIp, Lp, {Sd, Vn}, {_R,RVn}, Data, Mode, _ServerIp, _ServerPort, Bin} = 
					binary_to_term(list_to_binary(lists:reverse(BinaryList))),
				case Mode of
					confirmfriend ->
						chat!{status, self()},
						receive 
							{_, _, _, _, _, _, FriendList, _}->
								ets:insert(FriendList, {Sd, [Vn, PublicIp, Lp]})
						end;
					befriends ->
						spawn(peer,acceptFr,[Sd,Vn,PublicIp,Lp]);
					friendlist ->
						chat!{status, self()},
						receive
							{_, _, _, _, _, _, FriendList, _} ->
									rul:fillTable(FriendList, Bin),
									io:format("The friend list has been updated!~n") 
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
								rul:set_online(FriendList, Sd, PublicIp, Lp)
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
							{_, _, _, _, _, _, _,PingMode}  -> ok
						end,
								
						if 
							PingMode == pingon ->
								io:format("pong from ~p ~n", [Sd]);
							true ->
								[]
						end;
					_Any ->
						chat!{status, self()},
									
			      		io:format("~p~n", [Vn ++ " to " ++ RVn ++ ": " ++ Data]),
						file:write_file("log_file.txt", Timestamp ++ " " ,[append]),
     		  				file:write_file("log_file.txt", Vn ++ " to " 
							++ RVn ++ ": " ++ Data ++ "\n",[append])
				end
			catch
				Ek:En -> {Ek,En}
			end
     		end.
   
%%--------------------------------------------------------------------------------------------------
-spec send(string(), string(), atom(), any()) -> 
	term().
%% @doc <h4>send(Receiver, Data, Mode, Obj)</h4> sends Data, Mode and Obj to Receiver if Receiver is in the friend list,
%%Mode specifies for Receiver what kind of data that is beeing sent.<br>When Mode is set to file, friendlist, 
%%ping or pong, Receiver will neglect Data and process Obj that will be supposed to contain respectively
%%a file a friend list or an empty list in the case of a ping or a pong.</br>

send(Receiver, Data, Mode, Obj) ->
	E = [],
	chat!{status, self()},
	receive
		{PublicIp, _, Lp, {Sd,Vn}, ServerIp, ServerPort, FriendList, _}  ->
		try
			[RVn,IpAddress, RemotePort] = rul:take(FriendList, Receiver),
			case gen_tcp:connect(IpAddress, RemotePort, [binary,{packet,0},{port, 0}]) of
				{ok, Sock} ->
					try
						{{Y,M,D},{H,MM,S}} = erlang:localtime(),
						Timestamp = lists:flatten(io_lib:format(
							"~p/~p/~p - ~p:~p:~p",[Y,M,D,H,MM,S])),
						if Data /= "" ->
							
							file:write_file("log_file.txt", Timestamp ++ " " ,[append]),
	  						file:write_file("log_file.txt", Vn ++ " to " ++ RVn ++ ": " 
								++ Data ++ "\n",[append]);
						true ->
							ok			
						end,
						sendB(Sock, term_to_binary({PublicIp, Lp, {Sd,Vn}, {Receiver, RVn}, Data, Mode, 
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
	chat!{status, self()},
	receive
		{_, _ ,_ ,_ , _, _, FriendList, _}  ->
			ping_friends(FriendList)
	end,
	timer:sleep(10000),
	ping_loop().


-spec read_address(string())-> 
	any().
%% @doc <h4>read_address(Address)</h4> reads Address and if address is a domain it returns Address 
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
		chat!{status, self()},
		receive
			{PIp, _,ListenPort, {Me, Vn}, 
						_, _, FriendList,_} ->
			rul:add(FriendList, Me, Vn), 
			rul:set_online(friends, Me, PIp, ListenPort)			
		end
	catch Ek:En ->
		{Ek, En}
	end.

-spec friends()-> 
	any().
%% @doc <h4>friends()</h4> shows the friend list.
friends() ->
	chat!{status, self()},
	receive
		{_, _ ,_ ,_ , _, _, FriendList, _}  ->
			rul:show(FriendList)
	end.

-spec acceptFr(string(),string(),term(),integer())-> 
	any().
%% @doc <h4>acceptFr(Sd,Vn,PublicIp,Lp)</h4> asks if you accept Sd as friend.
acceptFr(Usr,Vn,PublicIp,Lp) ->
	Line = io_lib:format("accept friend request from ~p ~p (y/n)? ", [Usr,Vn]),
	case io:get_line(Line) of
		"y\n" ->
			chat!{status, self()},
			receive {_, _, _, _, _, _, FriendList, _} ->
				rul:add(FriendList, Usr, Vn),
				rul:set_online(FriendList,Usr,PublicIp,Lp),
				send(Usr, [], confirmfriend, [])
			end;
		"n\n" ->
			io:format("Friend request denied.~n");
		_ ->
			acceptFr(Usr,Vn,PublicIp,Lp)
	end.

-spec sendFr(string(),term(),integer())-> 
	any().
%% @doc <h4>sendFr(Friend_username, Friend_public_ip, Friend_listen_port)</h4> Sends a friend request to your friend.
sendFr(R, PublicIp, Lp) ->
	chat!{status, self()},
	receive {_, _, _, {Usr,Vn}, _, _, FriendList, _} ->
		rul:add(FriendList, R, "unknown"),
		rul:set_online(FriendList,R,PublicIp,Lp),
		send(R, [], befriends, {Usr,Vn}),
		rul:logout(FriendList,R)
	end.
