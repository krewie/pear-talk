%% @author Kristian Ionesku & Staffan Rodgren
%% @doc The module for the peer client.
%% @copyright 2011 Peer-talk.

-module(peer).
-compile(export_all). 
-export([start/1,shut_down/0,host_info/0,get_status/0,mess/2,ping/1,send_file/3,pingon/0,pingoff/0,
	 send/3,friends/0,sendFr/3,my/1,read_address/1]).

-spec start(integer()) -> list().
%% @doc <h4>start(G)</h4> Starts the client and creates an empty ets called friends that is the default friend list, 
%%if G is set to 1 then the gui is activated. 
%% @end
start(G) ->
    try		
	Me = "guest",
	Vsn = Me,
	ListenPortS = io:get_line("Insert your local port: "),
	ServerAddressS = io:get_line("Insert the address of server to connect: ") -- "\n",
	ServerPortS = io:get_line("Insert the server port: "),
	ServerAddress = read_address(ServerAddressS),

	{ok,[{integer,1,ListenPort}],1} = erl_scan:string(ListenPortS--"\n"),
	{ok,[{integer,1,ServerPort}],1} = erl_scan:string(ServerPortS--"\n"),


	{ok,{_, _, _, _, _, [NetworkInterface|_]}} = host_info(),
	
	application:set_env(kernel, inet_dist_listen_min, ListenPort),
	application:set_env(kernel, inet_dist_listen_max, ListenPort),

	register(chat_server, spawn(peer, server, [ListenPort])),

	register(chat, spawn(peer, status, [[{network_interface, NetworkInterface},
					     {listen_port, ListenPort}, {id,{Me, Vsn}}, {server_address, ServerAddress}, 
					     {server_port, ServerPort}, {ping_mode, pingoff}, {graphic, G}]])),

	chat!newfriends,	


	register(ping_pong, spawn(peer, ping_loop, [])),
	register(aging,spawn(peer,aging_loop,[])),
	if
	    G == 1 ->
		spawn(login_frame,start,["username:"]);
	    true ->
		Username = (io:get_line("Insert your username: ") --"\n"),
		Password = (io:get_line("Insert your password: ") --"\n"),
		chat!{login, {Username, Password}}
	end


    catch Ek:En ->
	    {Ek, En}

    end.

-spec status(term()) -> 
    nil().
%% @doc <h4>status(Status)</h4> starts a process that will hold Status.
%% @end
status(Status) ->
    receive
	shut_down ->
		spawn(peer,shut_down,[]),
		status(Status);
    	logout ->
    		rul:friends(),
    		spawn(login_frame, start, ["Username"]),
    		status(Status);
    	{changename, Name} ->
    		{value, {_ , {ID, _}}} = lists:keysearch(id, 1, Status),
    		spawn(peer,send_to_server,[{client, changename, ID, Name}]),
    		NewStatus = lists:keystore(id, 1, Status, {id, {ID, Name}}),
    		status(NewStatus);
        {reg_window, close} ->
            spawn(login_frame, start, ["username"]),
            status(Status);
    	{client, registerNewUser, Mail, Password, ShowedName} ->
    	    {value, {_ , ListenPort}} = lists:keysearch(listen_port, 1, Status),
    	    spawn(peer, newUser, [Mail, Password, ShowedName, ListenPort]),
    	    status(Status);
    	{client, registerWindow} ->
    	    register(reg_window, spawn(reg_frame, start, [])),
    	    status(Status);
        {login, reminder, Usermail} ->
            {value, {_ , ListenPort}} = lists:keysearch(listen_port, 1, Status),
            spawn(peer, reminder, [Usermail, ListenPort]),
	    status(Status);
	{gui, search, IDfriend} ->
	    spawn(peer, searchFriend, [IDfriend]),
	    status(Status);
	{delete_friend, Username} ->
	    spawn(peer, deletefriend,[Username]),
	    spawn(peer, deletefriend,[{Username, w}]),
	    spawn(rul,delete, [friends, Username]),
	    status(Status);
	{write, Obj}->
	    io:format("~w", [Obj]),
	    status(Status);
	{chat_send_file, Obj, Pid} ->
	    {value, {Receiver, _}} = lists:keysearch(Pid, 2, Status),
	    [Path, Name] = Obj,
	    io:format("preparing to sending file ~p~p to ~p \n", [Path, Name, Receiver]),
	    spawn(peer, send_file, [Receiver, Path, Name]),
	    status(Status);
	{file_accept, {{_, _}, FileName, File}} ->
	    file:write_file(FileName, File),
	    write_log("File " ++ FileName ++ " written on disk."),
	    status(Status);
	{file_refuse, {{_, _}, FileName, _}} ->
	    io:format("File ~p discarded.~n",[FileName]),
	    status(Status);
	{gui, addfriend, FriendID} ->
		spawn(peer, friend, [FriendID]),
		status(Status);
	{confirm, Sender_username} ->
	    spawn(peer,confirmfriend, [{Sender_username, p}]),
	    status(Status);
	{refuse, Sender_username} ->
	    spawn(peer,deletefriend, [{Sender_username, p}]),
	    status(Status);
	{close_window, Pid} ->
	    status(lists:keydelete(Pid, 2, Status));
	{chat_send, Pid, Message} ->
	    {value, {Receiver, _}} = lists:keysearch(Pid, 2, Status),
	    {value, {id, {ID, _}}} = lists:keysearch(id, 1, Status),
	    spawn(peer, send, [Receiver, string, {ID, Message}]),
	    status(Status);
	{chat_window, Receiver} ->
	    case rul:peek(friends, Receiver, ip) of
		{error, _} ->
		    status(Status);
		_ ->			
		    case lists:keysearch(Receiver, 1, Status) of
			false ->
			    {value, {_ , G}} = lists:keysearch(graphic, 1, Status),
			    if
				G == 1 ->
					N = rul:peek(friends, Receiver, name),
				    Pid = spawn(chat_frame, start, [N]),
				    NewStatus = lists:keystore(Receiver, 1, Status, {Receiver, Pid}),
				    status(NewStatus);
				true ->
				    status(Status)
			    end;
			_ ->
			    status(Status)
		    end
	    end;
	{login, {Username, Password}}  ->
	    {value, {_ , NetworkInterface}} = lists:keysearch(network_interface, 1, Status),
	    {value, {_ , ListenPort}} = lists:keysearch(listen_port, 1, Status),
	    rul:set_online(friends, Username, NetworkInterface, ListenPort),
	    spawn(peer,autentication,[Username, Password]),
	    {value, {_ , G}} = lists:keysearch(graphic, 1, Status),
	    if
		G == 1 ->
		    register(contacts_window, spawn(contacts, start, []));
		    %contacts_window!{client, friendlist};
		true ->
		    ok
	    end,
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

-spec server(integer()) -> 
    term().
%% @doc <h4>server(ListenPort)</h4> starts alistening proces on ListenPort.
%% @end
server(ListenPort) ->
    case gen_tcp:listen(ListenPort, [binary, {active, false}]) of 
	{ok, ListenSocket} ->
	    wait_connect(ListenSocket),
	    server(ListenPort); 
	{error, _} ->
	    "error on server start up!"
    end.

-spec wait_connect(term()) -> 
    term().
%% @doc <h4>wait_connect(ListenSocket)</h4> starts a proces waiting for a connection on ListenSocket.
%% @end
wait_connect(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
	{ok, Socket} ->
	    {ok,{Sender_address, _}} = inet:peername(Socket),
	    spawn(peer,get_request,[Sender_address,Socket,[]]),
	    wait_connect(ListenSocket);
	{error, _} ->
	    "connection refused!"
    end.



-spec get_request(term(), term(), binary()) -> 
    term().
%% @doc <h4>get_request(Sender_address, Socket, BinaryList)</h4> process the request BinaryList incoming on Socket.
%% @end
get_request(Sender_address, Socket, BinaryList) ->
    case gen_tcp:recv(Socket,0) of
	{ok, Binary} ->
	    get_request(Sender_address, Socket, [Binary|BinaryList]);
      	{error, closed} ->
	    try
		{Mode, Obj} = 
		binary_to_term(list_to_binary(lists:reverse(BinaryList))),
		case Mode of
		    usedID ->
		    	reg_window ! {usedID};
		    addUser ->
		    	reg_window ! {addUser},
		    	spawn(login_frame,start,[Obj]);
		    reminderSent ->
		    	spawn(dialog, msgbox,["reminder sent to mail"]);
		    noUser ->
		    	spawn(dialog, msgbox, ["no such email"]);
		    search ->
		    	contacts_window ! {client, search, Obj};
		    client_logout ->
			{Sender_username, Sender_showed_name} = Obj,
			String = Sender_showed_name ++ " has left.",
			io:format("~p~n", [String]),
			rul:logout(friends, Sender_username),
			gfu([Sender_username, Sender_showed_name, {offline}]),
			my(Sender_username)!{message_received, Sender_showed_name, String};
		    confirmfriend ->
				{_, Sender_username, Sender_showed_name} = Obj,
				rul:change(friends, Sender_username, name, Sender_showed_name);
		    befriends ->
				{Sender_listen_port, Sender_username, Sender_showed_name} = Obj,
				spawn(peer,acceptFr,[Sender_username, Sender_showed_name, Sender_address, Sender_listen_port]);
		    friendlist ->	
		    	{Id, My_showed_name, FriendList} = Obj,	
				U = my(username),
				case Id of
					U ->
						fillTable(friends, FriendList),
						chat!{change, id, {Id, My_showed_name}},
						contacts_window!{showed_name, My_showed_name},
						gf();
					_->
						[]
				end;	
		    file ->
				case my(graphic) of
					1 ->
						spawn(dialog, make_window , [Obj]);
					_ ->
						spawn(peer, handle_file_sending,[Obj])
				end;
		    ping ->
				{Sender_listen_port, {Sender_username, Sender_showed_name}} = Obj,
				chat!{send, Sender_username, pong, {Sender_listen_port,my(id)}},
				gfu([Sender_username, Sender_showed_name, {online}]),
				R = rul:set_online(friends, Sender_username, Sender_address, Sender_listen_port),
				rul:change(friends, Sender_username, name, Sender_showed_name),
			case my(ping_mode) of
			    pingon ->
				io:format("ping from ~p ~n", [Sender_username]);
			    _ ->
				[]
			end,
			case R of
				ok ->
					String = Sender_showed_name ++ " is online.",
					my(Sender_username)!{message_received, Sender_showed_name, String};
				_->
					[]
			end;
		    pong -> 
				{Sender_listen_port, {Sender_username, Sender_showed_name}} = Obj,
				gfu([Sender_username, Sender_showed_name, {online}]),
				R = rul:set_online(friends, Sender_username, Sender_address, Sender_listen_port),
				rul:change(friends, Sender_username, name, Sender_showed_name),
			case my(ping_mode) of
			    pingon ->
				io:format("pong from ~p ~n", [Sender_username]);
			    _ ->
				[]
			end,
			case R of
				ok ->
					String = Sender_showed_name ++ " is online.",
					my(Sender_username)!{message_received, Sender_showed_name, String};
				_->
					[]
			end;
		    string ->
			{Sender_username, String} = Obj,
			Sender_showed_name = rul:peek(friends, Sender_username, name),
			chat!{chat_window, Sender_username},
			write_log(Sender_showed_name ++ " to me: " ++ String),
			my(Sender_username)!{message_received, Sender_showed_name, String};
		    server_badlogin ->
			G = my(graphic),
			if
			    G == 1 ->
				try
				    exit(whereis(contacts_window),kill)
   				catch
				    Ek4:En4  -> {Ek4, En4}
    				end,
				case Obj of
				    badPass ->
					spawn(login_frame,start,["wrong password!"]);
				    _ ->
					spawn(login_frame,start,["wrong username!"])
				end;
			    true ->
				Username = (io:get_line("Insert your username: ") --"\n"),
				Password = (io:get_line("Insert your password: ") --"\n"),
				chat!{login, {Username, Password}}
			end;											 
		    _Any -> []
		end
	    catch
		Ek:En -> {Ek,En}
	    end
    end.

%%-------------------------------------------HJÄLPFUNKTIONER------------------------------------------------


-spec send(string(), atom(), any()) -> 
    term().
%% @doc <h4>send(Receiver, Mode, Obj)</h4> sends Mode and Obj to Receiver if Receiver is in the friend list,
%%Mode specifies for Receiver what kind of data that is beeing sent.<br>When Mode is set to file, friendlist, 
%%ping or pong, Receiver will neglect Data and process Obj that will be supposed to contain respectively
%%a file a friend list or an empty list in the case of a ping or a pong.</br>
%% @end
send(Receiver_username, Mode, Obj) ->
    try
	case rul:peek(friends, Receiver_username, age) of
	    infinity ->
		[];
	    _ ->
		Receiver_address = rul:peek(friends, Receiver_username, old_ip),
		Receiver_listen_port= rul:peek(friends, Receiver_username, old_port),
		{ok, Socket} = gen_tcp:connect(Receiver_address, Receiver_listen_port, 
					       [binary,{packet,0},{port, 0}]),
		sendB(Socket, term_to_binary({Mode, Obj})),
		gen_tcp:close(Socket)
	end
    catch 
	Ek:En -> 
	    {Ek,En}
    end.

-spec sendB(term(), binary()) -> 
    nil().
%% @doc <h4>sendB(Socket,Bin)</h4> sends Bin to sockets divided in 100bit-chunks. 
%% @end
sendB(Socket, <<Chunk:100/binary, Rest/binary>>) ->
    gen_tcp:send(Socket, Chunk),
    sendB(Socket, Rest);
sendB(Socket,Rest) ->
    gen_tcp:send(Socket, Rest).


-spec ping_friends(atom())-> 
    any().
%% @doc <h4>ping_friends(Table)</h4> pings all the friends in Table. 
%% @end
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
%% @end
ping_loop() ->
    ping_friends(friends),
    timer:sleep(10000),
    ping_loop().




-spec read_address(string())-> 
    any().
%% @doc <h4>read_address(Address)</h4> reads Address and if Address is a domain it returns Address if Address is an ip-address of the type "xx.yy.zz.ww"  it returns a tuple of the type {xx,yy,zz,ww}
%% @end
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
%% @end
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
%% @end
friends() -> rul:show(friends).

-spec acceptFr(string(),string(),term(),integer())-> 
    any().
%% @doc <h4>acceptFr(Sd,Vn,PublicIp,Lp)</h4> asks if you accept Sd as friend.
%% @end
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
%% @end
sendFr(Receiver_username, Receiver_address, Receiver_listen_port) ->
    {My_username, My_showed_name} = my(id),
    rul:add(friends, Receiver_username, "unknown"),
    rul:set_online(friends, Receiver_username, Receiver_address, Receiver_listen_port),
    send(Receiver_username, befriends, {my(listen_port), My_username, My_showed_name}),
    rul:logout(friends, Receiver_username).


-spec friend(string())-> 
    any().
%% @doc <h4>friend(Username)</h4> Sends a friend request to Username.
%% @end
friend(Username) ->
	{MyUser, _} = my(id),
	send_to_server({client,addfriend, MyUser, {Username,w}}),
    refresh().



refresh() -> autentication(my(username), my(password)).

autentication(Username, Password) ->
    try
	chat!{change, password, Password},
	chat!{change, username, Username},
	
	send_to_server({client,login, Username, Password, my(listen_port)})
    catch 
	Ek:En ->
	    {Ek,En}
    end.

aging_loop() ->
    try
	List = rul:tolist(friends), 
	old(List)
    catch 
	Ek:En ->
	    {Ek,En}
    end,
    timer:sleep (1000),
    aging_loop().

old ([{X,_}|L]) ->
    try
	Count = rul:peek(friends, X, age), 
	case Count of
	    infinity ->
		[];
	    30 ->
		Sender_showed_name = rul:peek(friends, X, name),
		String = Sender_showed_name ++ " is offline.",
		io:format("~p~n", [String]),
		rul:logout(friends, X),
		gfu([X, Sender_showed_name, {offline}]),
		my(X)!{message_received, Sender_showed_name, String},
		[];
	    _ ->
		rul:change(friends, X, age, Count + 1),
		[]
	end
    catch 
	_:_ ->
	    []
    end,
    old(L);
old([]) -> [].	

kill_conversations() -> shut(rul:tolist(friends)).

shut ([{X,_}|L]) ->
    chat! {send, X, client_logout, my(id)},
    Pid = my(X), 
    case Pid of
	{error,nomatch} ->
	    [];
	_ ->
	    exit(Pid, kill),
	    []
    end,
    shut(L);
shut([]) -> [].	

write_log(Note) ->
    {{Y,M,D},{H,MM,S}} = erlang:localtime(),
    Timestamp = lists:flatten(io_lib:format("~p/~p/~p - ~p:~p:~p",[Y,M,D,H,MM,S])),
    io:format("~p~n", [Note]),
    file:write_file("log_file.txt", Timestamp ++ " " ,[append]),
    file:write_file("log_file.txt", Note ++ "\n",[append]).

-spec shut_down() -> 
    term().
%% @doc <h4>shut_down()</h4> stops the client
%% @end
shut_down() ->
    try
	exit(whereis(contacts_window),kill)
    catch
	Ek5:En5  -> {Ek5, En5}
    end,
    try
    	kill_conversations()
    catch
	Ek6:En6  -> {Ek6, En6}
    end,
    try
	exit(whereis(aging),kill)
    catch
	Ek4:En4  -> {Ek4, En4}
    end,	
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
%% @end
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
%% @end
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
%% @end
mess(Receiver_username, String) ->	
    try
	{My_username, _} = my(id),
	Receiver_showed_name = rul:peek(friends, Receiver_username, name),
	chat!{send, Receiver_username, string,{My_username, String}}, 
	write_log("me to " ++ Receiver_showed_name ++ ": " ++ String),
	sent
    catch 
	Ek:En ->
	    {Ek,En}
    end.



-spec ping(string()) -> 
    term().
%% @doc <h4>ping(Receiver)</h4>  pings Receiver if Receiver is in the friend list  
%% @end
ping(Receiver) -> 
    chat!{send, Receiver, ping, {my(listen_port), my(id)}}, 
    sent.


-spec send_file(string(),string(),string()) -> 
    term().
%% @doc <h4>send_file(Receiver, Path, Name)</h4> sends the file Path ++ Name to Receiver if Receiver is in the friend list
%% @end
send_file(Receiver, Path, Name) ->
    try
	{ok, File} = file:read_file([Path]),
	chat!{send, Receiver, file, {my(id),Name, File}},
	sent
    catch _:_ ->
	    {error, file_not_found}
    end.
-spec pingon() -> 
    term().
%% @doc <h4>pingon()</h4> allows you to see who pings you.
%% @end
pingon() ->
    chat!{change, ping_mode, pingon}.



-spec pingoff() -> 
    term().
%% @doc <h4>pingoff()</h4> disallows you to see who pings you.
%% @end
pingoff() ->
    chat!{change, ping_mode, pingoff}.

handle_file_sending(Obj) ->
    {{_, Sender_showed_name}, FileName, _} = Obj,
    Line = io_lib:format("accept file ~p from ~p (y/n)? ", [FileName, Sender_showed_name]),
    case io:get_line(Line) of
	"y\n" ->
	    chat!{file_accept, Obj};
	"n\n" ->
	    chat!{file_refuse, Obj};
	_ ->
	    handle_file_sending(Obj)
    end.


%% @spec fillTable(Table, list()) -> ok
%% @doc <br>Pre: Table with name identifier 'friends' exists.</br><br>SIDE-EFFECT:Fills an already 
%%existing table with values from list().</br><br>Post:ok | error tuple</br>
fillTable(_, []) -> 
    io:format("Friend list updated succesfully~n"),
    ok;
fillTable(Table, [Friend|List]) ->
    case Friend of
	[{M,p},[Sn|_]] = Friend ->
	    accept_friend(M, Sn);
	[M,[Sn, Pip, Lp]] = Friend ->
	    ets:insert(Table, {M, [{name, Sn}, {old_ip, Pip}, {old_port, Lp}, {age, 0}]});
	[M, [Sn]] = Friend ->
	    ets:insert(Table, {M, [{name, Sn},{age, infinity}]})
    end,
    fillTable(Table, List).

confirmfriend(Usr) ->
	send_to_server({client,acceptfriend, my(username), Usr}),
	refresh().

accept_friend(Sender_username, Sender_showed_name) ->
	case my(graphic) of
		1->
			dialog:acc_friend({friendaccept, Sender_username, Sender_showed_name});
		_->
			Line = io_lib:format("accept friend request from ~p as ~p (y/n)? ", [Sender_username, Sender_showed_name]),
			case io:get_line(Line) of
				"y\n" ->
					chat!{confirm, Sender_username};
				"n\n" ->
					chat!{refuse, Sender_username};
				_ ->
					[]
			end
	end.

deletefriend(Usr) ->
	 send_to_server({client,removefriend, my(username), Usr}),
	 refresh().

gf() -> 
	case my(graphic) of
		1 ->
			contacts_window!{client, friendlist};
		_ ->
			[]
	end.

gfu(Obj) -> 
	case my(graphic) of
		1 ->
			contacts_window!{client, update, Obj};
		_ ->
			[]
	end.


searchFriend(ID) ->
	{MyUser, _} = my(id),
 	send_to_server({client, search, MyUser, ID}).
    

newUser(Mail, Password, ShowedName, ListenPort) ->
    send_to_server({client, adduser, Mail, ShowedName, Password, ListenPort}).


reminder(Usermail, ListenPort) ->
	 send_to_server({client, reminder, Usermail, ListenPort}).
    
send_to_server(Obj) ->
    try
        {ok, Sock} = gen_tcp:connect(my(server_address), my(server_port), [binary,{active, false}]),
	gen_tcp:send(Sock, term_to_binary(Obj))
    catch 
	Ek:En ->
	    {Ek,En}
    end.


