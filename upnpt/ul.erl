-module(ul).
-compile(export_all).

open() ->
	dets:open_file(ul, [{type, set}]),
	dets:sync(ul).

show() ->
	open(),
	dets:traverse(ul,fun(X) -> io:format("~p~n", [X]), continue end),
	close().

add(Mail, ShowedName, FriendList, NetworkInterface, ListenPort, ServerIp, ServerPort) ->
	open(),
	dets:insert(ul,{Mail,[ShowedName,FriendList, NetworkInterface, ListenPort, ServerIp, ServerPort]}),
	close().
logout(Mail) ->
	open(),
	try
		[ShowedName,FriendList,_,_,_,_] = take(Mail),
		dets:insert(ul,{Mail,[ShowedName,FriendList]}),
		close(),		
		ok
	catch		
		_:_ -> 
			close(),
			{error,user_allready_offline}
	end.
	
take(Mail) ->
	open(),
	[{_,Value}] = dets:lookup(ul, Mail),
	close(),
	Value.
set_online(M, Ni, Lp, Sip, Sp) ->
	open(),
	try
		[Sn, Fl] = take(M),
		add(M, Sn, Fl, Ni, Lp, Sip, Sp),
		close(),
		ok
	catch		
		_:_ -> 
			close(),
			{error,user_allready_online}
	end.
delete(Mail) ->
	open(),
	dets:delete(ul, Mail),
	close().
close() ->
	dets:close(ul).
