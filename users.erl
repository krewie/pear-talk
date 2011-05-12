-module(users).
-compile(export_all).

open() ->
	dets:open_file(users, [{type, set}]),
	dets:sync(users).

show() ->
	open(),
	dets:traverse(users,fun(X) -> io:format("~p~n", [X]), continue end),
	close().

add(Mail, ShowedName, FriendList, PeerIp, PeerPort, ServerIp, ServerPort) ->
	open(),
	dets:insert(users,{Mail,[ShowedName,PeerIp,PeerPort,ServerIp,ServerPort], FriendList, {online, false}),
	close().
	
add_friend(MyID, FriendID) -> 
	open(),
		[{MyID, NetInfo, FriendList, Online}] = dets:lookup(users, MyID),
		dets:insert(users, {MyID, NetInfo, [FriendID|FriendList], Online}).
	close().
		

get_friendlist(MyID) ->
	open(),
		[{_, _, FriendList, _}] = dets:lookup(users, MyID),
		[X || X <- Friendlist, select_user(X)]
		
		
select_user(ID) ->
	open(),
		[{UserID, NetInfo, Friendlist, Online}] = dets:lookup(users, ID),
		{UserID, NetInfo, Online},
	close().
		
login(ID) ->
	% blabla ska ändra onlinetupeln till true.
	%leker lite med svn...
	
close() ->
	dets:close(ul).
