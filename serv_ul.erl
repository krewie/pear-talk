-module(serv_ul).
-export([add_user/8, duplicateCheck/2, add_friend/3, onlineStatus/3, retrieveFriend/2, retrieveFriends/2, remove_friend/3, removeFromList/2]).
-define(DB, "users").

%% start/1, close/1, loop/1, change_name/3
%motherpid behövs nog inte :) LOL

start(MotherPid) ->
	{ok, Table} = detsapp:open(?DB),
	loop(MotherPid, Table).
	
loop(MotherPid, Table) -> 
	receive
	%INCOMPLETE
		{client, login, ID, Netinfo, Pid} ->
			Friendlist = retrieveFriends(Table, ID),
			onlineStatus(Table, ID, Netinfo),
			Pid ! {db, friendlist, Friendlist};
		{client, addfriend, {MyID, FriendID}, Pid} ->
			add_friend(Table, MyID, FriendID),
			Pid ! {db, addfriend, ok};
		{client, removefriend, {MyID, FriendID}, Pid} ->
			remove_friend(Table, MyID, FriendID),
			Pid ! {db, addfriend, ok};
		{client, changename, ID, Name, Pid} ->
			change_name(Table, ID, Name),
			Pid ! {db, changename, ok};
		
	end
	loop(Pid, Table).

add_user(Table, Mail, ShowedName, FriendList, PeerIp, PeerPort, ServerIp, ServerPort) ->
    detsapp:add(Table,Mail,[[ShowedName,PeerIp,PeerPort,ServerIp,ServerPort], FriendList, {online, false}]),
    detsapp:sync(Table).

duplicateCheck([FriendId|Friendlist], Id) ->
    if 
	Id == FriendId ->
	    true;
	true -> 
	    duplicateCheck(Friendlist, Id)
    end;
duplicateCheck([], _) ->
    false.

removeFromList([Friend|Friendlist], FriendID) ->
    case Friend == FriendID of
	false ->
	    [Friend|removeFromList(Friendlist,FriendID)];
	true -> 
	    Friendlist
    end;
removeFromList([], _) -> [].


add_friend(Table,MyID, FriendID) ->
    [{MyID,[ NetInfo, FriendList, Online]}] = detsapp:retrieve(Table, MyID),
    case duplicateCheck(FriendList, FriendID) of
	false ->
	    detsapp:add(Table, MyID, [NetInfo, [FriendID|FriendList], Online]);
	true ->
	    ok
    end,
    detsapp:sync(Table).

change_name(Table, ID, Name) ->
	{ID, [[ShowedName,PeerIp,PeerPort,ServerIp,ServerPort], FriendList, Online]} = detsapp:retrieve(Table, ID),
	add_user(Table, ID, Name, FriendList, PeerIp, PeerPort, ServerIp, ServerPort).
	%TBI


remove_friend(Table, MyID, FriendID) -> 
    [{Mail, [NetInfo, Friendlist, Online]}] = detsapp:retrieve(Table,MyID),
    detsapp:add(Table, Mail, [NetInfo, removeFromList(Friendlist,FriendID) , Online]),
    detsapp:sync(Table).
	    
		
onlineStatus(Table, MyID, NewInfo) ->
    [{MyID,[ _, FriendList, {online, Status}]}] = detsapp:retrieve(Table, MyID),
    case Status of 
	false ->
	    detsapp:add(Table, MyID, [NewInfo, FriendList, {online, true}]);
	true ->
	    detsapp:add(Table, MyID, [[], FriendList, {online, false}])
    end,
    detsapp:sync(Table).

retrieveFriend(Table, HisID) ->
     [{HisID, [NetInfo, _, _]}] = detsapp:retrieve(Table, HisID),
    case NetInfo == []  of
	true ->
	    [HisID, []];
	   _ ->
	    [ShownName, PeerIP, PeerPort, _, _] = NetInfo, 
	    [HisID, [ShownName, PeerIP,PeerPort]]
    end.


retrieveFriends(Table, MyID) ->
    [{MyID, [_, Friendslist, _]}] = detsapp:retrieve(Table,MyID),
    F = fun (FriendID) -> retrieveFriend(Table, FriendID)
	end,
    lists:map(F, Friendslist).


%%start(Table) ->
    

%%close(Table) ->
    

%%loop(Table) ->
    



	    
