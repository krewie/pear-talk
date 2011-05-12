-module(serv_ul).
-export([add_user/8, duplicateCheck/2, add_friend/3, onlineStatus/3, retrieveFriend/2, retrieveFriends/2, remove_friend/3, removeFromList/2]).

%% start/1, close/1, loop/1,

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



remove_friend(Table, MyID, FriendID) -> 
    [{Mail, [NetInfo, Friendlist, Online]}] = detsapp:retrieve(Table,MyID),
    detsapp:add(Table, Mail, [NetInfo, removeFromList(Friendlist,FriendID) , Online]).
	    
		
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
    



	    
