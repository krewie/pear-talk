-module(serv_ul).
-export([addUser/6, duplicateCheck/2, addFriend/3, removeFriend/3, changeName/3, changePassword/4, onlineStatus/3, login/3, retrieveFriend/2, retrieveFriends/2, start/0, loop/1]).
-define(DB, "users").


addUser(Table, Username, ShowedName, FriendList, NetInfo, Password) ->
    dapi:add(Table,Username,[NetInfo, FriendList, Password, ShowedName]),
    dapi:sync(Table).


duplicateCheck([Head|List], Key) ->
    if 
	Key == Head ->
	    true;
	true -> 
	    duplicateCheck(List, Key)
    end;
duplicateCheck([], _) ->
    false.


%%removeFromList([Friend|Friendlist], FriendID) ->
%%    case Friend == FriendID of
%%	false ->
%%	    [Friend|removeFromList(Friendlist,FriendID)];
%%	true -> 
%%	    Friendlist
%%  end;
%%removeFromList([], _) -> [].


addFriend(Table,MyID, FriendID) ->
    A = dapi:retrieve(Table, MyID),
    case A /= [] of 
	true ->
	    [{MyID,[ NetInfo, FriendList, Password, ShowedName]}] = A
		case duplicateCheck(FriendList, FriendID) of
		    false ->
			addUser(Table, MyID, ShowedName, [FriendID|Friendlist], NetInfo, Password),
			dapi:sync(Table);
		    true ->
			ok
		end;
	_ -> {error, {badmatch, MyID}}
    end.


removeFriend(Table, MyID, FriendID) -> 
    A = dapi:retrieve(Table, MyID),
    case A /= [] of
	true ->
	    [{Mail, [NetInfo, Friendlist, Password, ShowedName]}] = A,
	    addUser(Table, Mail, ShowedName,  [X || X <- Friendlist, X =/= FriendID], NetInfo, Password),	    
	    %%dapi:add(Table, Mail, [NetInfo, [X || X <- Friendlist, X =/= FriendID] , Password, ShowedName]),
	    dapi:sync(Table);
	_ ->
	    {error, {badmatch, MyID}}
    end.


changeName(Table, ID, Name) ->
    A = dapi:retrieve(Table, ID),
    case A /= [] of 
	true ->
	    [{ID, [NetInfo, FriendList, Password, _]}] = A,
	    addUser(Table, ID, Name, FriendList, NetInfo, Password),
	    dapi:sync(Table);
	_ -> {error, {badmatch, ID}}
    end.


changePassword(Table, ID, Password, OldPass) ->
    A = dapi:retrieve(Table, ID),
    case A /= [] of 
	true ->
	    [{ID, [NetInfo, Friendlist, RetrievedPass, ShowedName]}] = A,
	    case (OldPass == RetrievedPass) of
		true -> 
		    addUser(Table, ID, ShowedName, FriendList, NetInfo, Password),     
		    dapi:sync(Table);
		_ -> 
		    {error, {badmatch, OldPass}}
	    end;
	_ -> {error, {badmatch, ID}}
    end.


onlineStatus(Table, MyID, NewInfo) ->
    A = dapi:retrieve(Table, MyID),
    case A /= [] of 
	true ->
	    [{MyID,[ NetInfo, FriendList, Password, ShowedName]}] = A,
	    case (NetInfo == []) of 
		true ->
		    addUser(Table, MyID, ShowedName, FriendList, NewInfo, Password);
		false ->
		    addUser(Table, MyID, ShowedName, FriendList, [], Password)
	    end,
	    dapi:sync(Table);
	_ -> {error, {badmatch, MyID}}
    end.


login(Table, ID, Password) ->
    A = dapi:retrieve(Table, ID)
    case A /= [] of
	true ->
	    [{ID, [Netinfo, Friendlist, RetrievedPass]}] = A
		case (Password == RetrievedPass) of
		    true ->
			true;
		    _ ->
			{error, {badmatch, Password}}  
		end;
	_ ->
	    {error, {badmatch, ID}}
    end.


retrieveFriend(Table, HisID) ->
    A = dapi:retrieve(Table, HisID),
    case A /= [] of 
	true ->
	    [{HisID, [NetInfo, _, _, ShownName]}] = A,
	    [HisID, ShownName, NetInfo];
	_ ->
	    [HisID, HisID, []]
    end.


retrieveFriends(Table, MyID) ->
    A = dapi:retrieve(Table, MyID),
    case A /= [] of 
	true ->
	    [{MyID, [_, Friendslist, _]}] = dapi:retrieve(Table,MyID),
	    F = fun (FriendID) -> retrieveFriend(Table, FriendID)
		end,
	    lists:map(F, Friendslist);
	_ -> 
	    {error,{badmatch, MyID}}
    end.


start() ->
	{ok, Table} = dapi:open(?DB),
    loop(Pid, Table).


loop(Table) -> 
    receive
						%INCOMPLETE
	{client, login, ID, ShowedName,Netinfo, Password, ClientPid} ->
	    case login(Table, ID, Password) of 
		true -> 
		    Friendlist = retrieveFriends(Table, ID),
		    onlineStatus(Table, ID, Netinfo),
		    ClientPid ! {db, friendlist, Friendlist};
		{badmatch, Password} -> 
		    ClientPid!{db, badPass}; %% fel lösenord
		{badmatch, ID} ->
		    ClientPid!{db, badID} %% användare existerar inte
	    end;

	{client, addfriend, MyID, FriendID, Pid} ->
	    case addFriend(Table, MyID, FriendID) of
		ok ->
		    Pid ! {db, addfriend, ok};
		{error, {badmatch, MyID}} ->
		    Pid!{db, badID} %% användare existerar inte
	    end;

	{client, removefriend, MyID, FriendID, Pid} ->
	    case removeFriend(Table, MyID, FriendID) of
		ok ->
		    Pid ! {db, removefriend, ok};
		{error,{badmatch, MyID}} ->
		    Pid ! {db, badID}
	    end;

	{client, changename, ID, Name, Pid} ->
	    case changeName(Table, ID, Name) of
		ok ->
		    Pid ! {db, changename, ok};
		{error, {badmatch, ID}} ->
		    Pid ! {db, badID}
	    end
    end,       
    loop(Table).
    
