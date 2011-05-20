-module(serv_ul).
-export([addUser/6, addFriend/3, removeFriend/3, changeName/3, changePassword/4, onlineStatus/3, login/3, retrieveFriend/2, retrieveFriends/2, start/0, loop/2, acceptFriend/3]).
-define(DB, "users").


%% @doc Lägger till en användare (Username) i tabellen Table.
%% @spec addUser(Table, Username, ShowedName, FriendList, NetInfo, Password) -> ok | {error, Reason}
%% Table = atom() | reference()
%% Username = any()
%% ShowedName = any()
%% Friendlist = [Username|Friendlist]
%% NetInfo = any()
%% Password = any()
addUser(Table, Username, ShowedName, FriendList, NetInfo, Password) ->
    dapi:add(Table,Username,[NetInfo, FriendList, Password, ShowedName]),
    dapi:sync(Table).


%% @doc Traverserar en lista för att undersöka om en instans av ett objekt existerar i listan, om objektet kan jämföras med '=='.
%% @spec existanceCheck(List, Key) -> true | false
%% List = [any()]
%% Key = any()
existanceCheck([Head|List], Key) ->
    if 
	Key == Head ->
	    true;
	true -> 
	    existanceCheck(List, Key)
    end;
existanceCheck([], _) ->
    false.


%%removeFromList([Friend|Friendlist], FriendID) ->
%%    case Friend == FriendID of
%%	false ->
%%	    [Friend|removeFromList(Friendlist,FriendID)];
%%	true -> 
%%	    Friendlist
%%  end;
%%removeFromList([], _) -> [].


%% @doc Lägger till en person (FriendID) i en annan persons (MyID) vänlista, om den andra personen existerar i tabellen Table.
%% @spec addFriend(Table,MyID, FriendID) -> ok | {error, Reason}
%% Table = atom() | reference()
%% MyID = any()
%% FriendID = tuple()
addFriend(Table,MyID, FriendID) ->
    A = dapi:retrieve(Table, MyID),
    case (A /= []) of
	true ->
	    [{MyID,[ NetInfo, FriendList, Password, ShowedName]}] = A,
	    case existanceCheck(FriendList, FriendID) of
		false ->
		    addUser(Table, MyID, ShowedName, [FriendID|FriendList], NetInfo, Password),
		    dapi:sync(Table),
		    case FriendID of
		       {ID, w} ->
    		    		addFriend(Table, ID, {MyID, p});
    		    	_ -> ok
    		    end;
		true ->
		    ok
	    end;
	_ -> {error, {badmatch, MyID}}
    end.

%% @doc Accepterar en vänförfrågan och ändrar så att vännerna känner varandra
%% @spec acceptFriend(Table, MyID, FriendID) -> ok | false
%% Table = atom() | reference()
%% MyID = any()
%% FriendID = tuple()    
acceptFriend(Table, MyID, FriendID) ->
	case FriendID of
		{ID, p} ->
			removeFriend(Table, MyID, FriendID),
			addFriend(Table, MyID, ID),
			acceptFriend(Table, ID, {MyID, w});
		{ID, w} ->
			removeFriend(Table, MyID, FriendID),
			addFriend(Table, MyID, ID);
		_ -> {error, badID}
	end,
	ok.


%% @doc Tar bort en person (FriendID) från en annan persons (ID) vänlista, om den andra personen existerar i tabellen Table.
%% @spec removeFriend(Table,MyID, FriendID) -> ok | {error, Reason}
removeFriend(Table, MyID, FriendID) ->
    io:format("Gonna remove friend\n", []),
    A = dapi:retrieve(Table, MyID),
    case A /= [] of
	true ->
	    [{Mail, [NetInfo, Friendlist, Password, ShowedName]}] = A,
	    addUser(Table, Mail, ShowedName,  [X || X <- Friendlist, X =/= FriendID], NetInfo, Password),
	    io:format("Have removed friend: ~p\n", [FriendID]),
	    %%dapi:add(Table, Mail, [NetInfo, [X || X <- Friendlist, X =/= FriendID] , Password, ShowedName]),
	    dapi:sync(Table);
	_ ->
	    {error, {badmatch, MyID}}
    end.


%% @doc Ändrar en persons (ID) "smeknamn" till Name, givet att personen existerar i tabellen Table.
%% @spec changeName(Table, ID, Name) -> ok | {error, Reason}
%% Table = atom()|reference()
%% ID = any()
%% Name = any()
changeName(Table, ID, Name) ->
    A = dapi:retrieve(Table, ID),
    case A /= [] of 
	true ->
	    [{ID, [NetInfo, FriendList, Password, _]}] = A,
	    addUser(Table, ID, Name, FriendList, NetInfo, Password),
	    dapi:sync(Table);
	_ -> {error, {badmatch, ID}}
    end.


%% @doc Ändrar en persons (ID) lösenord till Password, givet att personen existerar, samt att det gamla lösenordet OldPass stämmer överens med det som lagrats i tabellen.
%% @spec changePassword(Table, ID, Password, OldPass) -> ok | {error, Reason}
%% Table = atom()|reference()
%% ID = any()
%% Password = any()
%% OldPass = any()
changePassword(Table, ID, Password, OldPass) ->
    A = dapi:retrieve(Table, ID),
    case A /= [] of 
	true ->
	    [{ID, [NetInfo, FriendList, RetrievedPass, ShowedName]}] = A,
	    case (OldPass == RetrievedPass) of
		true -> 
		    addUser(Table, ID, ShowedName, FriendList, NetInfo, Password),     
		    dapi:sync(Table);
		_ -> 
		    {error, {badmatch, password}}
	    end;
	_ -> {error, {badmatch, ID}}
    end.


%% @doc Ändrar en persons NetInfo, givet att personen existerar i tabellen Table.
%% @spec onlineStatus(Table, MyID, NetInfo) -> ok|{error, Reason}
%% Table = atom()|reference()
%% MyID = any()
%% NetInfo = any()
onlineStatus(Table, MyID, NetInfo) ->
    A = dapi:retrieve(Table, MyID),
    case A /= [] of 
	true ->
	    [{MyID,[ _, FriendList, Password, ShowedName]}] = A,
	    addUser(Table, MyID, ShowedName, FriendList, NetInfo, Password),
	    dapi:sync(Table);
	_ -> {error, {badmatch, MyID}}
    end.


%% @doc Undersöker hurvida lösenordet Password som givits av personen ID stämmer överens med vad som lagrats i tabellen Table. True om det stämmer, false om det inte stämmer.
%% @spec login(Table, ID , Password) -> true | false | {error, Reason}
%% Table = atom() | reference()
%% ID = any()
%% Password = any()
login(Table, ID, Password) ->
    A = dapi:retrieve(Table, ID),
    case A /= [] of
	true ->
	    [{ID, [_, _, RetrievedPass, _]}] = A,
	    case (Password == RetrievedPass) of
		true ->
		    true;
		_ ->
		    false  
	    end;
	_ ->
	    {error, {badmatch, ID}}
    end.


%% @doc Givet ett ID, hämtar ut kontaktinformation om en person.
%% @spec retrieveFriend(Table, HisID) -> [HisID | ShownName | NetInfo]
%% Table = atom()|reference()
%% HisID = any()
%% ShownName = any()
%% NetInfo = any()
retrieveFriend(Table, HisID) ->
    A = dapi:retrieve(Table, HisID),
    case A /= [] of 
	true ->
	    [{HisID, [NetInfo, _, _, ShownName]}] = A,
	    case NetInfo of
	    	[Ip, Port] ->
	    		[HisID, [ShownName, Ip, Port]];
	    	[] ->
	    		[HisID, [ShownName]]
	    end;
	_ ->
	    case HisID of
	    	{ID, w} ->
	    		[ID, [ID]];
	    	{ID, p} ->
	    		Info = retrieveFriend(Table, ID),
	    		[HisID, Info];
	    	_ ->
	    		[HisID, [HisID]]
	    	end
    end.


%% @doc Hämtar ut kontaktinformation för en persons vänner.
%% @spec retrieveFriends(Table, MyID) -> Friendslist
%% Table = atom()| reference()
%% MyID = any()
%% Friendslist = [ID | ShownName | NetInfo]
%% ID = any()
%% ShownName = any()
%% NetInfo = any()
retrieveFriends(Table, MyID) ->
    A = dapi:retrieve(Table, MyID),
    case A /= [] of 
	true ->
	    [{MyID, [_, Friendslist, _, _]}] = dapi:retrieve(Table,MyID),
	    F = fun (FriendID) -> retrieveFriend(Table, FriendID)
		end,
	    lists:map(F, Friendslist);
	_ -> 
	    {error,{badmatch, MyID}}
    end.

%% @doc Hämtar ut Nät informationen som användaren med id ID har
%% @spec getNetInfo(Table, ID) -> NetInfo
%% Table = atom() | reference()
%% ID = any()
%% NetInfo = tuple()
getNetInfo(Table, ID) ->
    case retrieveFriend(Table, ID) of
    	[_, [_, Ip, Port]] ->
    		[Ip, Port];
    	[_,[_]] -> []
    end.


%% @doc Öppnar en fördefinerad tabell för redigering.
%% @spec start() -> ok
start() ->
    {ok, Table} = dapi:open(?DB),
    loop(Table, true).


%% @doc Stänger en tabell Table.
%% @spec close(Table) -> ok | {error, Reason}
%% Table = atom()| reference()
close(Table) ->
    dapi:closeTable(Table).


%% @doc Manager av mottagna meddelanden från server, så länge State är true.
%% @spec loop(Table, State) -> ok | {error, Reason}
%% Table = atom()| reference()
%% State = true | false
loop(Table, State) -> 
    receive
	%% client requests %%
	{client, login, ID,Netinfo, Password, ClientPid} ->
	    io:format("Checking login info\n"),
	    case login(Table, ID, Password) of 
		true -> 
		    Friendlist = retrieveFriends(Table, ID),
		    onlineStatus(Table, ID, Netinfo),
		    ClientPid ! {db, friendlist, Netinfo, Friendlist};
		false ->
		    io:format("Bad password\n", []),
		    ClientPid!{db, badPass, Netinfo}; %% fel lösenord
		{error, {badmatch, ID}} ->
		    io:format("Bad username\n", []),
		    ClientPid!{db, badID, Netinfo} %% användare existerar inte
	    end;

	{client, addfriend, MyID, FriendID, Pid} ->
	    case addFriend(Table, MyID, FriendID) of
		ok ->
		    {ID, w} = FriendID,
		    Pid ! {db, addfriend, getNetInfo(Table, ID), retrieveFriends(Table, ID), ok};
		{error, {badmatch, MyID}} ->
		    Pid!{db, badID, getNetInfo(Table, MyID)} %% användare existerar inte
	    end;
	    
	{client, acceptfriend, MyID, FriendID, Pid} ->
		case acceptFriend(Table, MyID, FriendID) of
			ok ->
				{ID, p} = FriendID,
				Pid ! {db, acceptfriend, retrieveFriends(Table, MyID), retrieveFriends(Table, ID), getNetInfo(Table, MyID), getNetInfo(Table, ID)};
			{error, _} -> ok
		end;

	{client, removefriend, MyID, FriendID, Pid} ->
		io:format("Request of friend removel\n", []),
	    case removeFriend(Table, MyID, FriendID) of
		ok ->
		    Pid ! {db, removefriend, getNetInfo(Table, MyID), ok};
		{error,{badmatch, MyID}} ->
		    Pid ! {db, badID, getNetInfo(Table, MyID)} %% användare existerar inte
	    end;

	{client, changename, ID, Name, Pid} ->
	    case changeName(Table, ID, Name) of
		ok ->
		    Pid ! {db, changename, getNetInfo(Table, ID), ok};
		{error, {badmatch, ID}} ->
		    Pid ! {db, badID, getNetInfo(Table, ID)} %% användare existerar inte
	    end;

        {client, changepass, ID, NewPass, OldPass, Pid} ->
	    case changePassword(Table, ID, NewPass,OldPass) of
		ok -> 
		    Pid!{db, changepass, getNetInfo(Table, ID), ok};
		{error, {badmatch, password}} ->
		    Pid!{db, badPass, getNetInfo(Table, ID)};
		{error, {badmatch, ID}} ->
		    Pid!{db, badID, getNetInfo(Table, ID)}
	    end;

        %% server requests %%

	{server, ping, Pid} ->
	    Pid!{db, pong, self()};
	{server, close} -> 
	    close(Table),
	    State = false
    end,
    if 
	State == true -> 
	    loop(Table, State)
    end.

