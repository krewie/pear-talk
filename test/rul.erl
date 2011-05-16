-module(rul).
-compile(export_all).

friends()->
	try
		ets:new(friends, [set, named_table, public])
	catch  _:_ ->
		ets:delete(friends),
		friends()
	end.

fillTable(_, []) -> ok;
fillTable(Table, [Friend|List]) ->
	case Friend of
	[M,[Sn, Pip, Lp]] = Friend ->
		ets:insert(Table, {M, [Sn, Pip, Lp]});
	[M, []] = Friend ->
		add(Table, M, [])
	end,
	fillTable(Table, List). 

empty() -> spawn(fun() -> ets:new(tom, [set, public]), receive _X -> ok end end).

show(Table) ->
	rul:traverse(Table,fun(X) -> io:format("~p~n", [X]) end).

add(Table, Mail, ShowedName) ->
	case take(Table, Mail) of
	{error, nomatch} ->
		ets:insert(Table,{Mail,[ShowedName,[]]});
	_ ->
		{error, allready_existing_friend}
	end.


logout(Table, Mail) ->
	try
		[ShowedName,FriendList,_,_,_,_] = take(Table, Mail),
		ets:insert(Table,{Mail,[ShowedName,FriendList]}),		
		ok
	catch		
		_:_ -> 
			{error,user_allready_offline}
	end.
	
take(Table, Mail) ->
	try
		[{_,Value}] = ets:lookup(Table, Mail),
		Value
	catch
		_:_ ->
			{error, nomatch}
	end.
set_online(Table, Sender, SenderIP, SenderPort) ->
	try
		[Name, _] = take(Table, Sender),
		ets:insert(Table,{Sender,[Name, SenderIP, SenderPort]}),
		ok
	catch		
		Ek:En -> 
			{Ek,En}
	end.

delete(Table, Mail) ->
	dets:delete(Table, Mail).


traverse(Table, Fun) ->
	taux (Table, Fun, ets:first(Table)).

taux(_Table,_Fun, '$end_of_table' ) -> done;
taux(Table,Fun,  Key) ->
        Next = ets:next(Table, Key ),
        Fun(ets:lookup(Table, Key)),
        taux(Table, Fun, Next).

