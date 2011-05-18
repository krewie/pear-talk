%% @author Kristian Ionescu & Staffan Rodgren.
%% @doc The ets module for the peer client.
%% @copyright 2011 Peer-talk.

-module(rul).
-compile(export_all).
-export([friends/0, fillTable/2, empty/0, show/1, add/3, logout/2, take/2, set_online/4, delete/2, traverse/2, fold/2, tolist/1, peek/3, change/4]).

%%@spec friends() -> any()
%%@doc <br>Pre:NULL</br><br>Post:Creates an empty ets table with identifier 'friends'.</br><br>Post:NULL</br>
friends()->
	try
		ets:new(friends, [set, named_table, public])
	catch  _:_ ->
		ets:delete(friends),
		friends()
	end.

%% @spec fillTable(Table, list()) -> ok
%% @doc <br>Pre: Table with name identifier 'friends' exists.</br><br>SIDE-EFFECT:Fills an already 
%%existing table with values from list().</br><br>Post:NULL</br>
 
fillTable(_, []) -> ok;
fillTable(Table, [Friend|List]) ->
	case Friend of
		[M,[Sn, Pip, Lp]] = Friend ->
			ets:insert(Table, {M, [{name, Sn}, {ip, Pip}, {port, Lp}, {age, 0}]});
		[M, [Sn]] = Friend ->
			ets:insert(Table, {M, [{name, Sn},{age, infinity}]})
	end,
	fillTable(Table, List). 

%% @spec empty() -> any()
%% @doc <br>Pre:NULL</br><br>SIDE-EFFECT:Spawns a new process that creates an empty ets
%%table.</br><br>Post:Returns an empty ets table.</br>

empty() -> spawn(fun() -> ets:new(tom, [set, public]), receive _X -> ok end end).

%% @spec show(Table) -> io_device
%% @doc <br>Pre: NULL</br><br>Post:Prints table content to screen.</br>
show(Table) ->
	io:format("~p~n",[tolist(Table)]). 

%% @spec tolist(Table) -> list()
%% @doc <br>Pre:NULL</br><br>Post:Table content to list.</br>
tolist(Table) -> hide(fold(Table, (fun (X) -> X end))).


%% @spec add(tab, string(), string()) -> true | {error, allready_existing_friend}
%% @doc <br>Pre:NULL</br><br>SIDE-EFFECT:Inserts a new entry in table with the second argument as the key and the third as the 'showedname'.</br><br>Post:NULL</br>

add(Table, Mail, ShowedName) ->
	case take(Table, Mail) of
	{error, nomatch} ->
		ets:insert(Table,{Mail,[{name, ShowedName},{age, infinity}]});
	_ ->
		{error, allready_existing_friend}
	end.

%% @spec logout(Table, string()) -> true | {error, user_allready_offline}
%% @doc <br>Pre:NULL</br><br>SIDE-EFFECT:Removes tcp/ip information about a user in table when the user logout.</br><br>Post:NULL</br>
logout(Table, Mail) ->
	try
		[ShowedName,_,_] = take(Table, Mail),
		ets:insert(Table,{Mail,[{name, ShowedName}, {age, infinity}]}),				
		ok
	catch		
		_:_ -> 
			{error,user_allready_offline}
	end.

%% @spec take(Table, string()) -> {List | {error, nomatch}}
%% @doc <br>Pre:NULL</br><br>Post:Returns the value corresponding to the key.</br>
take(Table, Mail) ->
	try
		[{_,Value}] = ets:lookup(Table, Mail),
		hide_tag(Value)
	catch
		_:_ ->
			{error, nomatch}
	end.

%% @spec set_online(Table, string(),tuple(), integer()) -> true | {error, reason}
%% @doc <br>Pre:NULL</br><br>SIDE-EFFECT:Saves the tcp/ip information about a user in table when user is online.</br><br>Post:NULL</br>
set_online(Table, Sender, SenderIP, SenderPort) ->
	try
		[Name] = take(Table, Sender),
		ets:insert(Table,{Sender,[{name, Name}, {ip, SenderIP}, {port, SenderPort}, {age, 0}]}),
		ok
	catch		
		Ek:En -> 
			{Ek,En}
	end.
%% @spec delete(Table, string()) -> true
%% @doc <br>Pre:NULL</br><br>SIDE-EFFECT: Deletes the row  in table with key as second argument.</br><br>Post:NULL</br>
delete(Table, Mail) ->
	dets:delete(Table, Mail).

%% @spec peek(Table, Mail, Key) -> true
%% @doc <br>Pre:Table has to contain an element with key Key</br><br>SIDE-EFFECT: NULL.</br><br>Post:The element with key Key</br>
peek(Table, Mail, Key) ->
	try
		[{_,L}] = ets:lookup(Table, Mail),
		{value, {_ , Value}} = lists:keysearch(Key, 1, L),
		Value
	catch
		Ek:En ->
			{Ek, En}
	end.

%% @spec change(Table, Mail, Key, Value) -> true
%% @doc <br>Pre:NULL</br><br>SIDE-EFFECT: changes the element with key Key to Value if there is one</br><br>Post:Table with the element with key Key added or replaced with Value in Mail</br>
change(Table, Mail, Key, Value) ->
	try
		[{_,L}] = ets:lookup(Table, Mail),
		ets:insert(Table,{Mail,lists:keystore(Key, 1, L, {Key, Value})}),
		Table
	catch
		Ek:En ->
			{Ek, En}
	end.

	

%% @spec traverse(Table, Fun) -> any()
%% @doc <br>Pre:NULL</br><br>SIDE-EFFECT:Applies the function Fun() to every row in table.</br><br>Post:NULL</br>
traverse(Table, Fun) ->
	taux (Table, Fun, ets:first(Table)).

taux(_Table,_Fun, '$end_of_table' ) -> done;
taux(Table,Fun,  Key) ->
        Next = ets:next(Table, Key ),
        Fun(ets:lookup(Table, Key)),
        taux(Table, Fun, Next).

%% @spec fold(Table, Fun) -> any()
%% @doc <br>Pre:NULL</br><br>Post:Works just like foldr/foldl.</br>
fold(Table, Fun) ->
	foldAux (Table, Fun, ets:first(Table), []).

foldAux(_Table,_Fun, '$end_of_table', Ack ) -> Ack;
foldAux(Table, Fun,  Key, Ack) ->
        foldAux(Table, Fun, ets:next(Table, Key), Fun(ets:lookup(Table, Key)) ++ Ack).

hide_tag([]) -> [];
hide_tag([{T, X}|L]) -> 	(case ((T == name) or (T == ip) or (T == port)) of 
					true -> 
						[X]; 
					_-> 
						[]
				end) ++ hide_tag (L).
hide([]) -> [];
hide([{K,L}|R]) -> [{K, hide_tag(L)}] ++ hide(R).

