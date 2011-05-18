%% @author Kristian Ionescu & Staffan Rodgren.
%% @doc The ets module for the peer client.
%% @copyright 2011 Peer-talk.

-module(rul).
-compile(export_all).
-export([friends/0, fillTable/2, empty/0, show/1, add/3, logout/2, take/2, set_online/4, delete/2, traverse/2]).

-spec friends() -> tab.
%%@doc <h4>friends()</h4><br>Pre:NULL</br><br>Post:Creates an empty ets table with identifier 'friends'.</br><br>Post:NULL</br>
friends()->
	try
		ets:new(friends, [set, named_table, public])
	catch  _:_ ->
		ets:delete(friends),
		friends()
	end.

-spec fillTable(tab, list()) -> atom().
%% @doc <h4>fillTable()</h4><br>Pre: Table with name identifier 'friends' exists.</br><br>SIDE-EFFECT:Fills an already 
%%existing table with values from list().</br><br>Post:NULL</br>
 
fillTable(_, []) -> ok;
fillTable(Table, [Friend|List]) ->
	case Friend of
		[M,[Sn, Pip, Lp]] = Friend ->
			ets:insert(Table, {M, [Sn, Pip, Lp]});
		[M, [Sn]] = Friend ->
			ets:insert(Table, {M, [Sn]})
	end,
	fillTable(Table, List). 

-spec empty() -> tab.
%% @doc <h4>empty()</h4><br>Pre:NULL</br><br>SIDE-EFFECT:Spawns a new process that creates an empty ets
%%table.</br><br>Post:Returns an empty ets table.</br>

empty() -> spawn(fun() -> ets:new(tom, [set, public]), receive _X -> ok end end).

-spec show(tab) -> io_device().
%% @doc <h4>show(Tab())</h4><br>Pre: NULL</br><br>Post:Prints table content to screen.</br>
show(Table) ->
	rul:traverse(Table,fun(X) -> io:format("~p~n", [X]) end).

-spec add(tab, string(), string()) -> bool().
%% @doc <h4>add(Tab(), String(), String())</h4><br>Pre:NULL</br><br>SIDE-EFFECT:Inserts a new entry in table with the second argument as the key and the third as the 'showedname'.</br><br>Post:NULL</br>

add(Table, Mail, ShowedName) ->
	case take(Table, Mail) of
	{error, nomatch} ->
		ets:insert(Table,{Mail,[ShowedName]});
	_ ->
		{error, allready_existing_friend}
	end.

-spec logout(tab, string()) -> bool().
%% @doc <h4>logout(Tab(), String())</h4><br>Pre:NULL</br><br>SIDE-EFFECT:Removes tcp/ip information about a user in table when the user logout.</br><br>Post:NULL</br>
logout(Table, Mail) ->
	try
		[ShowedName,_,_] = take(Table, Mail),
		ets:insert(Table,{Mail,[ShowedName]}),		
		ok
	catch		
		_:_ -> 
			{error,user_allready_offline}
	end.

-spec take(tab, string()) -> list().
%% @doc <h4>take(Tab(), String())</h4><br>Pre:NULL</br><br>Post:Returns the value corresponding to the key, else {error, nomatch}.</br>
take(Table, Mail) ->
	try
		[{_,Value}] = ets:lookup(Table, Mail),
		Value
	catch
		_:_ ->
			{error, nomatch}
	end.

-spec set_online(tab, string(),tuple(), integer()) -> bool().
%%@doc <h4>set_online(Tab(), String(), Tuple(), Int())</h4><br>Pre:NULL</br><br>SIDE-EFFECT:Saves the tcp/ip information about a user in table when user is online.</br><br>Post:NULL</br>
set_online(Table, Sender, SenderIP, SenderPort) ->
	try
		[Name] = take(Table, Sender),
		ets:insert(Table,{Sender,[Name, SenderIP, SenderPort]}),
		ok
	catch		
		Ek:En -> 
			{Ek,En}
	end.
-spec delete(tab, string()) -> bool().
%% @doc <h4>delete(Tab(), String())</h4><br>Pre:NULL</br><br>SIDE-EFFECT: Deletes the row  in table with key as second argument.</br><br>Post:NULL</br>
delete(Table, Mail) ->
	dets:delete(Table, Mail).

-spec traverse(tab, fun()) -> atom().
%% @doc <h4>delete(Tab(), String())</h4><br>Pre:NULL</br><br>SIDE-EFFECT:Applies the function Fun() to every row in table.</br><br>Post:NULL</br>
traverse(Table, Fun) ->
	taux (Table, Fun, ets:first(Table)).

taux(_Table,_Fun, '$end_of_table' ) -> done;
taux(Table,Fun,  Key) ->
        Next = ets:next(Table, Key ),
        Fun(ets:lookup(Table, Key)),
        taux(Table, Fun, Next).

