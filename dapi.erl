%% @author Göran Hagelin, Gabriel Tholsgård
%% @doc modul med DETS API för projektet.
 
-module(dapi).
-export([openTable/2, closeTable/1, add/3,isDets/1, delete/2, retrieve/2, sync/1, open/1]).

-include_lib("eunit/include/eunit.hrl").

%% @doc openTable funktion som öppnar ett DETS table eller skapar en ny fil om det behövs. Args är en lista med valmöjligheter när man öppnar ett table, Args kan vara [] för default värden. 
%% @spec atom * {Key, Value} list ->  {ok, TableName} | {error, Reason}

openTable(FileName, Args) ->
    dets:open_file(FileName, Args).

%% @doc closeTable funktion som stänger en öppen table och skriver alla ändringar till filen på disk.
%% @spec TableName -> ok | {error, Reason}
closeTable(Table) ->
    dets:close(Table).

%% @doc add funktion som lägger till en tupel {Key, Value} (objekt hädanefter) i Table.
%% @spec TableName, String, Value | [Values] -> ok | {error, Reason}
add(Table,Key, Value) ->
    dets:insert(Table, {Key, Value}).

%% @doc isDets funktion som returnerar true om argumentet är ett DETS table.
%% @spec atom -> true | {errorl, Reason}
isDets(FileName) ->
    dets:is_dets_file(FileName).

%% @doc delete funktion som tar bort ett objekt ur Table med nyckeln Key.
%% @spec TableName * String -> ok | {error, Reason}
delete(Table, Key) ->
    dets:delete(Table, Key).

%% @doc sync funktion som skriver alla ändringar i Table till filen på disk.
%% @spec TableName -> ok | {error, Reason}
sync(Table) ->
    dets:sync(Table).

%% @doc retrieve funktion som hämtar en lista av objekt från Table med nyckeln Key. Om inga object hittades returneras tom lista.
%% @spec TableName * String -> [{String, Value | [Values]}] | {error, Reason}
retrieve(Table, Key) ->
    dets:lookup(Table, Key).

%% @doc open funktion som öppnar ett DETS table om filen FileName är ett DETS table (kan ej skapa nya filer vilket openTable kan). 
%% @spec atom -> {ok, TableName} | {error, Reason}
open(FileName) ->
    case isDets(FileName) of
	true ->
	    dets:open_file(FileName);
	{error, Reason} -> {error, Reason}
    end.
	      

%% eunit tests

openTable_test_() ->
    File = openTable(dapitable.test,[]),
    {_ , Cake} = File,
    [
     ?_assertEqual({ok, Cake}, File),
     ?_assertEqual( ok, closeTable(Cake))
    ].

closeTable_test_() ->
    {_, Cake} = openTable(dapitable.test, []),
    [
     ?_assertEqual( ok, closeTable(Cake))
    ].

add_test_() ->
    {ok, Cake} = openTable(dapitable.test,[]),
    [
     ?_assertEqual(ok, add(Cake, "Key", ["smurfs"])),
     ?_assertEqual( ok, closeTable(Cake))
    ].

isDets_test()->
    ?assertEqual(true, isDets(dapitable.test)).

retrieve_test_()->
    {ok, Cake} = openTable(dapitable.test,[]),
    [
     ?_assertEqual([{"Key", ["smurfs"]}], retrieve(Cake, "Key")),
     ?_assertEqual( ok, closeTable(Cake))
    ].


sync_test_()->
    {ok, Cake} = openTable(dapitable.test,[]),
    [
     ?_assertEqual(ok, sync(Cake)), 
     ?_assertEqual( ok, closeTable(Cake))
    ].


delete_test_() ->
    {ok, Cake} = openTable(dapitable.test,[]),
    [
     ?_assertEqual(ok, delete(Cake, "Key")),
     ?_assertMatch ([], retrieve(Cake, "Key")),
     ?_assertEqual( ok, closeTable(Cake))
    ].

