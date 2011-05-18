%% @author Göran Hagelin, Gabriel Tholsgård
%% @doc modul med DETS API för projektet.
 
-module(dapi).
-export([openTable/2, closeTable/1, add/3,isDets/1, delete/2, retrieve/2, sync/1, open/1]).

-include_lib("eunit/include/eunit.hrl").

%% @doc öppnar ett DETS table eller skapar en ny fil om det behövs. Args är en lista med valmöjligheter när man öppnar ett table, Args kan vara [] för default värden. 
%% @spec openTable(FileName, Args) -> {ok, atom()} | {error, Reason}
%% Args = [{Key, Val}]
%% FileName = atom()

openTable(FileName, Args) ->
    dets:open_file(FileName, Args).

%% @doc stänger en öppen table och skriver alla ändringar till filen på disk.
%% @spec closeTable(Table) -> ok | {error, Reason}
%% Table = atom() | reference()


closeTable(Table) ->
    dets:close(Table).

%% @doc lägger till en tupel {Key, Value} (objekt hädanefter) i Table.
%% @spec add(Table, Key, Value) -> ok | {error, Reason}
%% Table = atom() | reference()
%% Key = any()
%% Value = any()
add(Table,Key, Value) ->
    dets:insert(Table, {Key, Value}).

%% @doc returnerar true om argumentet är ett DETS table.
%% @spec isDets(FileName) -> true | {error, Reason}
%% FileName = atom()
isDets(FileName) ->
    dets:is_dets_file(FileName).

%% @doc tar bort ett objekt ur Table med nyckeln Key.
%% @spec delete(Table, Key) -> ok | {error, Reason}
%% Table = atom() | reference()
%% Key = any()
delete(Table, Key) ->
    dets:delete(Table, Key).

%% @doc skriver alla ändringar i Table till filen på disk.
%% @spec sync(Table) -> ok | {error, Reason}
%% Table = atom() | reference()
sync(Table) ->
    dets:sync(Table).

%% @doc hämtar en lista av objekt från Table med nyckeln Key. Om inga object hittades returneras tom lista.
%% @spec retrieve(Table, Key) -> [{Key, Value | [Values]}] | {error, Reason}
%% Table = atom() | reference()
%% Key = any()
%% Value = any() 
retrieve(Table, Key) ->
    dets:lookup(Table, Key).

%% @doc öppnar ett DETS table om filen FileName är ett DETS table (kan ej skapa nya filer vilket openTable kan). 
%% @spec open(FileName) -> {ok, TableName} | {error, Reason}
%% FileName = atom()| reference()
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

