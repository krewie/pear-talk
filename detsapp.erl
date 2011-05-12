-module(detsapp).
-export([openTable/2, closeTable/1, add/3,isDets/1, delete/2, retrieve/2, sync/1, info/1, open/1]).

-include_lib("eunit/include/eunit.hrl").

openTable(FileName, Args) ->
    dets:open_file(FileName, Args).

closeTable(Table) ->
    dets:close(Table).

add(Table,Key, Value) ->
    dets:insert(Table, {Key, Value}).

isDets(FileName) ->
    dets:is_dets_file(FileName).

info(Table) ->
    dets:info(Table).

delete(Table, Key) ->
    dets:delete(Table, Key).

sync(Table) ->
    dets:sync(Table).

retrieve(Table, Key) ->
    dets:lookup(Table, Key).
 
open(FileName) ->
    case isDets(FileName) of
	true ->
	    dets:open_file(FileName);
	{error, Reason} -> {error, Reason}
    end.
	      

%% eunit

openTable_test_() ->
    File = openTable(detsapptable.test,[]),
    {_ , Cake} = File,
    [
     ?_assertEqual({ok, Cake}, File),
     ?_assertEqual( ok, closeTable(Cake))
    ].

closeTable_test_() ->
    {_, Cake} = openTable(detsapptable.test, []),
    [
     ?_assertEqual( ok, closeTable(Cake))
    ].

add_test_() ->
    {ok, Cake} = openTable(detsapptable.test,[]),
    [
     ?_assertEqual(ok, add(Cake, "Key", ["smurfs"])),
     ?_assertEqual( ok, closeTable(Cake))
    ].

isDets_test()->
    ?assertEqual(true, isDets(detsapptable.test)).

retrieve_test_()->
    {ok, Cake} = openTable(detsapptable.test,[]),
    [
     ?_assertEqual([{"Key", ["smurfs"]}], retrieve(Cake, "Key")),
     ?_assertEqual( ok, closeTable(Cake))
    ].


sync_test_()->
    {ok, Cake} = openTable(detsapptable.test,[]),
    [
     ?_assertEqual(ok, sync(Cake)), 
     ?_assertEqual( ok, closeTable(Cake))
    ].


delete_test_() ->
    {ok, Cake} = openTable(detsapptable.test,[]),
    [
     ?_assertEqual(ok, delete(Cake, "Key")),
     ?_assertMatch ([], retrieve(Cake, "Key")),
     ?_assertEqual( ok, closeTable(Cake))
    ].

