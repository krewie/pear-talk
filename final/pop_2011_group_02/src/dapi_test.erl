-module(dapi_test).

-include_lib("eunit/include/eunit.hrl"). 

openTable_test_() ->
    File = dapi:openTable(dapitable.test,[]),
    {_ , Cake} = File,
    [
     ?_assertEqual({ok, Cake}, File),
     ?_assertEqual( ok, dapi:closeTable(Cake))
    ].

closeTable_test_() ->
    {_, Cake} = dapi:openTable(dapitable.test, []),
    [
     ?_assertEqual( ok, dapi:closeTable(Cake))
    ].

add_test_() ->
    {ok, Cake} = dapi:openTable(dapitable.test,[]),
    [
     ?_assertEqual(ok, dapi:add(Cake, "Key", ["smurfs"])),
     ?_assertEqual( ok, dapi:closeTable(Cake))
    ].

isDets_test()->
    ?assertEqual(true, dapi:isDets(dapitable.test)).

retrieve_test_()->
    {ok, Cake} = dapi:openTable(dapitable.test,[]),
    [
     ?_assertEqual([{"Key", ["smurfs"]}], dapi:retrieve(Cake, "Key")),
     ?_assertEqual( ok, dapi:closeTable(Cake))
    ].

member_test_() ->
    {ok, Cake} = dapi:openTable(dapitable.test,[]),
    [
     ?_assertEqual(true, dapi:member(Cake, "Key")),
     ?_assertEqual( ok, dapi:closeTable(Cake))
    ].

sync_test_()->
    {ok, Cake} = dapi:openTable(dapitable.test,[]),
    [
     ?_assertEqual(ok, dapi:sync(Cake)), 
     ?_assertEqual( ok, dapi:closeTable(Cake))
    ].


delete_test_() ->
    {ok, Cake} = dapi:openTable(dapitable.test,[]),
    [
     ?_assertEqual(ok, dapi:delete(Cake, "Key")),
     ?_assertMatch ([], dapi:retrieve(Cake, "Key")),
     ?_assertEqual( ok, dapi:closeTable(Cake))
    ].