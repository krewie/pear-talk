-module(upnp).
-compile(export_all).
-define (ADDR, {225,0,0,25} ).
-define (PORT,8080).
-define (N,4).

open(Addr,Port) ->
   {ok,S} = gen_udp:open(Port, [{reuseaddr,true}, {multicast_ttl,220}, {multicast_loop,true}, binary]),
   inet:setopts(S,[{add_membership,{Addr,{0,0,0,0}}}]),
   {S, Port}.

tryconn(Addr, Port, N) ->
	try
		open(Addr, Port)
	catch 
		_:_ ->
			case (Port =< ?PORT + N) of
				true ->
					tryconn(Addr, Port+1, N);
				_->
					error
			end
	end. 

nsend(S, Addr, Port, N, Obj) ->
	gen_udp:send(S, Addr, Port, Obj),
	case (Port =< ?PORT + N) of
		true ->
			nsend(S, Addr, Port+1, N, Obj);
		_->
			done
	end. 

server(Me) -> 
   try
   		{S, Port} = tryconn(?ADDR, ?PORT, ?N),
   		Pid = spawn(?MODULE,receiver,[S, Me, Port]),
   		register(upnp, Pid),
   		gen_udp:controlling_process(S, Pid),
   		Port
   catch 
   		_:_ ->
   				error
   end.

receiver(S, Me, Port) -> 
    receive
	{message, Receiver_username, Obj} ->
		  nsend(S, ?ADDR , ?PORT, ?N, term_to_binary({Receiver_username, Obj})),
    		  receiver(S, Me, Port);
      {udp, _, _, _, Bin} ->
		{Usr, Obj} = binary_to_term(Bin),
		case Usr of
		    	Me ->
		    		spawn(?MODULE,send,[Port, Obj]),
		    		receiver(S, Me, Port);
		    	_->
			 		receiver(S, Me, Port)
		end;
	stop ->
            gen_udp:close(S),
            ok;
	X ->
	  	io:format("Eh?~p,~n",[X]),
		receiver(S, Me, Port)	
    end.


send(Port, Obj) ->
    try
      {ok, Sock} = gen_tcp:connect(localhost, Port, [binary,{active, false}]),
	  gen_tcp:send(Sock, term_to_binary(Obj)),
	  gen_tcp:close(Sock)
    catch 
	Ek:En ->
	    {Ek,En}
    end.

