-module(smtp).
-export([reminder/3]).

reminder(Mail, Option, User, Password) ->
   ssl:start(),
   {ok, Socket} = ssl:connect("smtp.gmail.com", 465, [{active, false}], 1000),
   recv(Socket),
   send(Socket, "HELO localhost"),
   send(Socket, "AUTH LOGIN"),
   send(Socket, binary_to_list(base64:encode("PearTalk@gmail.com"))),
   send(Socket, binary_to_list(base64:encode("minutemaid"))),
   send(Socket, "MAIL FROM: <PearTalk@gmail.com>"),
   send(Socket, "RCPT TO:<"++Mail++">"),
   send(Socket, "DATA"),
   send_no_receive(Socket, "From: PearTalk"),
   send_no_receive(Socket, "To: <"++Mail++">"),
   send_no_receive(Socket, "Date: Tue, 15 Jan 2008 16:02:43 +0000"),
   send_no_receive(Socket, "Subject: Account reminder"),
   send_no_receive(Socket, ""),
   send_no_receive(Socket, Option),
   send_no_receive(Socket, "Username: "++User),
   send_no_receive(Socket, "Password: "++Password),
   send_no_receive(Socket, ""),
   send(Socket, "."),
   send(Socket, "QUIT"),
   ssl:close(Socket).

send_no_receive(Socket, Data) ->
   ssl:send(Socket, Data ++ "\r\n").


send(Socket, Data) ->
   ssl:send(Socket, Data ++ "\r\n"),
   recv(Socket).

recv(Socket) ->
   case ssl:recv(Socket, 0, 1000) of
 {ok, Return} -> io:format("~p~n", [Return]);
 {error, Reason} -> io:format("ERROR: ~p~n", [Reason])
   end.
