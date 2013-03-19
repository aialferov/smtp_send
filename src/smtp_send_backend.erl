%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 19 Mar 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(smtp_send_backend).
-export([message/9]).

-define(CRLF, "\r\n").
-define(SP, " ").

-define(EHLO, "EHLO" ++ ?CRLF).

-define(STARTTLS, "STARTTLS" ++ ?CRLF).
-define(AUTH(Type, Data), "AUTH" ++ ?SP ++ Type ++ ?SP ++ Data ++ ?CRLF).

-define(FROM(From), "MAIL FROM:" ++ "<" ++ From ++ ">" ++ ?CRLF).
-define(TO(To), "RCPT TO:" ++ "<" ++ To ++ ">" ++ ?CRLF).

-define(DATA, "DATA" ++ ?CRLF).

-define(DATA(FromName, From, To, Subject, Body),
	"From:" ++ ?SP ++ FromName ++ ?SP ++ "<" ++ From ++ ">" ++ ?CRLF ++ 
	"To:" ++ ?SP ++ To ++ ?CRLF ++
	"Subject:" ++ ?SP ++ Subject ++ ?CRLF ++ ?CRLF ++
	Body ++ ?CRLF ++ "." ++ ?CRLF
).
-define(Subject(Subject), "Subject:" ++ ?SP ++ Subject ++ ?CRLF ++ ?CRLF).

-define(QUIT, "QUIT" ++ ?CRLF).

-define(ServiceReady, "220" ++ _).
-define(ServiceClosing, "221" ++ _).
-define(AuthSuccess, "235" ++ _).
-define(ActionOK, "250" ++ _).
-define(StartMailInput, "354" ++ _).

message(
	FromName, FromAddress, ToAddress, Subject, Body,
	HostName, Port, UserName, Password
) ->
	{ok, Socket} = gen_tcp:connect(HostName, Port, [{active, false}]),
	{ok, ?ServiceReady} = gen_tcp:recv(Socket, 0),
	gen_tcp:send(Socket, ?EHLO),
	{ok, ?ActionOK} = gen_tcp:recv(Socket, 0),
	gen_tcp:send(Socket, ?STARTTLS),
	{ok, ?ServiceReady} = gen_tcp:recv(Socket, 0),
	{ok, TlsSocket} = ssl:connect(Socket, [{active, false}]),
	ssl:send(TlsSocket, ?EHLO),
	{ok, ?ActionOK} = ssl:recv(TlsSocket, 0),
	ssl:send(TlsSocket, auth(plain, UserName, Password)),
	{ok, ?AuthSuccess} = ssl:recv(TlsSocket, 0),
	ssl:send(TlsSocket, ?FROM(FromAddress)),
	{ok, ?ActionOK} = ssl:recv(TlsSocket, 0),
	ssl:send(TlsSocket, ?TO(ToAddress)),
	{ok, ?ActionOK} = ssl:recv(TlsSocket, 0),
	ssl:send(TlsSocket, ?DATA),
	{ok, ?StartMailInput} = ssl:recv(TlsSocket, 0),
	ssl:send(TlsSocket, ?DATA(FromName,
		FromAddress, ToAddress, Subject, Body)),
	{ok, ?ActionOK} = ssl:recv(TlsSocket, 0),
	ssl:send(TlsSocket, ?QUIT),
	{ok, ?ServiceClosing} = ssl:recv(TlsSocket, 0),
	ssl:close(TlsSocket).

auth(plain, UserName, Password) ->
	?AUTH("PLAIN", binary_to_list(base64:encode(
		[0] ++ UserName ++ [0] ++ Password))).
