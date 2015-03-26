%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 19 Mar 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(smtp_send_backend).
-export([message/9]).

-define(ConnectionTimeout, 60000).

-define(CRLF, "\r\n").
-define(SP, " ").

-define(EHLO(Service), "EHLO" ++ ?SP ++ Service ++ ?CRLF).

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

-define(QUIT, "QUIT" ++ ?CRLF).

-define(ServiceReady, "220" ++ _).
-define(ServiceClosing, "221" ++ _).
-define(AuthSuccess, "235" ++ _).
-define(ActionOK, "250" ++ _).
-define(AuthMethodsLine, "250-AUTH ").
-define(AuthMethodsLine2, "250 AUTH ").
-define(StartTlsLine, "250-STARTTLS" ++ _).
-define(ActionOK(Response), "250" ++ Response).
-define(PasswordChallenge, "334 UGFzc3dvcmQ6" ++ ?CRLF).
-define(StartMailInput, "354" ++ _).

message(
	FromName, FromAddress, ToAddress, Subject, Body,
	HostName, Port, UserName, Password
) ->
	{TcpModule, Socket, [AuthMethod|_]} = initialize({HostName, Port}),
	authorize({TcpModule, Socket}, AuthMethod, UserName, Password),
	{ok, ?AuthSuccess} = recv(TcpModule, Socket),
	TcpModule:send(Socket, ?FROM(FromAddress)),
	{ok, ?ActionOK} = recv(TcpModule, Socket),
	TcpModule:send(Socket, ?TO(ToAddress)),
	{ok, ?ActionOK} = recv(TcpModule, Socket),
	TcpModule:send(Socket, ?DATA),
	{ok, ?StartMailInput} = recv(TcpModule, Socket),
	TcpModule:send(Socket,
		?DATA(FromName, FromAddress, ToAddress, Subject, Body)),
	{ok, ?ActionOK} = recv(TcpModule, Socket),
	TcpModule:send(Socket, ?QUIT),
	{ok, ?ServiceClosing} = recv(TcpModule, Socket),
	TcpModule:close(Socket).

initialize({HostName, Port}) ->
	{ok, Socket} = gen_tcp:connect(HostName, Port,
		[{active, false}], ?ConnectionTimeout),
	{ok, ?ServiceReady} = recv(gen_tcp, Socket),
	initialize({gen_tcp, Socket}, HostName).

initialize(Tcp = {TcpModule, Socket}, Service) ->
	TcpModule:send(Socket, ?EHLO(Service)),
	{ok, ?ActionOK(Response)} = recv(TcpModule, Socket),
	TlsRequired = TcpModule =/= ssl andalso is_tls_required(Response),
	initialize(Tcp, Service, {tls_required, TlsRequired}, Response).

initialize({_TcpModule, Socket}, Service, {tls_required, true}, _Response) ->
	gen_tcp:send(Socket, ?STARTTLS),
	{ok, ?ServiceReady} = recv(gen_tcp, Socket),
	{ok, TlsSocket} = ssl:connect(Socket, [{active, false}]),
	initialize({ssl, TlsSocket}, Service);

initialize({TcpModule, Socket}, _Service, {tls_required, false}, Response) ->
	{TcpModule, Socket, auth_methods(Response)}.

authorize({TcpModule, Socket}, login, UserName, Password) ->
	TcpModule:send(Socket,
		?AUTH("LOGIN", binary_to_list(base64:encode(UserName)))),
	{ok, ?PasswordChallenge} = recv(TcpModule, Socket),
	TcpModule:send(Socket, binary_to_list(base64:encode(Password)) ++ ?CRLF);

authorize({TcpModule, Socket}, plain, UserName, Password) ->
	TcpModule:send(Socket, ?AUTH("PLAIN",
		binary_to_list(base64:encode([0] ++ UserName ++ [0] ++ Password)))).

is_tls_required(?StartTlsLine) -> true;
is_tls_required([_|T]) -> is_tls_required(T);
is_tls_required([]) -> false.

auth_methods(?AuthMethodsLine ++ T) -> auth_methods(T, [], []);
auth_methods(?AuthMethodsLine2 ++ T) -> auth_methods(T, [], []);
auth_methods([_|T]) -> auth_methods(T);
auth_methods([]) -> [].
auth_methods(?SP ++ T, Method, Methods) ->
	auth_methods(T, [], add_auth_method(Method, Methods));
auth_methods(?CRLF ++ _, Method, Methods) -> auth_methods([], Method, Methods);
auth_methods([H|T], Method, Methods) -> auth_methods(T, [H|Method], Methods);
auth_methods([], Method, Methods) ->
	lists:reverse(add_auth_method(Method, Methods)).

add_auth_method(Method, Methods) ->
	[list_to_atom(string:to_lower(lists:reverse(Method)))|Methods].

recv(TcpModule, Socket) -> recv(TcpModule, Socket, []).
recv(TcpModule, Socket, Acc) ->
	{ok, Data} = TcpModule:recv(Socket, 0),
	case ends_with(Data, ?CRLF) of
		true -> {ok, Acc ++ Data};
		false -> recv(TcpModule, Socket, Acc ++ Data)
	end.

ends_with(String, End) -> string:right(String, string:len(End)) == End.
