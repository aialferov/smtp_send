%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 19 Mar 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(smtp_send_server).
-behaviour(gen_server).

-export([start_link/1]).
-export([message/3]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

start_link(_Args) -> gen_server:start_link({local, ?MODULE}, ?MODULE,
	utils_app:get_env([from, host_name, port, user_name, password]), []).

init([
	{from, {Name, Address}},
	{host_name, HostName}, {port, Port},
	{user_name, UserName}, {password, Password}
]) ->
	process_flag(trap_exit, true),
	{ok, [{Name, Address, HostName, Port, UserName, Password}, []]}.

message(To, Subject, Body) ->
	gen_server:call(?MODULE, {message, To, Subject, Body}, infinity).

handle_call(
	{message, ToAddress, Subject, Body}, From,
	[Smtp = {FromName, FromAddress, HostName, Port, UserName, Password}, Pids]
) ->
	Pid = spawn_link(fun() -> message(FromName, FromAddress, ToAddress,
		Subject, Body, HostName, Port, UserName, Password, From) end),
	{noreply, [Smtp, [{Pid, From}|Pids]]}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(Info, [Smtp, Pids]) ->
	{noreply, [Smtp, lists:delete(case Info of
		{'EXIT', Pid, normal} -> lists:keyfind(Pid, 1, Pids);
        {'EXIT', Pid, Reason} ->
			{Pid, From} = lists:keyfind(Pid, 1, Pids),
			gen_server:reply(From, {error, Reason}),
			{Pid, From}
	end, Pids)]}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

message(
	FromName, FromAddress, ToAddress, Subject, Body,
	HostName, Port, UserName, Password, From
) ->
	gen_server:reply(From, smtp_send_backend:message(
		FromName, FromAddress, ToAddress, Subject, Body,
		HostName, Port, UserName, Password
	)).
