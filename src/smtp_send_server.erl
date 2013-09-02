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
-export([set_config/1]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-export([message/5]).

-record(smtp, {name, address, host_name, port, user_name, password}).

start_link(_Args) -> gen_server:start_link({local, ?MODULE}, ?MODULE,
	utils_app:get_env([from, host, auth]), []).

init([
	{from, {Name, Address}},
	{host, {HostName, Port}},
	{auth, {UserName, Password}}
]) ->
	process_flag(trap_exit, true),
	{ok, [#smtp{
		name = Name, address = Address,
		host_name = HostName, port = Port,
		user_name = UserName, password = Password
	}, []]}.

message(To, Subject, Body) ->
	gen_server:call(?MODULE, {message, To, Subject, Body}, infinity).

set_config(Config) -> gen_server:call(?MODULE, {set_config, Config}).

handle_call({message, ToAddress, Subject, Body}, From, [Smtp, Pids]) ->
	Pid = spawn_link(?MODULE, message, [Smtp, ToAddress, Subject, Body, From]),
	{noreply, [Smtp, [{Pid, From}|Pids]]};

handle_call({set_config, Config}, _From, [Smtp, Pids]) ->
	{reply, ok, [lists:foldl(fun(Field, S) -> case Field of
		{from, {Name, Address}} -> S#smtp{name = Name, address = Address};
		{host, {HostName, Port}} -> S#smtp{host_name = HostName, port = Port};
		{auth, {UserName, Password}} ->
			S#smtp{user_name = UserName, password = Password}
	end end, Smtp, Config), Pids]}.

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

message(#smtp{
	name = FromName, address = FromAddress,
	host_name = HostName, port = Port,
	user_name = UserName, password = Password
}, ToAddress, Subject, Body, From) ->
	gen_server:reply(From, smtp_send_backend:message(
		FromName, FromAddress, ToAddress, Subject, Body,
		HostName, Port, UserName, Password
	)).
