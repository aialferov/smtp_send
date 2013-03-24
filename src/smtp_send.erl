%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 19 Mar 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(smtp_send).

-export([start/0, stop/0]).
-export([message/3]).

start() -> application:start(?MODULE).
stop() -> application:stop(?MODULE).

message(To, Subject, Text) -> smtp_send_server:message(To, Subject, Text).
