%%%-------------------------------------------------------------------
%%% Created: 20 Mar 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(usage).
-export([start/0]).

start() -> smtp_send:message("username@example.com", "Subject", "Body").
