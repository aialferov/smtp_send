%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 19 Mar 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

-module(smtp_send_app).
-behaviour(application).

-export([start/2, stop/1]).
-export([config_change/3]).

start(_StartType, StartArgs) -> smtp_send_sup:start_link(StartArgs).
stop(_State) -> ok.

config_change(Changed, _New, _Removed) -> smtp_send_server:set_config(Changed).
