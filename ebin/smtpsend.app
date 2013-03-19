%%%-------------------------------------------------------------------
%%% @author Anton I Alferov <casper@ubca-dp>
%%% @copyright (C) 2013, Anton I Alferov
%%%
%%% Created: 19 Mar 2013 by Anton I Alferov <casper@ubca-dp>
%%%-------------------------------------------------------------------

{application, smtpsend, [
	{id, "smtpsend"},
	{vsn, "0.0.1"},
	{description, "Simple SMTP sender."},
	{modules, [
		smtpsend,
		smtp_send_app,
		smtp_send_sup,
		smtp_send_server,
		smtp_send_backend
	]},
	{registered, [smtpsend_server]},
	{applications, [kernel, stdlib, sasl]},
	{mod, {smtp_send_app, []}}
]}.
