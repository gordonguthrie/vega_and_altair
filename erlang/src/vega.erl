-module(vega).

-export([
			root/2,
			lover/2,
			is/2,
			wunts/2,
			admin/2
		]).

% route handlers API

root(Route, _Vals) ->
	[
		"20 text/gemini\r\n",
		<<"### Welcome to Vega and Altair, star-crossed lover 💫😻.\r\n"/utf8>>,
		"\r\n",
		"May you find whoever you are seekin.\r\n",
		"\r\n",
		<<"This is a 👻 site - data is never saved. Conversations  ⚰️  when you choose to kill them or if neither party has contributed in 7 days.\r\n"/utf8>>,
		"\r\n",
		<<"Everythin also  ⚰️   if the system goes down for maintenance.\r\n"/utf8>>,
		"\r\n",
		"You can, of course, delete your profile at any time and feel free to use a disposable identity.\r\n",
		"\r\n",
		"\r\n"
	] ++ get_footer(Route).

lover(#{id := Id}, _Vals) ->
	Path = ["lover"],
	ok = altair_lovers:add_lover(Id),
	#{name := Name} = Id,
	[
		<<"20 text/gemini\r\n### 😻 Hey "/utf8>>,
		Name,
		", who are ya pussycat?\r\n",
		"\r\n",
		<<"If any 😼 is lookin for any of these things, they'll find ya.\r\n"/utf8>>,
		"\r\n",
		make_link(Path, "am/str8boi",   Id, <<"♂ str8 boi"/utf8>>),
		make_link(Path, "am/lezzer",    Id, <<"⚢ lezzer"/utf8>>),
		make_link(Path, "am/str8gurl",  Id, <<"♂ str8 gurl"/utf8>>),
		make_link(Path, "am/transmasc", Id, <<"🤴 trans masc"/utf8>>),
		make_link(Path, "am/fairy",     Id, <<"⚣ fairy"/utf8>>),
		make_link(Path, "am/bi",        Id, <<"⚤ bi"/utf8>>),
		make_link(Path, "am/polly",     Id, <<"🦜 polly"/utf8>>),
		make_link(Path, "am/transgurl", Id, <<"🚂 trans gurl"/utf8>>)
	].

is(#{id := Id} = Route, Vals) ->
	io:format("in is route is ~p~nVals is ~p~n", [Route, Vals]),
	Path = ["lover"],
	ok = altair_lovers:who_am_I(Id, Vals),
	[
		"20 text/gemini\r\n",
		<<"=> lover/desires 😻 Wut seeks ya ❤️ pussycat?"/utf8>>,
		"\r\n",
		make_link(Path, "wunts/str8boi",   Id, <<"♂ str8 boi"/utf8>>),
		make_link(Path, "wunts/lezzer",    Id, <<"⚢ lezzer"/utf8>>),
		make_link(Path, "wunts/str8gurl",  Id, <<"♂ str8 gurl"/utf8>>),
		make_link(Path, "wunts/transmasc", Id, <<"🤴 trans masc"/utf8>>),
		make_link(Path, "wunts/fairy",     Id, <<"⚣ fairy"/utf8>>),
		make_link(Path, "wunts/bi",        Id, <<"⚤ bi"/utf8>>),
		make_link(Path, "wunts/polly",     Id, <<"🦜 polly"/utf8>>),
		make_link(Path, "wunts/transgurl", Id, <<"🚂 trans gurl"/utf8>>)

	].

wunts(#{id := Id} = Route, Vals) ->
	io:format("in desires route is ~p~nVals is ~p~n", [Route, Vals]),
	ok = altair_lovers:wut_I_wunt(Id, Vals),
	[
		<<"20 text/gemini\r\n 🏹 shot, looking for a ❤️\r\n"/utf8>>
	].

admin(Route, Vals) ->
	#{path := Path} = Route,
	io:format("in vega:admin Path is ~p Vals is ~p~n", [Path, Vals]),
	[
		"20 text/gemini\r\n",
		"Welcome Administrator\r\n",
		"=> admin/nonce2F24DC1FE7AEEB1E8F8BC906232A7DB9 try and do somethin\r\n"
	].

% internal functions

make_link(Path, Extension, Id, Text) when is_binary(Text) ->
	URL = string:join(Path ++ [Extension], "/"),
	Nonce = belka_router:get_nonce(URL, Id),
	Start = list_to_binary("=> " ++ "/" ++ URL ++ "/" ++ Nonce ++ " "),
	<<Start/binary, Text/binary, "\r\n">>.

get_footer(#{id := no_identity}) ->
	[<<"👓 identify yourself!\r\n"/utf8>>];
get_footer(_) ->
	[<<"=> /lover 😻 what's ya story, pussycat?\r\n"/utf8>>].