-module(vega).

% user API
-export([
			root/2,
			lover/2,
			is/2,
			wunts/2,
			about/2
		]).

% administrator API
-export([
			admin/2
		]).

-import(vega_urls, [
					 	make_link/3,
					 	make_action_link/4,
					 	make_redirect/1,
					 	make_input/1,
					 	make_unicode/1
				    ]).

% route handlers API

root(#{id := no_identity} = _Route, _Vals) ->
	Name =  "Stranger",
	Page =
		[
			belka_templates:render("home_body", [{name, Name}]),
			make_unicode("## 👓 identify yourself!")
		],
	io:format("Page is ~p~n", [Page]),
	Page;
root(#{id := #{name := Name} = Id} = _Route, _Vals) ->
	IsLover = altair_lovers:is_lover(Id),
	Footer = case IsLover of
		false             -> make_link(["/"], "lover", "😻 What's ya story, pussycat?");
		{true, is}        -> make_link(["/"], "lover", "😻 Who are ya, pussycat?");
		{true, wunts}     -> make_link(["/"], "lover", "😻 Wut ❤️ seeks ya, pussycat?");
		{true, about_moi} -> make_link(["/"], "lover", "💌 Da balcony is yours, Juliette");
		{true, complete}  -> make_link(["/"], "",      "😻 🏹 💘💞")
	end,
	[
		belka_templates:render("home_body", [{name, Name}]),
		Footer
	].

lover(#{id := Id}, _Vals) ->
	Path = ["lover"],
	#{name := Name} = Id,
	IsLover = altair_lovers:is_lover(Id),
	case IsLover of
		false ->
			altair_lovers:add_lover(Id),
			make_redirect("/lover");
		{true, is} ->
			[
				belka_templates:render("lover_is", [{name, Name}]),
				make_action_link(Path, "am/str8boi",   Id, "♂ str8 boi"),
				make_action_link(Path, "am/lezzer",    Id, "⚢ lezzer"),
				make_action_link(Path, "am/str8gurl",  Id, "♂ str8 gurl"),
				make_action_link(Path, "am/transmasc", Id, "🤴 trans masc"),
				make_action_link(Path, "am/fairy",     Id, "⚣ fairy"),
				make_action_link(Path, "am/bi",        Id, "⚤ bi"),
				make_action_link(Path, "am/polly",     Id, "🦜 polly"),
				make_action_link(Path, "am/transgurl", Id, "🚂 trans gurl")
			];
		{true, wunts} ->
			[
				belka_templates:render("lover_wunts", []),
				make_action_link(Path, "wunts/str8boi",   Id, "♂ str8 boi"),
				make_action_link(Path, "wunts/lezzer",    Id, "⚢ lezzer"),
				make_action_link(Path, "wunts/str8gurl",  Id, "♂ str8 gurl"),
				make_action_link(Path, "wunts/transmasc", Id, "🤴 trans masc"),
				make_action_link(Path, "wunts/fairy",     Id, "⚣ fairy"),
				make_action_link(Path, "wunts/bi",        Id, "⚤ bi"),
				make_action_link(Path, "wunts/polly",     Id, "🦜 polly"),
				make_action_link(Path, "wunts/transgurl", Id, "🚂 trans gurl")
			];
		{true, about_moi} ->
			[
				belka_templates:render("lover_about_moi", []),
				make_action_link(["lover"], "about/moi", Id, "💌 shoot yer shot, make yer pitch 🏹")
			];
		{true, complete}  ->
			make_redirect("/")
	end.

is(#{id := Id}, Vals) ->
	ok = altair_lovers:who_I_am(Id, Vals),
	make_redirect("/lover").

wunts(#{id := Id}, Vals) ->
	ok = altair_lovers:wut_I_wunt(Id, Vals),
	make_redirect("/lover").

about(#{querykvs := []} =_Route, _Vals) ->
	% send the user home
	make_input("give it your best shot");
about(#{id := Id, querykvs := [{Text, true}]} = _Route, _Vals) ->
	ok = altair_lovers:about_moi(Id, Text),
	make_redirect("/").

admin(_Route, _Vals) ->
	[
		belka_templates:render("admin", [])
	].

% internal functions

