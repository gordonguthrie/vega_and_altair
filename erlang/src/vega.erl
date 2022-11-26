-module(vega).

-export([
			root/2,
			home/2
		]).

root(Route, Vals) ->
	#{path := Path} = Route,
	io:format("in vega:root Path is ~p Vals is~p~n", [Path, Vals]),
	["20 text/gemin\r\nyou are in root\r\n"].

home(Route, Vals) ->
	#{path := Path} = Route,
	io:format("in vega:home Path is ~p Vals is ~p~n", [Path, Vals]),
	["20 text/gemini\r\nhome\r\n"].

