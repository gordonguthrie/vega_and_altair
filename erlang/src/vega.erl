-module(vega).

-export([
			home/1,
			'51'/1
		]).

home(Route) ->
	io:format("Route is ~p~n", [Route]),
	"20 text/gemini home\r\n".

'51'(Route) ->
	 io:format("Route is ~p~n", [Route]),
	"20 text/gemini 👽\r\n".
