%%%-------------------------------------------------------------------
%% @doc vega and altair public API
%% @end
%%%-------------------------------------------------------------------

-module(vega_and_altair).

-behaviour(application).

%% normal application API
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = ssl:start(),
    Port = 1965,
    CertFile = "/vega_and_altair/priv/keys/server.crt",
    KeyFile  = "/vega_and_altair/priv/keys/server.key",
    _PID = laika:start(Port, CertFile, KeyFile, {laika_router, dispatch}),
    vega_and_altair_sup:start_link().

stop(_State) ->
    ok.
