%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(altair_lovers).

% http://erlang.org/doc/design_principles/gen_server_concepts.html
-behaviour(gen_server).

% API
-export([start_link/0, start_link/1, start_link/2]).
-export([start_link_local/0, start_link_local/1, start_link_local/2]).

% Callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


% function API
-export([
            add_lover/1,
            who_am_I/2,
            wut_I_wunt/2,
            list_lovers/0
        ]).

-record(lover, {name = [], is = undefined, wunts = undefined, dates = []}).
-record(state, {lovers = #{}, no_lovers = 0}).

% API

% see: http://erlang.org/doc/man/gen_server.html#start_link-3
start_link_local() ->
    start_link_local(#{}).

start_link_local(Args) ->
    start_link_local(Args, []).

start_link_local(Args, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

start_link() ->
    start_link(#{}).

start_link(Args) ->
    start_link(Args, []).

start_link(Args, Opts) ->
    gen_server:start_link(?MODULE, Args, Opts).

%% Functional API

add_lover(Id) ->
    io:format("adding lover ~p~n", [Id]),
    gen_server:call(?MODULE, {add_lover, Id}).

who_am_I(Id, Who) ->
    gen_server:call(?MODULE, {who_am_I, Id, Who}).

wut_I_wunt(Id, What) ->
    gen_server:call(?MODULE, {wut_I_wunt, Id, What}).

list_lovers() ->
    gen_server:call(?MODULE, list_lovers).

% Callbacks

init(_Args) ->
    true = register(?MODULE, self()),
    io:format("starting altair lovers~n"),
    {ok, #state{}}.

handle_call({add_lover, #{name := Name, key := Key}}, _From, State) ->
    #state{lovers     = Lovers,
            no_lovers = No} = State,
    Lover = #lover{name = Name},
    NewState = case maps:is_key(Key, Lovers) of
        true  -> State;
        false -> State#state{lovers    = maps:put(Key, Lover, Lovers),
                             no_lovers = No + 1}
    end,
    {reply, ok, NewState};
handle_call(list_lovers, _From, State) ->
    #state{lovers     = Lovers,
            no_lovers = No} = State,
    {reply, {No, maps:to_list(Lovers)}, State};
handle_call(Request, _From, State) ->
    io:format("got request ~p~n", [Request]),
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
