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
            is_lover/1,
            add_lover/1,
            who_I_am/2,
            wut_I_wunt/2,
            about_moi/2,
            delete/1,
            nous_vous_proposons/1
        ]).

% admin/debug API
-export([
            list_lovers/0
        ]).

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

is_lover(no_identity) -> false;
is_lover(Id) ->
    gen_server:call(?MODULE, {is_lover, Id}).

add_lover(Id) ->
    gen_server:call(?MODULE, {add_lover, Id}).

who_I_am(Id, [{"who", Who}]) ->
    gen_server:call(?MODULE, {who_I_am, Id, Who}).

wut_I_wunt(Id, [{"wut", What}]) ->
    gen_server:call(?MODULE, {wut_I_wunt, Id, What}).

about_moi(Id, Text) ->
    gen_server:call(?MODULE, {about_moi, Id, Text}).

delete(Id) ->
    gen_server:call(?MODULE, {delete, Id}).

nous_vous_proposons(Id) ->
    gen_server:call(?MODULE, {nous_vous_proposons, Id}).

% Admin/debugging fns

list_lovers() ->
    gen_server:call(?MODULE, list_lovers).

% Callbacks

init(_Args) ->
    true = register(?MODULE, self()),
    {ok, #state{}}.

handle_call({is_lover, #{key := Key}}, _From, State) ->
    #state{lovers = Lovers} = State,
    Reply = case maps:is_key(Key, Lovers) of
        false -> false;
        true -> L = maps:get(Key, Lovers),
                {true, maps:get(next, L)}
    end,
    {reply, Reply, State};

handle_call({add_lover, #{name := Name, key := Key}}, _From, State) ->
    #state{lovers     = Lovers,
            no_lovers = No} = State,
    Lover = #{name => Name, next => is},
    NewState = case maps:is_key(Key, Lovers) of
        true  -> State;
        false -> State#state{lovers    = maps:put(Key, Lover, Lovers),
                             no_lovers = No + 1}
    end,
    {reply, ok, NewState};

handle_call({who_I_am, Id, Who}, _From, State) ->
    NewState= update_lover(State, Id, is, Who, wunts),
    {reply, ok, NewState};

handle_call({wut_I_wunt, Id, Wut}, _From, State) ->
    NewState= update_lover(State, Id, wunts, Wut, about_moi),
    {reply, ok, NewState};

handle_call({about_moi, Id, Text}, _From, State) ->
    NewState= update_lover(State, Id, about_moi, Text, complete),
    {reply, ok, NewState};

handle_call({nous_vous_proposons, #{key := Key}}, _From, State) ->
    #state{lovers = Lovers} = State,
    Me = maps:get(Key, Lovers),
    #{is := Is, wunts := Wunts} = Me,
    Matches = match(maps:iterator(Lovers), Is, Wunts, Key, []),
    {reply, Matches, State};

handle_call({delete, #{key := Key}}, _From, State) ->
    #state{lovers = Lovers} = State,
    NewLovers = maps:remove(Key, Lovers),
    {reply, ok, State#state{lovers = NewLovers}};

handle_call(list_lovers, _From, State) ->
    #state{lovers     = Lovers,
            no_lovers = No} = State,
    io:format("there are ~p Lovers~n", [No]),
    print(maps:to_list(Lovers)),
    {reply, ok, State};

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
match(none, _, _, _Key, Acc) -> Acc;
match(Iterator, Is, Wunts, Key, Acc) ->
    {K, V, I} = maps:next(Iterator),
    NewAcc = case V of
        #{is        := Wunts,
          wunts     := Is} ->
            % don't match yourself
            case K of
                Key -> Acc;
                _   -> [{K, V} | Acc]
            end;
        _ ->
            Acc
    end,
    match(I, Is, Wunts, Key, NewAcc).

update_lover(State, #{key :=K}, Key, Value, Next) ->
    #state{lovers = Lovers} = State,
    Lover = maps:get(K, Lovers),
    NewLover  = Lover#{Key => Value, next => Next},
    NewLovers = Lovers#{K => NewLover},
    State#state{lovers = NewLovers}.

print([]) -> ok;
print([{K, V} | T]) ->
    Name     = maps:get(name, V),
    Is       = maps:get(is, V, not_provided_yet),
    Wunts    = maps:get(wunts, V, not_provided_yet),
    AboutMoi = maps:get(about_moi, V, not_provided_yet),
    Next     = maps:get(next, V, whatsup),
    io:format("User name ~p with key starting ~p at stage: ~p~n", [Name, short(K), Next]),
    io:format("- bio: ~p~n", [AboutMoi]),
    io:format("- is: ~p wunts: ~p~n~n", [Is, Wunts]),
    print(T).

short(N) -> [P, R, E, F, I, X | _Rest] = integer_to_list(N),
            [P, R, E, F, I, X].
