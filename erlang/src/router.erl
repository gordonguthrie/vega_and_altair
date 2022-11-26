%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(router).

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
         code_change/3,
         dispatch/1,
         '51'/2,
         '60'/2,
         '60 (nonce)'/2,
         make_nonce/3
         ]).

-record(state, {routes = [], salt = ""}).

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

dispatch(Route) ->
    {{M, F}, Vals} = gen_server:call(?MODULE, {route, Route}),
    apply(M, F, [Route, Vals]).

% Callbacks

'51'(_Route, _Vals) ->
    [<<"51 Welcome to Area 51 👽\r\n"/utf8>>].

'60'(_Route, _Vals)->
    [<<"60 Criminal Code Section 60 Violation 👮\r\n"/utf8>>].

'60 (nonce)'(_Route, _Vals)->
    [<<"60 Criminal Code Section 60 Violation - you're goin to jail ya nonce 🚓\r\n"/utf8>>].

init(_Args) ->
    {ok, Salt} = application:get_env(vega_and_altair, salt),
    true = register(?MODULE, self()),
    Routes = get_routes(),
    CompiledRoutes = compile_routes(Routes),
    {ok, #state{routes = CompiledRoutes, salt = Salt}}.

handle_call({route, Route}, _From, State) ->
    Reply = get_dispatch(Route, State#state.routes, State#state.salt),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

get_dispatch(Route, Routes, Salt) ->
    match_route(Routes, Route, Salt).

match_route([], _, _Salt) -> {{router, '51'}, []};
match_route([H | T], Route, Salt) ->
    #{path := GotPath, id := Id} = Route,
    #{path := ExpPath, requires_login := Login, is_nonced := Nonced, dispatch := MF} = H,
    case unnonce(GotPath, Nonced, Id, Salt) of
        {invalid, Error} ->
            Error;
        Path ->
            case match_path(Path, ExpPath, []) of
                no_match      -> match_route(T, Route, Salt);
                {match, Vals} ->
                case {Id, Login} of
                    {no_identity, login} -> {{router, '60'}, []};
                    _                    -> {MF, Vals}
                end
            end
    end.

unnonce(Path,  no_nonce, _, _) -> Path;
unnonce(_Path, nonce, no_identity, _) -> {invalid, {{router, '60'}, []}};
unnonce(Path,  nonce, Id, Salt) ->
    [Nonce | Rest] = lists:reverse(Path),
    OrigPath = lists:reverse(Rest),
    ExpNonce = make_nonce(Path, Id, Salt),
    case Nonce of
        ExpNonce -> OrigPath;
        _        -> {invalid, {{router, '60 (nonce)'}, []}}
    end.

match_path([], [], Acc)                      -> {match, Acc};
match_path([H | T1], [H | T2], Acc)          -> match_path(T1, T2, Acc);
match_path([H | T1], [{Name, []} | T2], Acc) -> match_path(T1, T2, [{Name, H} | Acc]);
match_path(_, _, _Acc)                       -> no_match.

compile_routes(Routes) -> [compile_route(X) || X <- Routes].

compile_route({Path, Is_nonced, Requires_login, Dispatch}) ->
    #{path           => compile_path(Path),
      is_nonced      => Is_nonced,
      requires_login => Requires_login,
      dispatch       => Dispatch}.

compile_path(Path) ->
    Segs = string:tokens(Path, "/"),
    [make_seg(X) || X <- Segs].

make_seg(":" ++ Seg) -> {Seg, []};
make_seg(X)          -> X.

get_routes() ->
    % a route is a tuple of:
    % * path           requires a string
    % * is_nonced      requires [nonce | no_nonce]
    % * requires_login requires [login | no_login]
    % * dispatch       requires {ModuleName, FunctionName}
    [
        {"/",           no_nonce, no_login, {vega, root}},
        {"/home/:user", no_nonce, login,    {vega, home}},
        {"/nonce",      nonce,    no_login, {vega, root}}
    ].

make_nonce(URL, #{key := K}, Salt) ->
    Nonce = crypto:hash(md5, list_to_binary([Salt, URL, integer_to_list(K)])),
    SafeNonce = binary:encode_hex(Nonce),
    URL ++ binary_to_list(SafeNonce).